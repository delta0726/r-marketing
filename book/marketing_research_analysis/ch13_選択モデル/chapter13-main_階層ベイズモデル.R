# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 13 選択モデル
# Theme     : 選択モデルへの消費者の異質性の追加（階層ベイズモデル）
# Created on: 2021/04/10
# Page      : P487 - P499
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 EDAプロセス
# 2 モデルデータの作成
# 3 モデル構築
# 4 データ確認
# 5 シェア予測


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ChoiceModelR)
library(conflicted)

conflict_prefer("select", "dplyr", quiet = TRUE)


# データロード
cbc.df <- read_csv("data/cbc_df.csv")

# データ加工
cbc.df <-
  cbc.df %>%
    mutate(eng = factor(eng, levels = c("gas", "hyb", "elec")),
           carpool = factor(carpool, levels = c("yes", "no")))

# データ確認
# --- レコードが回答となっているアンケートデータ
cbc.df %>% print()
cbc.df %>% glimpse()


# 1 EDAプロセス -------------------------------------------------------------------------

# ＜ポイント＞
# - カテゴリカルデータを扱うときはデータごとのカテゴリ数の分布を確認しておく


# カテゴリごとの出現回数
cbc.df %>%
  select(carpool, seat, cargo, eng, price, choice) %>%
  map(table)

# 多次元分割表
# --- choiceが1の場合の内訳を出力
xtabs(choice ~ price, data = cbc.df)
xtabs(choice ~ cargo, data = cbc.df)

# ピボットテーブル
# --- 検証用
#cbc.df %>% rpivotTable::rpivotTable()


# 2 モデルデータの作成 --------------------------------------------------------

# データ変換
# --- alt1にchoice1のaltの値を入れる
cbc.df <- cbc.df %>% as.data.frame()
choice <- rep(0, nrow(cbc.df))
choice[cbc.df[, "alt"] == 1] <- cbc.df[cbc.df[, "choice"] == 1, "alt"]
head(choice, 20)

# モデルデータ
# --- 切片項は削除
cbc.coded <- model.matrix(~seat + eng + cargo + price, data = cbc.df)
cbc.coded <- cbc.coded[, -1] # remove the intercept

# 階層データ
choicemodelr.data <- cbind(cbc.df[, 1:3], cbc.coded, choice)
head(choicemodelr.data)


carpool <- cbc.df$carpool[cbc.df$ques == 1 & cbc.df$alt == 1] == "yes"
carpool <- as.numeric(carpool)
choicemodelr.demos <- as.matrix(carpool, nrow = length(carpool))
str(choicemodelr.demos)


# 3 モデル構築 -----------------------------------------------------------

# モデル定義
hb.post <-
  choicemodelr(data = choicemodelr.data, xcoding = rep(1, 5),
               demos = choicemodelr.demos,
               mcmc = list(R = 20000, use = 10000),
               options = list(save = TRUE))

# 確認
hb.post %>% names()
hb.post %>% summary()
hb.post %>% listviewer::reactjson()


# 4 データ確認 -------------------------------------------------------------

# Model parameters
hb.post$compdraw[[567]]$mu
hb.post$deltadraw[567,]

hb.post$compdraw[[567]]$rooti
crossprod(hb.post$compdraw[[567]]$rooti)

# Individual-level betas
head(hb.post$betadraw[, , 567])
str(hb.post$betadraw)

beta.post.mean <- apply(hb.post$betadraw, 1:2, mean)
head(beta.post.mean)

beta.post.q05 <- apply(hb.post$betadraw, 1:2, quantile, probs = 0.05)
beta.post.q95 <- apply(hb.post$betadraw, 1:2, quantile, probs = 0.95)
rbind(q05 = beta.post.q05[1,], mean = beta.post.mean[1,], q95 = beta.post.q95[1,])


# 5 シェア予測 ---------------------------------------------------------------

# Prediction using individual-level draws
predict.hb.mnl <- function(betadraws = hb.post$betadraw, data = new.data) {
  # Function to  predict shares from a hierarchical multinomial logit model
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to
  #       predict shares.  Same format at the data used to estimate model.
  data.model <- model.matrix(~ seat + eng + cargo + price, data = data)
  data.model <- data.model[ , -1]
  nresp <- dim(betadraws)[1]
  ndraws <- dim(hb.post$betadraw)[3]
  shares <- array(dim=c(nresp, nrow(data), ndraws))
  for (d in 1:ndraws) {
    for (i in 1:nresp) {
      utility <- data.model %*% betadraws[i, , d]
      shares[i, , d] = exp(utility)/sum(exp(utility))
    }
  }
  shares.agg <- apply(shares, 2:3, mean)
  cbind(share=apply(shares.agg, 1, mean),
        pct=t(apply(shares.agg, 1, quantile, probs=c(0.05, 0.95))),
        data)
}


# カテゴリリスト
# --- データ作成で使用したもの
attrib <-
  list(seat  = c("6", "7", "8"),
       cargo = c("2ft", "3ft"),
       eng   = c("gas", "hyb", "elec"),
       price = c("30", "35", "40"))

# データ作成
# --- シミュレータ用のデータサンプル
new.data <-
  attrib %>%
    expand.grid() %>%
    slice(8, 1, 3, 41, 49, 26)

new.data %>% head()
hb.post$betadraw %>% predict.hb.mnl(new.data)
