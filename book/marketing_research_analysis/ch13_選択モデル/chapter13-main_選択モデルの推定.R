# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 13 選択モデル
# Theme     : 選択モデルの推定
# Created on: 2022/07/27
# Page      : P465 - P480
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 選択モデルは製品の属性が顧客の商品選択にどのような影響を与えるかを理解するために使われる
# - 最も一般的なのは多項ロジットモデルで頻度統計では{mlogit}が用いられる


# ＜目次＞
# 0 準備
# 1 EDAプロセス
# 2 モデルデータの作成
# 3 モデル構築
# 4 モデル解釈
# 5 数値データによるモデル構築
# 6 モデル解釈：支払意思額
# 7 モデル解釈：選択肢シェアシミュレータ
# 8 モデル解釈：感度プロット


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(mlogit)
library(dfidx)
library(broom)
library(conflicted)

conflict_prefer("select", "dplyr", quiet = TRUE)


# データロード
cbc.df <- read_csv("data/cbc_df.csv")

# データ加工
# --- ファクターに変換
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

# 参考：テーブル集計
cbc.df %>% select(choice, price) %>% table()
cbc.df %>% select(choice, cargo) %>% table()


# 2 モデルデータの作成 --------------------------------------------------------------------

# ＜ポイント＞
# - 全ての列がファクターのデータフレームdfidxオブジェクトにを作成する


# データ変換
# --- {mlogit}のバージョンが1.1以上であればdfidxを使う
cbc.mlogit <-
  cbc.df %>%
    mutate(chid = rep(1:(nrow(.) / 3), each = 3)) %>%
    mutate_all(as.factor) %>%
    dfidx(choice = "choice", idx = list(c("chid", "resp.id"), "alt"))

# クラス確認
cbc.mlogit %>% class()

# データ確認
cbc.mlogit %>% print()
cbc.mlogit


# 3 モデル構築 --------------------------------------------------------------------

# ＜ポイント＞
# - 多項ロジットモデルは選択モデルで最も基本的かつ使用頻度が高い（{mlogit}で提供されている）
#   --- 大規模モデルで高速化が必要な場合は{mnlogit}


# 学習
# --- モデル1：定数項なしモデル
# --- モデル1：定数項ありモデル
m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit)
m2 <- mlogit(choice ~ seat + cargo + eng + price, data = cbc.mlogit)

# 確認
m1 %>% summary()
m2 %>% summary()


# 4 モデル解釈 --------------------------------------------------------------------

# 回帰係数
# - カテゴリカルデータの回帰なので回帰係数はベースとの差分を示す
# - seat7やseat8がマイナスなのはseat6が相対的に好まれていることを意味する
# - std.errorは回帰係数がゼロと異なるかどうかを示す
# - 定数項を含めるとmlogitは異なる選択肢に対する嗜好を表すパラメータを追加する
m1 %>% tidy()
m2 %>% tidy()

# 尤度比検定
lrtest(m1, m2)


# 5 数値データによるモデル構築 --------------------------------------------------------

# データ変換
cbc.mlogit2 <-
  cbc.mlogit %>%
    mutate(price = as.numeric(as.character(price)))

# モデル構築
m3 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit2)

# 確認
m3 %>% summary()
m3 %>% tidy()

# 尤度比検定
lrtest(m1, m3)


# 6 モデル解釈：支払意思額 -----------------------------------------------------------

# 回帰係数の確認
coef(m3)

# 支払意思額
# --- 特定の係数を価格係数で割ることで平均支払意思額を求める
# --- 価格は1000ドル単位で記録されてているため1000で割る
coef(m3)["cargo3ft"] / (-coef(m3)["price"] / 1000)


# 7 モデル解釈：選択肢シェアシミュレータ -----------------------------------------------

# 関数定義
# --- 多項ロジットモデルからシェアを予測するための関数
predict.mnl <- function(model = m3, data = new.data) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[ , -1]
  utility <- data.model %*% model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share, data)
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

new.data.num <-
  new.data %>%
    mutate(price = as.numeric(as.character(price)))

m3 %>% predict.mnl(new.data.num)
m1 %>% predict.mnl(new.data)


# 8 モデル解釈：感度プロット ------------------------------------------------------

# 関数定義
# --- シェア感度チャートのデータを作成するための関数
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1, 1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1, 1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
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
base.data <-
  attrib %>%
    expand.grid() %>%
    slice(8)

competitor.data <-
  attrib %>%
    expand.grid() %>%
    slice(1, 3, 41, 49, 26)

# 感応度データの作成
tradeoff <- m1 %>% sensitivity.mnl(attrib, base.data, competitor.data)

# プロット作成
tradeoff$increase %>%
  barplot(horiz = FALSE, names.arg = tradeoff$level,
          ylab="Change in Share for Baseline Product")


# 9 同一代替案が含まれる場合のシェア予測 --------------------------------------------------------------------

# データ作成
# --- シミュレータ用のデータサンプル

new.data.2 <-
  attrib %>%
    expand.grid() %>%
    slice(8, 8, 1, 3, 41, 49, 26)

m1 %>% predict.mnl(new.data.2)


# コンジョイント分析のサンプルサイズ
small.cbc <-
  cbc.df %>%
    slice(1:(25 * 15 * 3)) %>%
    mutate(chid = rep(1:(nrow(.) / 3), each = 3)) %>%
    mutate_all(as.factor) %>%
    dfidx(choice = "choice", idx = list(c("chid", "resp.id"), "alt"))

m4 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = small.cbc)
summary(m4)  # larger standard errors

cbind(predict.mnl(m4, new.data), predict.mnl(m1, new.data))


# ------- Mixed logit model ------------------------------
# Model estimation
m1.rpar <- rep("n", length=length(m1$coef))
names(m1.rpar) <- names(m1$coef)
m1.rpar
m1.hier <- mlogit(choice ~ 0 + seat + eng + cargo + price, 
                  data = cbc.mlogit, 
                  panel=TRUE, rpar = m1.rpar, correlation = FALSE)
summary(m1.hier)
stdev(m1.hier)

m2.hier <- update(m1.hier, correlation = TRUE)
summary(m2.hier)
cov2cor(cov.mlogit(m2.hier))


# Simulating shares
predict.hier.mnl <- function(model, data, nresp=1000) {
  # Function to predict shares from a hierarchical multinomial logit model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares.  Same format at the data used to estimate model. 
  # Note that this code assumes all model parameters are random
  library(MASS)
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[ , -1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i, ]
    share = exp(utility)/sum(exp(utility))
    shares[i, ] <- share
  }
  cbind(colMeans(shares), data)
}

predict.hier.mnl(m2.hier, data=new.data)

