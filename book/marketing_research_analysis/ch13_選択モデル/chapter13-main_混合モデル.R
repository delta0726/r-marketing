# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 13 選択モデル
# Theme     : 選択モデルへの消費者の異質性の追加（混合モデル）
# Created on: 2021/04/10
# Page      : P480 - P486
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜ポイント＞
# - 各回答者が個別の係数を持つことを可能とする階層モデルを用いて推定する
#   --- 個人の嗜好の違いをモデルに反映させる


# ＜目次＞
# 0 準備
# 1 モデルデータの作成
# 2 混合モデル構築
# 3 混合モデルの更新
# 4 シェア予測


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(mlogit)
library(dfidx)
library(conflicted)

conflict_prefer("select", "dplyr", quiet = TRUE)


# データロード
cbc.df <- read_csv("data/cbc_df.csv")

# データ確認
# --- レコードが回答となっているアンケートデータ
cbc.df %>% print()
cbc.df %>% glimpse()


# 1 モデルデータの作成 --------------------------------------------------------------------

# ＜ポイント＞
# - mlogit()は選択モデルとして最も基本的でよく使用される多項ロジットモデル(条件付ロジットモデル)を推定する


# データ変換
cbc.mlogit <-
  cbc.df %>%
    mutate(eng = factor(eng, levels = c("gas", "hyb", "elec")),
           carpool = factor(carpool, levels = c("yes", "no")),
           chid = rep(1:(nrow(.) / 3), each = 3)) %>%
    mutate_all(as.factor) %>%
    dfidx(choice = "choice", idx = list(c("chid", "resp.id"), "alt"))

# クラス確認
cbc.mlogit %>% class()
cbc.mlogit %>% print()


# 2 混合モデル構築 --------------------------------------------------------------

# ＜ポイント＞
# - ランダム係数を持った多項ロジットモデルを定義するため、どの係数が顧客ごとに異なるかを指定する
#   --- 係数ごとに分布を示す文字列を与えたベクトルを作る

# ＜分布の記号＞
# - n ：正規分布
# - l ：対数分布
# - t ：切断正規分布
# - u ：一様分布


# ベースモデル
m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit)

# パラメータ作成
# --- 係数ごとの分布を記号で定義
# --- 今回は全て正規分布を想定する
m1.rpar <-
  rep("n", length = length(m1$coef)) %>%
    set_names(names(m1$coef))

# 混合モデルの構築
# --- rpar引数に分布のパラメータを渡す（rpar：random parameters）
# --- panel引数をTRUEにすることで混合モデル(パネルデータモデル)を選択する
# --- correlation引数をFALSEにすることでパラメータ間を相関させない
m1.hier <-
  mlogit(choice ~ 0 + seat + eng + cargo + price,
         data = cbc.mlogit, panel = TRUE, rpar = m1.rpar, correlation = FALSE)

# 確認
m1.hier %>% summary()

# 標準偏差の推定
m1.hier %>% stdev()


# 3 混合モデルの更新 ----------------------------------------------------------

# ＜ポイント＞
# - モデルの更新はupdate()を用いて行うことができる
# - 今回はパラメータ間に相関があることを想定する（correlation = TRUE）


# モデル更新
# --- correlation引数をTRUEとする
m2.hier <- m1.hier %>% update(correlation = TRUE)

# 確認
m2.hier %>% summary()

# 相関係数行列
m2.hier %>% cov.mlogit() %>% cov2cor()


# 4 シェア予測 ---------------------------------------------------------------

# Simulating shares
predict.hier.mnl <- function(model, data, nresp = 1000) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[, -1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- MASS::mvrnorm(n = nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow = nresp, ncol = nrow(data))
  for (i in 1:nresp) {
    utility <- data.model %*% draws[i,]
    share = exp(utility) / sum(exp(utility))
    shares[i,] <- share
  }
  cbind(colMeans(shares), data)
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

# シェア推定
m2.hier %>% predict.hier.mnl(data = new.data)
