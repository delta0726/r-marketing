# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（データ作成）
# Objective : 階層線形モデル / 階層ベイズ
# Created by: Owner
# Created on: 2021/02/22
# Page      : P298 - P299
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - conjoint.dfデータの作成


# ＜データ内容＞
# - 200人に対してアンケートで16種類のローラーコースターのプロファイルから評価スコアを決めてもらうアンケート
#   --- 3200レコード


# ＜目次＞
# 0 準備
# 1 ベースプロファイルの作成
# 2 個人ごとのウエイト作成
# 3 データ作成
# 4 データ確認


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(MASS)


# 1 ベースプロファイルの作成 ---------------------------------------------------------------------

# 乱数シード
set.seed(12814)

# パラメータ設定
resp.id <- 1:200
nques <- 16

# 系列作成
# --- ベースプロファイル
speed  <- c("40", "50", "60", "70") %>% as.factor() %>% sample(size = nques, replace = TRUE)
height <- c("200", "300", "400") %>% as.factor() %>% sample(size = nques, replace = TRUE)
const  <- c("Wood", "Steel") %>% as.factor() %>% sample(size =  nques, replace = TRUE)
theme  <- c("Dragon", "Eagle") %>% as.factor() %>% sample(size = nques, replace = TRUE)

# データフレーム作成
# --- ベースプロファイルの作成
profiles.df <- data.frame(speed, height, const, theme)
profiles.model <- model.matrix(~ speed + height + const + theme, data = profiles.df)


# 2 個人ごとのウエイト作成 -----------------------------------------------------------------------

# ウエイトベクトル
weights <-
  resp.id %>%
    length() %>%
    mvrnorm(mu = c(-3, 0.5, 1, 3, 2, 1, 0, -0.5),
            Sigma = diag(c(0.2, 0.1, 0.1, 0.1, 0.2, 0.3, 1, 1)))


# 3 データ作成 -----------------------------------------------------------------------

# データ格納オブジェクト
conjoint.df <- NULL

i <- 1
for (i in seq_along(resp.id)) {
  utility <- profiles.model %*% weights[i, ] + rnorm(nques)
  rating <- utility %>% cut(10) %>% as.numeric()
  conjoint.resp <- cbind(resp.id = rep(i, nques), rating, profiles.df)
  conjoint.df <- conjoint.df %>% rbind(conjoint.resp)
}  


# 4 データ確認 -------------------------------------------------------------------------------

# 完成
conjoint.df %>% print()

# 確認
conjoint.df %>% print()
conjoint.df %>% glimpse()
