# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 7 結果に対する要因を特定する（データ作成）
# Created by: Owner
# Created on: 2021/03/08
# Page      : P194 - P197
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 遊園地データの作成


# ＜目次＞
# 0 準備
# 1 データ作成


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)


# 1 データ作成 ----------------------------------------------------------------------------

# ＜ポイント＞
# - インポートしたsat.dfの作成プロセス
#   --- データがどのような構造で作られたかを知ることができる


# 乱数シード
set.seed(08226)

# サーベイ数
nresp <- 500

# ハロー効果
# --- 顧客が潜在的に持っている効果
# --- 全ての効果に影響を与える
halo <- rnorm(n = nresp, mean = 0, sd = 5) %>% floor()

# 各項目の満足度
rides <- floor(halo + rnorm(n = nresp, mean = 80, sd = 3) + 1)
games <- floor(halo + rnorm(n = nresp, mean = 70, sd = 7) + 5)
wait  <- floor(halo + rnorm(n = nresp, mean = 65, sd = 10) + 9)
clean <- floor(halo + rnorm(n = nresp, mean = 85, sd = 2) + 1)

# 相関係数
# --- ハロー効果の相関
# --- 正規乱数を用いている部分は相関ゼロ
cor(rides, games)


# distance列
# --- 対数正規分布
distance <- nresp %>% rlnorm(meanlog = 3, sdlog = 1)

# num.child列
num.child <-
  sample(x = 0:5, size = nresp, replace = TRUE,
         prob = c(0.3, 0.15, 0.25, 0.15, 0.1, 0.05))

# weekend列
# --- 50%の確率でYes/No
weekend <-
  c("yes", "no") %>%
    sample(size = nresp, replace = TRUE, prob = c(0.5, 0.5)) %>%
    as.factor()

# overall列
# --- 全体的満足度を示す
# --- 複数列の線形結合とランダムノイズで表現（線形モデル）
overall <-
  floor(halo + 0.5 * rides + 0.1 * games + 0.3 * wait + 0.2 * clean +
          0.03 * distance + 5 * (num.child == 0) + 0.3 * wait*(num.child>0) +
          rnorm(n = nresp, mean = 0, sd = 7) - 54)

# データ完成
sat_df <- tibble(weekend, num.child, distance, rides, games, wait, clean, overall)


# データ確認
sat_df %>% print()
sat_df %>% glimpse()
