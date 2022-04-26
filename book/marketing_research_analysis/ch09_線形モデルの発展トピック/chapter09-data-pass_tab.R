# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（データ作成）
# Objective : ロジスティック回帰
# Created on: 2021/02/22
# Page      : P284 - P289
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - pass.dfデータの作成


# ＜データ内容＞
# -


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 データ確認


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(vcdExtra)


# 1 データ作成 ---------------------------------------------------------------------------

# テーブル作成
# --- 売上データ
pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list(Channel=c("Mail", "Park", "Email"),
                           Promo=c("Bundle", "NoBundle"),
                           Pass=c("YesPass", "NoPass") )
# データ確認
pass.tab %>% print()

# テーブルをデータフレームに変換
pass.df <-
  pass.tab %>%
    expand.dft() %>%
    mutate(Promo = factor(Promo, levels = c("NoBundle", "Bundle")))


# 2 データ確認 ---------------------------------------------------------------------------

# データ確認
pass.df %>% glimpse()

# 混合行列
table(pass.df$Pass, pass.df$Promo)