# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 8 データの複雑さを低減する（多次元尺度構成法）
# Created on: 2022/04/01
# Page      : P265 - P268
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 多次元尺度構成法では類似度を用いた次元圧縮をおこなう
#   --- 類似度を最も良く保持する低次元空間に射影する
#   --- カテゴリカルデータに対しても適用することが可能（gower距離を使用）


# ＜目次＞
# 0 準備
# 1 数値データのMDS
# 2 カテゴリカルデータのMDS


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(conflicted)
library(cluster)
library(MASS)
library(listviewer)


# コンフリクト解消
conflict_prefer("select", "dplyr")

# データ準備
brand.ratings <- read_csv("data/brand.rating.csv")

# データ変換
# --- Zスコア変換
brand.sc <-
  brand.ratings %>%
    mutate_if(is.numeric, function(x) (x - mean(x)) / sd(x)) %>%
    as_tibble()

# ブランドごとの評価
# --- グループ別の平均スコア
brand.mean <-
  brand.sc %>%
    group_by(brand) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup()


# 1 数値データのMDS ---------------------------------------------------------

# ＜ポイント＞
# - 多次元尺度法は数値データから距離行列を計算してから適用する
# - PCAで作成した知覚マップと同じような結果を得ることができる


# 多次元尺度法の計算
# --- 距離を測定してからMDSを算出する
brand.mds <-
  brand.mean %>%
    dist() %>%
    cmdscale()

# 確認
brand.mds %>% print()
brand.mds %>% class()

# プロット
brand.mds %>% plot(type = "n")
brand.mds %>% text(rownames(brand.mds), cex = 2)


# 2 カテゴリカルデータのMDS -------------------------------------------------------

# ＜ポイント＞
# - 多次元尺度法は距離行列を使用するのでカテゴリカルデータを含むデータに対する距離行列を定義
#   --- cluster::daisy()でgower距離を用いたクラスタリングを行う


# カテゴリカルデータ
# --- カテゴリ別の集計値からランキングデータを作成
brand.rank <-
  brand.mean %>%
    lapply(function(x) ordered(rank(x))) %>%
    data.frame()

# データ確認
brand.rank %>% print()
brand.rank %>% glimpse()

# 距離行列の作成
# --- カテゴリカルデータを含むのでgower距離を使用
brand.dist.r <- brand.rank %>% daisy(metric = "gower")
brand.dist.r

# 非計量多次元尺度の算出
brand.mds.r <- brand.dist.r %>% isoMDS()

# 確認
brand.mds.r %>% print()
brand.mds.r %>% reactjson(collapsed = TRUE)

# プロット作成
brand.mds.r$points %>% plot(type = "n")
brand.mds.r$points %>% text(levels(brand.sc$brand), cex = 2)

