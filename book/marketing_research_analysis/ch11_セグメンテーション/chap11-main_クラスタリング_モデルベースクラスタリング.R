# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 11 セグメンテーション：クラスタリングと分類（メイン）
# Objective : クラスタリング（モデルベースクラスタリング）
# Created on: 2022/04/05
# Page      : P389 - P394
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - モデルベースクラスタリングは観測値が異なる統計分布を持つグループで構成されると仮定する
#   --- 観測値の母集団は未知（平均や分散は事前に分からない）
#   --- 観測されたデータを説明するのに最も適した分布の集合を見つける


# ＜クラスタリング手法＞
# - 距離に基づくクラスタリング
#   --- 階層クラスタリング（ツリー構造：hclust）
#   --- 分割型クラスタリング（グループ重心：kmeans）
# - モデルに基づくクラスタリング
#   --- データを正規分布の混合としてモデル化（Mclust）
#   --- カテゴリカル変数の潜在クラスモデル（poLCA）


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 モデルベースクラスタリングの実行
# 3 ベイズ情報量基準(BIC)による評価
# 4 クラスタリングの評価
# 5 クラスタリングのプロット


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(cluster)
library(mclust)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("map", "purrr")


# データ準備
# --- セグメンテーションデータ
seg.raw <- read_csv("data/seg_raw.csv")

# データ確認
seg.raw %>% print()
seg.raw %>% glimpse()


# 1 データ加工 ----------------------------------------------------------------------------

# ＜ポイント＞
# - モデルベースクラスタリングは数値データのみを扱うことができる
#   --- インプットに距離行列はないのでgower距離によるカテゴリカルデータの使用は不可


# データ加工
seg.df  <-
  seg.raw %>%
    select(-Segment)

# データ変換
# --- カテゴリカル変数を数値に変換
# --- 2値カテゴリを0/1に変換するのが最適というわけではない
seg.df.num <-
  seg.df %>%
    mutate(gender = ifelse(seg.df$gender=="Male", 0, 1),
           ownHome = ifelse(seg.df$ownHome=="ownNo", 0, 1),
           subscribe = ifelse(seg.df$subscribe=="subNo", 0, 1))

# データ確認
seg.df.num %>% print()
seg.df.num %>% glimpse()

# データサマリー
seg.df.num %>% summary()


# 2 モデルベースクラスタリングの実行--------------------------------------------------------------

# ＜ポイント＞
# - モデルベースクラスタリングでは、各観測値は異なる統計分布を持つグループから作成されたと仮定する
#   --- 観測されたデータを説明するのに最も適した分布の集合を見つける
#   --- mclust::Mclust()では正規分布の混合から観測値が抽出されたと仮定する(数値データが前提、距離行列は不可)

# 学習1
# --- 初期設定ではクラスタ数はモデルが探索
seg.mc <- seg.df.num %>% Mclust()
seg.mc %>% summary()

# 学習2
# --- クラスタ数を指定することも可能
# --- 学習1のクラスタ1が2つに分離した
seg.mc4 <- seg.df.num %>% Mclust(G = 4)
seg.mc4 %>% summary()


# 3 ベイズ情報量基準(BIC)による評価 ----------------------------------------------------------

# ＜ポイント＞
# - BICが低いほどモデルとして優れている
# - BICはマイナスとなることもある（0に近いほど優れている）


# ベイズ情報量基準(BIC)
BIC(seg.mc, seg.mc4)

# 探索的なクラスタ数の分析
seg.df.num %>% mclustBIC()


# 4 クラスタリングの評価 ----------------------------------------------------------

# ＜ポイント＞
# - 通常のクラスタ分析と同様に特徴量分布の分離の程度で評価することも可能
#   --- k-meansと比べると分離度合いが小さい


# 関数定義
# --- 平均集計（数値データのみ）
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

# データ集計
seg.df %>% seg.summ(seg.mc$class)

# ボックスプロット作成
boxplot(seg.df$income ~ seg.mc$class, ylab = "Income", xlab = "Cluster")
boxplot(seg.df$age ~ seg.mc$class, ylab = "Age", xlab = "Cluster")
boxplot(seg.df$kids ~ seg.mc$class, ylab = "Kids", xlab = "Cluster")


# 5 クラスタリングのプロット --------------------------------------------------------------

# ＜ポイント＞
# - グループが重なり合っている


# プロット作成
seg.df %>%
  clusplot(seg.mc$class, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "Model-based cluster plot")
