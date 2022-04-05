# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 11 セグメンテーション：クラスタリングと分類（メイン）
# Objective : 分類（ナイーブベイズ）
# Created on: 2021/04/06
# Page      : P400 - P406
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - クラス分類の予測を行いたい場合は分類問題の機械学習を使用する
# - ナイーブベイズは単純な確率モデルを基にベイズの識別規則に沿って予測を行う
#   --- 機械学習の中では単純なアルゴリズム


# ＜目次＞
# 0 準備
# 1 データ分割
# 2 学習＆予測
# 3 モデル評価
# 4 カテゴリ評価


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(yardstick)
library(cluster)
library(mclust)
library(e1071)
library(gplots)
library(RColorBrewer)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("map", "purrr", quiet = TRUE)

# データ準備
# --- セグメンテーションデータ
seg.raw <-
  read_csv("data/seg_raw.csv") %>%
    mutate_if(is.character, as.factor)


# 1 データ分割 ----------------------------------------------------------------------------

# ＜ポイント＞
# - 学習用と評価用でデータを分割する


# データ分割
set.seed(04625)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw) * train.prop)
seg.df.train <- seg.raw %>% slice(train.cases)
seg.df.test  <- seg.raw %>% slice(-train.cases)

# データ確認
seg.df.train %>% dim()
seg.df.test %>% dim()

# ラベル確認
# --- マルチクラスの分類問題
seg.df.train$Segment %>% table()


# 2 学習＆予測 -----------------------------------------------------------------

# モデル構築
seg.nb <- naiveBayes(Segment ~ ., data = seg.df.train)

# 確認
seg.nb %>% summary()

# 予測
seg.nb.class <- seg.nb %>% predict(seg.df.test)

# テーブル集計
# --- 割合ベース
seg.nb.class %>% table() %>% prop.table()

# プロット作成
seg.df.test[, -7] %>%
  clusplot(seg.nb.class, color = TRUE, shade = TRUE,
         labels = 4, lines = 0, main = "Naive Bayes classification, holdout data")


# 3 モデル評価 ------------------------------------------------------------------

# Accuracy
mean(seg.df.test$Segment == seg.nb.class)

# ランダムインデックス
seg.nb.class %>% adjustedRandIndex(seg.df.test$Segment)

# 混合行列
# --- マルチクラス
table(seg.nb.class, seg.df.test$Segment)


# 4 カテゴリ評価 ------------------------------------------------------------------

# 関数定義
# --- 特徴量のグループ平均値の算出
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

# カテゴリ集計
# --- Segment(実際の値)
# --- Segment(予測値)
seg.df.test %>% seg.summ(seg.nb.class)
seg.df.test %>% seg.summ(seg.df.test$Segment)

# クラス確率の推定
seg.nb %>% predict(seg.df.test, type = "raw") %>% as_tibble()
