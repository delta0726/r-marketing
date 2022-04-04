# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 11 セグメンテーション：クラスタリングと分類（メイン）
# Objective : クラスタリング（潜在クラス分析）
# Page      : P394 - P400
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 潜在クラス分析は求めようとする未知のグループに起因した差異が生じることを仮定する（混合モデルとの類似点）
#   --- poLCAライブラリを用いて分析する（カテゴリカルデータのみを扱う）


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
# 2 潜在クラス分析の実行
# 3 ベイズ情報量基準(BIC)による評価
# 4 クラスタリングの評価
# 5 クラスタリングのプロット
# 6 潜在クラス分析の解の比較


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(cluster)
library(poLCA)
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
# - 数値データをカテゴリカルデータに変換する
#   --- poLCAライブラリはカテゴリカルデータのみを扱う

# データ加工
seg.df  <-
  seg.raw %>%
    select(-Segment)

# データ変換
# --- 数値をカテゴリカル変数に変換
seg.df.cut <-
  seg.df %>%
    mutate(age = factor(ifelse(age < median(age), 1, 2)),
           income = factor(ifelse(income < median(income), 1, 2)),
           kids = factor(ifelse(kids < median(kids), 1, 2))) %>%
    mutate_if(is.character, as.factor)

# データ確認
seg.df.cut %>% print()
seg.df.cut %>% glimpse()

# データサマリー
seg.df.cut %>% summary()


# 2 潜在クラス分析の実行 -------------------------------------------------------------------

# フォーミュラ設定
# --- 1のみを切片に持つフォーミュラ
seg.f <- seg.df.cut %>% with(cbind(age, gender, income, kids, ownHome, subscribe)~1)
seg.f

# モデル構築
set.seed(02807)
seg.LCA3 <- seg.f %>% poLCA(data = seg.df.cut, nclass = 3)
seg.LCA4 <- seg.f %>% poLCA(data = seg.df.cut, nclass = 4)

# データ確認
seg.LCA3 %>% print()
seg.LCA3 %>% summary()
seg.LCA3 %>% listviewer::reactjson(collapsed = TRUE)


# 3 ベイズ情報量基準(BIC)による評価 -----------------------------------------------------------

# ベイズ情報量基準(BIC)の確認
seg.LCA4$bic
seg.LCA3$bic


# 4 クラスタリングの評価 ---------------------------------------------------------------------

# ＜ポイント＞
# - 通常のクラスタ分析と同様に特徴量分布の分離の程度で評価することも可能
#   --- モデルベースクラスタリングと比べると分離が見られる


# 関数定義
# --- 平均集計（数値データのみ）
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


# データ集計
seg.df %>% seg.summ(seg.LCA3$predclass)
seg.df %>% seg.summ(seg.LCA4$predclass)

# カテゴリごとのサンプル数を確認
seg.LCA3$predclass %>% table()
seg.LCA4$predclass %>% table()

# ボックスプロット作成
boxplot(seg.df$income ~ seg.LCA3$predclass, ylab = "Income", xlab = "Cluster")
boxplot(seg.df$age ~ seg.LCA3$predclass, ylab = "Age", xlab = "Cluster")
boxplot(seg.df$kids ~ seg.LCA3$predclass, ylab = "Kids", xlab = "Cluster")


# 5 クラスタリングのプロット ------------------------------------------------------------------

# プロット
seg.df %>%
  clusplot(seg.LCA3$predclass, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "LCA plot (K=3)")

seg.df %>%
  clusplot(seg.LCA4$predclass, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "LCA plot (K=4)")


# 6 潜在クラス分析の解の比較 -------------------------------------------------------------------

# 分割表の作成
# --- 見やすいが主観的判断となる
table(seg.LCA3$predclass, seg.LCA4$predclass)

# 定量的な比較
# --- モデル結果
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)
seg.LCA3$predclass %>% adjustedRandIndex(seg.LCA4$predclass)

# 定量的な比較
# --- ランダムデータ（ベンチマーク）
set.seed(11021)
random.data <- sample(4, length(seg.LCA4$predclass), replace=TRUE)
random.data %>% adjustedRandIndex(seg.LCA4$predclass)


# compare to known segments
table(seg.raw$Segment, seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass)
