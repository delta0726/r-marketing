# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 11 セグメンテーション：クラスタリングと分類（メイン）
# Objective : クラスタリング（潜在クラス分析）
# Page      : P394 - P400
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 潜在クラス分析は数値データとカテゴリカルデータをより柔軟に統合して分析できる
#   --- 従来の手法によるセグメンテーションと比べ、より煩雑で膨大なデータを機械的に分類することができる手法
#   --- 統計的基準で最適なセグメント数が決まる
#   --- 回答者毎の各セグメントへの所属確率が算出される


# ＜参考＞
# 潜在クラス分析とクラスター分析の違いとは
# https://jmro.co.jp/amr00029/


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
# 5 プロットによるクラスタリング結果の解釈
# 6 クラスタリングのプロット
# 7 潜在クラス分析の解の比較


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

# 参考：with文
# --- 以下の2つは同義
iris %>% with(Sepal.Width + Petal.Width)
iris$Sepal.Width + iris$Petal.Width

# フォーミュラ設定
# --- 1のみを切片に持つフォーミュラ
seg.f <- seg.df.cut %>% with(cbind(age, gender, income, kids, ownHome, subscribe)~1)

# 確認
seg.f %>% print()
seg.f %>% class()

# モデル構築
set.seed(02807)
seg.LCA3 <- seg.f %>% poLCA(data = seg.df.cut, nclass = 3)
seg.LCA4 <- seg.f %>% poLCA(data = seg.df.cut, nclass = 4)

# データ確認
seg.LCA3 %>% print()
seg.LCA3 %>% summary()
seg.LCA3 %>% listviewer::reactjson(collapsed = TRUE)


# 3 ベイズ情報量基準(BIC)による評価 -----------------------------------------------------------

# ＜ポイント＞
# - 潜在クラス分析の特徴としてBICによるモデル評価が行われる点にある
# - LCA3(2298.767) LCA4(2330.043) とLCA3の方が32低くフィッティングが良くなっている


# BICの確認
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
seg.df.cut %>% seg.summ(seg.LCA3$predclass)
seg.df.cut %>% seg.summ(seg.LCA4$predclass)

# カテゴリごとのサンプル数
seg.LCA3$predclass %>% table()
seg.LCA4$predclass %>% table()


# 5 プロットによるクラスタリング結果の解釈 --------------------------------------------------

# ボックスプロット作成
boxplot(seg.df$income ~ seg.LCA3$predclass, ylab = "Income", xlab = "Cluster")
boxplot(seg.df$age ~ seg.LCA3$predclass, ylab = "Age", xlab = "Cluster")
boxplot(seg.df$kids ~ seg.LCA3$predclass, ylab = "Kids", xlab = "Cluster")


# 6 クラスタリングのプロット ------------------------------------------------------------------

# ＜ポイント＞
# - LCA4の場合はPC1とPC2の2次元でみると要素が混在するクラスタが中央に確認される


# プロット
seg.df %>%
  clusplot(seg.LCA3$predclass, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "LCA plot (K=3)")

seg.df %>%
  clusplot(seg.LCA4$predclass, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "LCA plot (K=4)")


# 7 潜在クラス分析の解の比較 -------------------------------------------------------------------

# ＜ポイント＞
# - 2つのクラスタリング結果を比較する場合は適切なツールを用いて行う必要がある
# - mclust::mapClass()
#   --- 2つのクラスタリングの結果の全ての対応関係を調べて一致度が最大のものを抽出
# - mclust::adjustedRandIndex()
#   --- mapClassと同様の処理を観測値がランダムに割り当てられた場合の確率を差し引いた一致度を計算（調整ランド指数）


# クラス分類
seg.LCA3$predclass %>% table()
seg.LCA4$predclass %>% table()

# 分割表による一致確認
# --- LCA3-3 & LCA4-1 / LCA3-2 & LCA4-2 あたりがペアか？
# --- 見やすいが主観的判断となる
seg.LCA3$predclass %>% table(seg.LCA4$predclass)

# クラスタのマップ比較
# --- 2つのクラスタベクトルの対応関係をリストで表現
comp1 <- seg.LCA3$predclass %>% mapClass(seg.LCA4$predclass)
comp1 %>% listviewer::reactjson()

# クラスタの一致度
# --- 調整ランド指数
seg.LCA3$predclass %>% adjustedRandIndex(seg.LCA4$predclass)

# ランダムデータとの比較
# --- 調整ランド指数のベンチマーク
# --- 調整ランド指数は概ねゼロ
set.seed(11021)
random.data <- sample(4, length(seg.LCA4$predclass), replace=TRUE)
random.data %>% adjustedRandIndex(seg.LCA4$predclass)

# 正解ラベルとの比較
seg.raw$Segment %>% table(seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass)
