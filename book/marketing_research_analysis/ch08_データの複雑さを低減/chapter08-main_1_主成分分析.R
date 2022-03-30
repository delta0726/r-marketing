# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 8 データの複雑さを低減する（メイン1 主成分分析）
# Created on: 2021/02/22
# Page      : P237 - P254
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 次元圧縮で情報集約することで複雑なデータを解釈しやすくする
# - ポジショニングやブランド戦略を定量的に扱うための基礎概念の習得を目指す（応用は10章以降）


# ＜アルゴリズム＞
# 1 主成分分析：
#   - 互いに相関が無く、分散が最大となるような主成分を順次構成していく
#   - 元の変数による次元解釈が困難
#
# 2 因子分析
#   - 主成分分析と同様に少ない次元で分散を捉えるが、元の変数によって次元解釈が可能
#
# 3 多次元尺度構成法
#   - 観測値の類似度を2次元プロットのような低次元空間に射影


# ＜目次＞
# 0 準備
# 1 データ加工と相関分析
# 2 ヒートマップによるブランド評価
# 3 主成分分析の基礎
# 4 ブランド評価とPCA
# 5 PCAと知覚マップ


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(corrplot)
library(gplots)
library(RColorBrewer)
library(psych)
library(nFactors)
library(conflicted)
#library(GPArotation)
#library(gplots)
#library(RColorBrewer)
#library(semPlot)
#library(cluster)
#library(MASS)


# コンフリクト解消
conflict_prefer("select", "dplyr")


# データ準備
brand.ratings <- read_csv("data/brand.rating.csv")

# データ確認
# --- 各列が1-10のスコアになっている
# --- 10: 最もそう思う場合
# ---  1: そう思わない場合
brand.ratings %>% as_tibble()
brand.ratings %>% glimpse()

# データサマリー
# --- 各項目を1-10で表している
brand.ratings %>% describe()
brand.ratings %>% map(table)


# 1 データ加工と相関分析 ---------------------------------------------------------------------

# ＜ポイント＞
# - 評価項目ごとのスコアの相関関係を確認する
#   --- この時点ではブランドごとの評価の違いは見ていない


# データ変換
# --- Zスコア変換
brand.sc <-
  brand.ratings %>%
    mutate_if(is.numeric, function(x) (x - mean(x)) / sd(x)) %>%
    as_tibble()

# 確認
brand.sc %>% print()
brand.sc %>% describe()

# 相関プロット
# --- 階層クラスタリングで並び替え
brand.sc %>%
  select(1:9) %>%
  cor() %>%
  corrplot(order = "hclust", addCoef.col = 'black', tl.pos = 'd')


# 2 ヒートマップによるブランド評価 ------------------------------------------------------------

# ＜ポイント＞
# - 評価項目ごとのブランドの評価がどのようになっているかを確認する
#   --- スコアの平均値で評価する
#   --- 視覚的に理解するために平均値をヒートマップで表現する


# ブランドごとの評価
# --- グループ別の平均スコア
brand.mean <-
  brand.sc %>%
    group_by(brand) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup()

# 確認
brand.mean %>% print()

# ヒートマップ
# --- ブランドごとの評価項目
# --- 同じような評価を受けているカテゴリを視覚的に理解
brand.mean %>%
  select(is.numeric) %>%
  as.matrix() %>%
  set_rownames(brand.mean$brand) %>%
  heatmap.2(col = brewer.pal(9, "GnBu"), trace = "none", key = FALSE,
            dend = "none", main = "\n\n\n\n\nBrand attributes")


# 3 主成分分析の基礎 ----------------------------------------------------------------------

# ＜ポイント＞
# - 主成分分析は元のデータセットを分散最大化の観点でPC1をとり、以降は前のPCに直交して成分を抽出する
#   --- 主成分間の相関はゼロとなる
#   --- 主成分スコアと固有値に分解される
#   --- 主成分負荷量(因子負荷量)とは主成分得点と観測変数との相関係数（-1から1の値を取る）

# ＜参考資料＞
# 主成分分析 - 統計科学研究所
# https://statistics.co.jp/reference/software_R/statR_9_principal.pdf


# データ準備
my.vars <- read_csv("data/chap8_pca_samle.csv")

# 散布図
# --- jitter()で元データにノイズを加える（プロットの重なりを回避）
my.vars %>% as.matrix() %>% jitter() %>% plot(yvar ~ xvar, data = .)

# 相関係数
my.vars %>% cor()

# 主成分分析
my.pca <- my.vars %>% prcomp()

# 確認
my.pca %>% summary()
my.pca %>% listviewer::reactjson(collapsed = TRUE)

# 相関係数
# --- 主成分の相関はゼロ
my.pca$x %>% cor() %>% round(5)

# バイプロット
# --- PC1とPC2で関係性を表現
# --- 主成分は無相関化されているため２次元の表現に適している
my.pca %>% biplot(scale = TRUE)


# 4 ブランド評価とPCA ----------------------------------------------------------------------

# ＜ポイント＞
# - 主成分分析を適用することで元の特徴量と別の観点からデータセットの特性を知ることができる
# - ここでは、元のブランドデータにPCAを適用する
#   --- ブランドごとの平均化したデータに対するPCAではない点に注意
#   --- 主成分のイメージは掴めるが、ブランドとの関係性は記述できない


# PCAの実行
# --- 元のブランドデータ
brand.pc <- brand.sc %>% select_if(is.numeric) %>% prcomp()

# 確認
brand.pc %>% print()
brand.pc %>% listviewer::reactjson(collapsed = TRUE)

# スクリープロット
# --- PCごとの主成分寄与度を示す
brand.pc %>% plot(type = "l")

# バイプロット
# --- PC1とPC2で元系列を射影
# --- PC1とPC2で観測値をマッピング
brand.pc %>% biplot()


# 5 PCAと知覚マップ ----------------------------------------------------------------------

# ＜ポイント＞
# - カテゴリごとに平均化したデータに対してPCAを適用すると知覚マップを得ることができる
#   --- 平均値を使うことでデータ構造を見誤らないことを確認する必要がある
#   --- バイプロットで主成分とカテゴリ平均値との関係が理解しやすくなる

# ＜知覚マップの注意事項＞
# - 1 集計レベルや集計方法を慎重に選ぶ必要がある
#     --- 全データで作成したバイプロットと知覚マップが類似した傾向であることを確認
# - 2 知覚マップはデータ間の相対関係を示すので、データが追加されるとポジショニングが変わる可能性がある
#     --- データをサンプル抽出するなどするとロバスト性を高めることができる
# - 3 マップ上の矢印との相対的な位置関係でブランド評価することはできない
#     --- PC1とPC2の水準で評価することができる（PCの解釈ができないことが多い）
#     --- 初見の人はブランド(数字)と矢印を結びつけようとするが、それぞれは安定しないため独立して解釈すべき

# データ確認
# --- ブランドごとの評価平均
brand.mean %>% print()

# PCAの実行
brand.mu.pc <- brand.mean %>% select_if(is.numeric) %>% prcomp(scale = TRUE)

# 確認
brand.mu.pc %>% print()
brand.mu.pc %>% summary()
brand.mu.pc %>% listviewer::reactjson(collapsed = TRUE)

# スクリープロット
# --- 集計前と同一イメージ
brand.mu.pc %>% plot(type = "l")

# バイプロット（知覚マップ）
# --- ブランド(a-j)とPC又は原系列の関係が明確になった
brand.mu.pc %>% biplot(main = "Brand positioning", cex = c(1.5, 1))

# ブランド比較
# --- eが平均的なブランド
# --- 各ブランドとeの差を取ることで特性を数量化しやすくすることが可能
# --- 仮にeのような中心近くのサンプルが無くてもPCの絶対値で評価してもよい
c <- brand.mean %>% column_to_rownames("brand") %>% .["c", ]
e <- brand.mean %>% column_to_rownames("brand") %>% .["e", ]
c - e

