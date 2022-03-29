# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 8 データの複雑さを低減する（メイン）
# Created on: 2021/02/22
# Page      : P237 - P271
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
# 6 探索的因子分析の概要
# 7 因子軸の回転
# 8 多次元尺度法


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(corrplot)
library(gplots)
library(RColorBrewer)
library(nFactors)
library(GPArotation)
library(gplots)
library(RColorBrewer)
library(semPlot)
library(cluster)
library(MASS)
library(conflicted)


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
my.vars2 <- read_csv("data/chap8_pca_samle.csv")

# 散布図
# --- jitter()で元データにノイズを加える（プロットの重なりを回避）
my.vars %>% jitter() %>% plot(yvar ~ xvar, data = .)

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


# 6 探索的因子分析の概要 -------------------------------------------------------------------

# 因子数のイメージ確認
# --- 固有値から求める
brand.sc %>%
  select(1:9) %>%
  cor() %>%
  eigen() %>%
  use_series(values) %>%
  nScree()

# 因子数のイメージ確認
# --- 固有値の水準から求める
# --- 1以上が有効な因子
brand.sc %>%
  select(1:9) %>%
  cor() %>%
  eigen() %>%
  use_series(values)

# 因子分析の実行
brand.fa.2 <- brand.sc %>% select(1:9) %>% factanal(factors = 2)
brand.fa.3 <- brand.sc %>% select(1:9) %>% factanal(factors = 3)

# 確認
brand.fa.2 %>% print()
brand.fa.3 %>% print()

# ヒートマップの作成
brand.fa.3$loadings %>%
  heatmap.2(col = brewer.pal(9, "Greens"), trace="none", key=FALSE,
            dend="none", Colv=FALSE, cexCol = 1.2,
            main="\n\n\n\n\nFactor loadings for brand adjectives")


# 7 因子軸の回転 -------------------------------------------------------------------------

# 因子軸の回転
brand.fa.ob <-
  brand.sc %>%
    select(1:9) %>%
    factanal(factors=3, rotation="oblimin")

# ヒートマップの作成
brand.fa.ob$loadings %>%
  heatmap.2(col = brewer.pal(9, "Greens"), trace = "none",
            key = FALSE, dend = "none", Colv = FALSE, cexCol = 1.2,
            main = "\n\n\n\n\nFactor loadings for brand adjectives")


# plot the structure
brand.fa.ob %>%
  semPaths(what = "est", residuals = FALSE, cut = 0.3,
           posCol = c("white", "darkgreen"), negCol = c("white", "red"),
           edge.label.cex = 0.75, nCharNodes = 7)


# use regression scores

brand.fa.ob <-
  brand.sc %>%
    select(1:9) %>%
    factanal(factors = 3, rotation = "oblimin", scores = "Bartlett")



brand.scores <- brand.fa.ob$scores %>% data.frame()

brand.scores$brand <- brand.sc$brand
brand.scores %>% head()

brand.fa.mean <-
  aggregate(. ~ brand, data = brand.scores, mean) %>%
    column_to_rownames("brand") %>%
    set_colnames(c("Leader", "Value", "Latest"))

brand.fa.mean


brand.fa.mean %>%
  as.matrix() %>%
  heatmap.2(col = brewer.pal(9, "GnBu"), trace = "none",
            key = FALSE, dend = "none", cexCol = 1.2,
            main = "\n\n\n\n\nMean factor score by brand")


# 8 多次元尺度法 -------------------------------------------------------------------------

# 距離行列
brand.dist <- brand.mean %>% dist()

# MDS
brand.mds <- brand.dist %>% cmdscale()

# 確認
brand.mds %>% print()

# プロット
brand.mds %>% plot(type = "n")
brand.mds %>% text(rownames(brand.mds), cex = 2)


# non-metric MDS alternative 
brand.rank <- brand.mean %>% lapply(function(x) ordered(rank(x))) %>% data.frame()

# データ確認
brand.rank %>% print()
brand.rank %>% glimpse()

brand.dist.r <- brand.rank %>% daisy(metric = "gower")
brand.dist.r

brand.mds.r <- brand.dist.r %>% isoMDS()
brand.mds.r

brand.mds.r$points %>% plot(type = "n")
brand.mds.r$points %>% text(levels(brand.sc$brand), cex = 2)


# the following is NOT in the book
# same general solution as PCA, but with clustering instead 
# this is a preview of the segmentation chapter
# solving for k=3 clusters, these are the closest-related brands

brand.dist %>%
  fanny(k = 3) %>%
  clusplot(color = FALSE, shade = FALSE, labels = 3, lines = 0,
           plotchar = FALSE, main="Brand perception groups")
