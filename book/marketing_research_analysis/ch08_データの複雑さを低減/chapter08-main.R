# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 8 データの複雑さを低減する（メイン）
# Objective : TODO
# Created by: Owner
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
#
# 2 因子分析
#   - 主成分分析と同様に少ない次元で分散を捉えるが、元の変数によって次元解釈が可能
#
# 3 多次元尺度構成法
#   - 観測値の類似度を2次元プロットのような低次元空間に射影


# ＜目次＞
# 0 準備
# 1 データの相関関係
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
brand.ratings %>% print()
brand.ratings %>% glimpse()

# データサマリー
# --- 各項目を1-10で表している
brand.ratings %>% summary()
brand.ratings %>% map(table)

# データ変換
# --- Zスコア変換
brand.sc <-
  brand.ratings %>%
    mutate_at(1:9, function(x) (x - mean(x)) / sd(x)) %>%
    as_tibble()

# 確認
brand.sc %>% print()
brand.sc %>% summary()


# 1 データの相関関係 -----------------------------------------------------------------------

# 相関プロット
# --- 階層クラスタリングで並び替え
brand.sc %>%
  select(1:9) %>%
  cor() %>%
  corrplot(order = "hclust")


# 2 ヒートマップによるブランド評価 ------------------------------------------------------------

# ブランドごとの評価
# --- 平均値
brand.mean <- aggregate(. ~ brand, data = brand.sc, mean)
brand.mean

# ブランド名を列名に変換
brand.mean <-
  brand.mean %>%
    column_to_rownames("brand")

# 確認
brand.mean %>% print()

# ヒートマップ
# --- ブランド属性
brand.mean %>%
  as.matrix() %>%
  heatmap.2(col = brewer.pal(9, "GnBu"), trace = "none", key = FALSE,
            dend = "none", main = "\n\n\n\n\nBrand attributes")



# 3 主成分分析の基礎 ----------------------------------------------------------------------

# データ作成
set.seed(98286)
xvar <- 1:10 %>% sample(100, replace = TRUE)
yvar <- xvar
yvar[sample(1:length(yvar), 50)] <- 1:10 %>% sample( 50, replace=TRUE)
zvar <- yvar
zvar[sample(1:length(zvar), 50)] <- 1:10 %>% sample( 50, replace=TRUE)
my.vars <- cbind(xvar, yvar, zvar)

# 散布図
# --- jitter()で元データにノイズを加える
my.vars %>% jitter() %>% plot(yvar ~ xvar, data = .)

# 相関係数
my.vars %>% cor()

# 主成分分析
my.pca <- my.vars %>% prcomp()

# 確認
my.pca %>% summary()
my.pca %>% glimpse()

# 相関係数
# --- 主成分の相関はゼロ
my.pca$x %>% cor()

# バイプロット
my.pca %>% biplot(scale = TRUE)


# 4 ブランド評価とPCA ----------------------------------------------------------------------

# PCAの実行
brand.pc <- brand.sc %>% select(1:9) %>% prcomp()

# 確認
brand.pc %>% print()
brand.pc %>% summary()

# スクリープロット
# --- PCごとの主成分寄与度を示す
brand.pc %>% plot(type = "l")

# バイプロット
# --- PC1とPC2で元系列を射影
# --- PC1とPC2で観測値をマッピング
brand.pc %>% biplot()


# 5 PCAと知覚マップ ----------------------------------------------------------------------

# データ確認
# --- ブランドごとの評価平均
brand.mean %>% print()

# PCAの実行
brand.mu.pc <- brand.mean %>% prcomp(scale = TRUE)

# 確認
brand.mu.pc %>% print()
brand.mu.pc %>% summary()

# スクリープロット
# --- 集計前と同一イメージ
brand.mu.pc %>% plot(type = "l")

# バイプロット（知覚マップ）
# --- ブランド(a-j)とPC又は原系列の関係が明確になった
brand.mu.pc %>% biplot(main = "Brand positioning", cex = c(1.5, 1))

# ブランド比較
# --- eが平均的なブランド
brand.mean["c", ] - brand.mean["e", ]

# ブランド比較
colMeans(brand.mean[c("b", "c", "f", "g"), ]) - brand.mean["e", ]


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
