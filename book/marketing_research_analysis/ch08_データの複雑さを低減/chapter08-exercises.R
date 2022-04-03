# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 8 データの複雑さを低減する（エクササイズ）
# Created by: Owner
# Created on: 2021/02/22
# Page      : P272 - P274
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************




# ＜目次＞
# 0 準備
# === 基礎的な分析 ===
# 1 基本統計量の算出
# 2 データ基準化
# 3 相関係数行列のプロット
# 4 ブランド別の評価平均
# === 主成分分析(PCA) ===
# 5 主成分分析の実行
# 6 知覚マップの作成
# 7 ポジショニングの変更
# === 探索的因子分析(EFA) ===
# 8 探索的因子分析の実行
# 9 適切な因子数と回転
# 10 ヒートマップの作成
# 11 ブランドごとの因子得点
# 12 因子分析で見るブランド戦略
# === 多次元尺度構成法(MDS) ===
# 13 ブランドの多次元尺度マップの作成
# 14 MDSマップとPCA/EFAの関係性


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(psych)
library(corrplot)
library(gplots)
library(RColorBrewer)
library(nFactors)
library(GPArotation)
library(gplots)
library(RColorBrewer)
library(semPlot)
library(conflicted)


# データ準備
prst1 <- read_csv("data/chapter8-brands1.csv")


# 1 基本統計量の算出 ---------------------------------------------------------------------

# データ確認
# --- 1-7の順序データ
# --- Brandのみカテゴリカルデータ
prst1 %>% print()
prst1 %>% glimpse()
prst1 %>% map(table)

# 基本統計量の算出
# --- 数値データとして扱うのであれば基準化したほうがよさそう
# --- どのような分析をするかによって異なる
prst1 %>% summary()
prst1 %>% describe()


# 2 データ基準化 ------------------------------------------------------------------------

# データ基準化
prst1.sc <-
  prst1 %>%
    select(Adaptable:Intuitive) %>%
    scale() %>%
    data.frame() %>%
    mutate(Brand = prst1$Brand) %>%
    as_tibble()

# 基本統計量
prst1.sc %>% describe()


# 3 相関係数行列のプロット ----------------------------------------------------------------

# プロット作成
# --- 相関係数行列（階層クラスタリングあり）
# --- 大きく3カテゴリが存在することを示唆している
prst1 %>%
  select(Adaptable:Intuitive) %>%
  cor() %>%
  corrplot(order = "hclust")


# 4 ブランド別の評価平均 -----------------------------------------------------------------

# グループごとの平均算出
prst1.mean <-
  prst1 %>%
    group_by(Brand) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    as.data.frame() %>%
    remove_rownames() %>%
    column_to_rownames("Brand")

# 確認
rst1.mean %>% print()

# ヒートマップの作成
prst1.mean %>%
  as.matrix() %>%
  heatmap.2(col = brewer.pal(9, "GnBu"), trace = "none", key = FALSE,
            dend = "none", main = "\n\n\n\n\nBrand attributes", margins = c(8, 6))


# 5 主成分分析の実行 --------------------------------------------------------------------

# 主成分分析
prst1.pc <-
  prst1 %>%
    select(Adaptable:Intuitive) %>%
    prcomp()

# サマリー
# --- 主成分の数は9
# --- PC5で7割の分散を説明している
prst1.pc %>% summary()

# 分散プロット
# --- PCごとの分散量
prst1.pc %>% plot(type = "l")

# バイプロット
prst1.pc %>% biplot()


# 6 知覚マップの作成 --------------------------------------------------------------------

# ＜ポイント＞
# - 集計データを用いてPCAを実行することで知覚マップを作成することができる
#   --- 知覚マップは集計データを用いることでポジショニングを見やすくしている
#   --- 元データでPCAを行う場合と同じようなプロットが得られる

# データ確認
# --- 4で作成したデータ
prst1.mean %>% print()

# 主成分分析
# --- 集計データに対してPCAを実行
prst1.mu.pc <-
  prst1.mean %>%
    prcomp()

# 知覚マップ
# --- PC1 vs PC2
# --- PC2 vs PC3
prst1.mu.pc %>% biplot()
prst1.mu.pc %>% biplot(choices = 2:3)


# 7 ポジショニングの変更 --------------------------------------------------------------------

# ＜質問＞
# - Tagnoのポジショニングを変更するにあたり、PCAからどのような示唆を受けることができるか？

# ＜回答＞
# -


# 8 探索的因子分析の実行 --------------------------------------------------------------------

# 因子数の推定
# --- VSSで因子数を推定
prst1 %>%
  select(Adaptable:Intuitive) %>%
  cor() %>%
  nfactors()


# 9 適切な因子数と回転 --------------------------------------------------------------------

# 因子数の検討
prst1 %>% select(Adaptable:Intuitive) %>% factanal(factors = 2)
prst1 %>% select(Adaptable:Intuitive) %>% factanal(factors = 3)
prst1 %>% select(Adaptable:Intuitive) %>% factanal(factors = 4)

# 回転の検討
prst1 %>% select(Adaptable:Intuitive) %>% factanal(factors = 3, rotation = "varimax")
prst1 %>% select(Adaptable:Intuitive) %>% factanal(factors = 3, rotation = "oblimin")


# 10 ヒートマップの作成 ------------------------------------------------------------------

# 因子分析の実行
prst1.fa <-
  prst1 %>%
    select(Adaptable:Intuitive) %>%
    factanal(factors = 3, rotation = "varimax")

# ヒートマップの作成
prst1.fa$loadings %>%
  heatmap.2(col = brewer.pal(9, "Blues"), trace = "none",
            key = FALSE, dend = "none", Colv = FALSE, cexCol = 1.2,
            main = "\n\n\n\n\nFactor loadings for brand adjectives")

# パス図
prst1.fa %>%
  semPaths(what = "est", residuals = FALSE, cut = 0.3,
           posCol = c("white", "darkblue"), negCol = c("white", "red"),
           edge.label.cex = 0.75, nCharNodes = 7)


# 11 ブランドごとの因子得点 ----------------------------------------------------------------

# 因子分析の実行
prst1.fa <-
  prst1 %>%
    select(Adaptable:Intuitive) %>%
    factanal(factors = 3, scores = "Bartlett", rotation = "varimax")

# 因子スコアの算出
prst1.scores <-
  prst1.fa$scores %>%
    data.frame() %>%
    mutate(Brand = prst1$Brand) %>%
    as_tibble()

# 確認
prst1.scores %>% print()
prst1.scores %>% summary()

# ブランドごとの平均点
prst1.fa.mean <-
  prst1.scores %>%
    group_by(Brand) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    as.data.frame() %>%
    remove_rownames() %>%
    column_to_rownames("Brand")

# 確認
prst1.fa.mean %>% print()

# ヒートマップの作成
prst1.fa.mean %>%
  as.matrix() %>%
  heatmap.2(col = brewer.pal(9, "Blues"), trace = "none", key = FALSE,
            margin = c(8, 8), dend = "none")


# 12 因子分析で見るブランド戦略 ----------------------------------------------------------------

# ＜質問＞
# - 主成分分析と因子分析でブランド戦略の解釈にどのような違いがあるか
# - SierraとTagnoのブランド戦略の方向性


# 13 ブランドの多次元尺度マップの作成 -----------------------------------------------------------

# 多次元尺度の計算
# --- 距離行列から作成
prst1.mds <-
  prst1.mean %>%
    dist() %>%
    cmdscale()

# 確認
prst1.mds %>% print()

# 多次元尺度マップ
prst1.mds %>% plot(type="n")
prst1.mds %>% text(rownames(prst1.mds), cex = 1.2)


# 14 MDSマップとPCA/EFAの関係性 -----------------------------------------------------------


