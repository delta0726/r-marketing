# ***********************************************************************************************
# Title   : Rによる実践的マーケティングリサーチと分析
# Chapter : 10 確認的因子分析と構造方程式モデル（メイン）
# Theme   : 確認的因子分析
# Date    : 2022/4/8
# Page    : P340 - P348
# URL     : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 確認的因子分析ではデータ構造(パス図)をあらかじめフォーミュラで定義してフィッティング度合いを確認する
# - 課題に対する考え方の整理とデータ解釈が両立できた際に効果を発揮する


# ＜目次＞
# 0 準備
# 1 モデル推定
# 2 SEMプロットの作成
# 3 PIES CFAモデルの評価
# 4 各モデルの比較


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lavaan)
library(semTools)
library(semPlot)
library(car)
library(psych)
library(car)
library(RColorBrewer)
library(conflicted)

conflict_prefer("some", "car", quiet = TRUE)
conflict_prefer("logit", "car", quiet = TRUE)


# データロード
piesSimData <- read_csv("data/piesSimData.csv")

# データ確認
piesSimData %>% print()
piesSimData %>% glimpse()
piesSimData %>% describe()


# 1 モデル推定 ------------------------------------------------------------------------

# ＜ポイント＞
# - 始めに3つの潜在変数(General/Fuature/Image)をモデル化する
# - 次にPIES潜在変数を3つの因子に対する構成概念としてモデル化する


# モデル定義
# --- 潜在変数に名前を付けて定義する
piesModel <- " General =~ i1 + i2 + i3
               Feature =~ i4 + i5 + i6  + i7
               Image   =~ i8 + i9 + i10 + i11
               PIES    =~ General + Feature + Image "

# 学習
pies.fit <- piesModel %>% cfa(data = piesSimData)

# 確認
pies.fit %>% print()
pies.fit %>% summary(fit.measures=TRUE)
pies.fit %>% listviewer::reactjson(collapsed = TRUE)


# 2 SEMプロットの作成 ----------------------------------------------------------------------

# ＜ポイント＞
# - 確認的因子分析(cfa)で定義した構造に基づくパス図で関係性を表現する


# プロット作成
pies.fit %>%
  semPaths(what = "est", fade = FALSE, residuals = FALSE,
           edge.label.cex = 0.75)


# 3 PIES CFAモデルの評価 -------------------------------------------------------------------

# ＜ポイント＞
# - SEMではモデル構造を変えた代替モデルの方が効果的な場合がある
#   --- 複数パターンで検証する必要がある


# ＜代替案＞
# - 1： 1つの関与因子が全ての観測項目に影響を与える単一因子モデル
# - 2： 相関を持たない3つの独立因子が3グループの各項目に対応する3因子モデル


# 代替案1 ------------------------------------------------------

# モデル定義
piesModelNH1 <- " PIES =~ i1 + i2 + i3 + i4 + i5 + i6  + 
                          i7 + i8 + i9 + i10 + i11 "

# 学習
pies.fit.NH1 <- piesModelNH1 %>% cfa(data = piesSimData)

# プロット
pies.fit.NH1 %>%
  semPaths(what = "est", fade = FALSE, residuals = FALSE,
           edge.label.cex = 0.75)


# 代替案2 ------------------------------------------------------

# モデル定義
piesModelNH3 <- " General =~ i1 + i2 + i3
                  Feature =~ i4 + i5 + i6  + i7
                  Image   =~ i8 + i9 + i10 + i11
                  General ~~ 0.1*Feature
                  General ~~ 0.1*Image
                  Feature ~~ 0.1*Image "

# 学習
pies.fit.NH3 <- piesModelNH3 %>% cfa(data = piesSimData)

# プロット
pies.fit.NH3 %>%
  semPaths(what = "est", fade = FALSE, residuals = FALSE,
           edge.label.cex = 0.75)


# 4 各モデルの比較 ----------------------------------------------------------------------

# モデル比較
compareFit(pies.fit.NH1, pies.fit.NH3, pies.fit) %>% summary()
