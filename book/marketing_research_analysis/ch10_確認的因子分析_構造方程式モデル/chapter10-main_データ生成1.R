# ***********************************************************************************************
# Title   : Rによる実践的マーケティングリサーチと分析
# Chapter : 10 確認的因子分析と構造方程式モデル（メイン）
# Theme   : シミュレーションデータ生成（piesSimData）
# Date    : 2022/4/5
# Page    : P336 - P340
# URL     : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - lavaan::simulateData()を使って確認的因子モデル(CFA)で使用するデータを生成する


# ＜目次＞
# 0 準備
# 1 モデル定義
# 2 シミュレーションデータの生成
# 3 シミュレーションデータの検証


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(magrittr)
library(tidyverse)
library(lavaan)
library(car)
library(psych)
library(RColorBrewer)
library(conflicted)

conflict_prefer("some", "car", quiet = TRUE)
conflict_prefer("logit", "car", quiet = TRUE)



# 1 モデル定義 ---------------------------------------------------------------------------------

# ＜ポイント＞
# - 構造モデルはフォーミュラとして定義される


# 構造モデルのイメージ
# --- PIESが3つの要素で構成され、各要素も複数の変数で定義される構造を表現
# --- ｢=~｣の記号は｢~によって観測される｣という意味を持つ
piesModel <- " General =~ i1 + i2 + i3
               Feature =~ i4 + i5 + i6  + i7
               Image   =~ i8 + i9 + i10 + i11
               PIES =~ General + Feature + Image "


# 構造モデルの定義
# --- 各変数に係数を付与してモデルを定義する
piesDataModel <- " General =~ 0.9*i1 + 0.7*i2 + 0.5*i3
                   Feature =~ 0.3*i3 + 0.7*i4 + 0.9*i5 + 0.5*i6  + 0.9*i7
                   Image   =~ 0.2*i3 + 0.8*i8 + 0.9*i9 + 0.8*i10 + 0.7*i11 
                   PIES =~ 0.7*General + 0.8*Feature + 0.8*Image"


# 2 シミュレーションデータの生成 ---------------------------------------------------------------

# ＜ポイント＞
# - 定義済の構造モデルをもとにシミュレーションデータを生成することができる
#   --- 連続値で出力されるので離散化しておく


# データ作成
# --- 連続値を分位化で離散値に変換
set.seed(10001)
piesSimData.norm <-
  piesDataModel %>%
    simulateData(sample.nobs = 3600) %>%
    lapply(function(x) {cut(x, breaks=7, labels=FALSE)}) %>%
    as_tibble()

# データ確認
piesSimData.norm %>% class()
piesSimData.norm %>% as_tibble()


# 3 シミュレーションデータの検証 -----------------------------------------------------------------

# 統計量の確認
piesSimData %>% describe()

# プロット作成
# --- 離散値の密度を確認
piesSimData[, c(1, 2, 4, 5, 8, 9)] %>%
  scatterplotMatrix(col = brewer.pal(3, "Paired"), ellipse=TRUE)

# 探索的因子分析の実行
# --- 3ファクターで想定したモデルを捉えている
piesSimData %>% factanal(factors = 3) %>% use_series(loadings)
