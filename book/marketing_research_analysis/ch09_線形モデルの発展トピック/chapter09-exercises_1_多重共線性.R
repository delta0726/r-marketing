# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（エクササイズ）
# Objective : 多重共線性
# Created by: Owner
# Created on: 2021/03/11
# Page      : P325 - P326
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 線形モデルによる予測
# 2 データ変換によるモデルの変化
# 3 高相関の特徴量をPCAで合成
# 4 コードなしの演習


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(car)
library(psych)
library(forecast)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
sales.data.raw <- read_csv("book/marketing_research_analysis/data/chapter9-sales.csv")

# データ確認
sales.data.raw %>% print()
sales.data.raw %>% glimpse()

# サマリー
sales.data.raw %>% summary()


# 1 線形モデルによる予測 --------------------------------------------------------------------

# プロット
# --- 正規性のないデータも含まれる
sales.data.raw %>% pairs.panels()


# モデル構築
# --- とりあえず全変数でモデル化
mod.raw1 <- lm(spendMonth ~ ., sales.data.raw)

# サマリー
# --- 回帰係数で全体的にp値が高い
mod.raw1 %>% glance()
mod.raw1 %>% tidy() %>% mutate(p.value = round(p.value, 3))

# VIFの確認
# --- 5を超える特徴量が含まれている
# --- 多重共線性問題の疑いあり
mod.raw1 %>% vif()


# 修正版のモデル ********************************

# モデル再構築
# --- VIFが5を超える特徴量を削除
mod.raw2 <- lm(spendMonth ~ . - satSite - satPrice, sales.data.raw)

# サマリー
mod.raw2 %>% glance()
mod.raw2 %>% tidy() %>% mutate(p.value = round(p.value, 3))

# VIFの確認
# --- 5を超える特徴量が含まれている
# --- 多重共線性問題の疑いあり
mod.raw2 %>% vif()


# 2 データ変換によるモデルの変化 ------------------------------------------------

# 関数定義
autoTransform <- function(x) {
  return(as.vector(scale(BoxCox(x, BoxCox.lambda(x)))))
}

# データ変換
sales.data <-
  sales.data.raw %>%
    mutate_if(is.numeric, autoTransform)

# データ確認
sales.data %>% print()
sales.data %>% describe()

# モデル構築
mod.tr1 <- sales.data %>% lm(spendMonth ~ . - satSite - satPrice, data = .)

# サマリー
mod.tr1 %>% glance()
mod.tr1 %>% tidy()

# VIFの確認
mod.tr1 %>% vif()


# 3 高相関の特徴量をPCAで合成 ---------------------------------------------------

# 相関係数行列
# --- 満足関連指標(sat*)は相関が高い
sales.data %>% select(satSite:satOverall) %>% cor()

# 主成分分析
# --- PC1で大半が説明できる
sat.pc <- sales.data %>% select(satSite:satOverall) %>% prcomp()
sat.pc %>% summary()

# モデル構築
# --- PCAファクターを導入
mod.tr2 <-
  sales.data %>%
    mutate(satPC = sat.pc$x[ , 1]) %>%
    select(-starts_with("sat")) %>%
    lm(spendMonth ~ . , data = .)

# サマリー
mod.tr2 %>% glance()
mod.tr2 %>% tidy()


# 4 コードなしの演習 --------------------------------------------------------------------------

# 4. [thought exercise without code]. When the model is fit with region as a
#    predictor, it may show the West region with a large, possibly even the
#    largest, effect. Yet it is not statistically significant whereas smaller
#    effects are. Why could that be?
