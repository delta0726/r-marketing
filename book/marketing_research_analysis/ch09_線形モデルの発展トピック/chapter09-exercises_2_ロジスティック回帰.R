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
# 1 線形モデルによる予測(5)
# 2 説明変数の追加による変化(6)
# 3 相互効果の確認(7)


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(car)
library(psych)
library(forecast)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# 関数定義
autoTransform <- function(x) {
  return(as.vector(scale(BoxCox(x, BoxCox.lambda(x)))))
}

# データ準備
sales.data.raw <- read_csv("book/marketing_research_analysis/data/chapter9-sales.csv")

# PCAファクターの作成
satPC <-
  sales.data.raw %>%
    select(starts_with("sat")) %>%
    prcomp() %>%
    use_series(x) %>%
    .[,1]

# データ変換
sales.data <-
  sales.data.raw %>%
    select(-starts_with("sat")) %>%
    mutate_if(is.numeric, autoTransform) %>%
    mutate(satPC = satPC,
           purchase = sales.data.raw$purchase)

# データ確認
sales.data.raw %>% print()
sales.data.raw %>% glimpse()
sales.data$purchase %>% table()

# サマリー
sales.data.raw %>% summary()


# 1 線形モデルによる予測(5) ------------------------------------------------------------------

# モデル構築
# --- クーポン効果の測定
# --- 二項分布を想定（以降共通）
purchase.lr1 <-
  sales.data %>%
    glm(purchase ~ coupon, data = ., family = binomial)

# 確認
purchase.lr1 %>% glance()
purchase.lr1 %>% tidy()


# 2 説明変数の追加による変化(6) ------------------------------------------------------------------

# モデル構築
purchase.lr2 <-
  sales.data %>%
    glm(purchase ~ coupon + spendToDate + region + satPC, data = ., family = binomial)

# 確認
purchase.lr2 %>% glance()
purchase.lr2 %>% tidy()


# 3 相互効果の確認(7) -----------------------------------------------------------------------

# モデル構築
purchase.lr3 <-
  sales.data %>%
    glm(purchase ~ coupon * satPC, data = ., family = binomial)

# 確認
purchase.lr3 %>% glance()
purchase.lr3 %>% tidy()

summary(purchase.lr3)

# 8. What is the best estimate for how much a coupon is related to increased purchase, as
#    an odds ratio?
plogis(0.342) / 0.5   # using model 3


## EXTRAS not in book
##
# x1. What is the change in purchase likelihood, in relation to a change of 1
#    unit of satisfaction? (Hint: what is a unit of satisfaction?) Approximately
#    how many points would ``1 unit'' be, on the actual 1-10 rating scales?)
plogis(-0.64) / 0.5  # using model 3

# 1 unit is 1 sd, so look at the sd in the raw data
library(psych)
sales.data.raw[ , 5:8] %>% describe()

# x2. [thought exercise] For product strategy, what questions are suggested by
#     this relationship between satisfaction and purchase? What possible
#     explanations are there, or what else would you wish to know?
##
## END EXTRAS
