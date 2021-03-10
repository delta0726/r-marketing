# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（メイン）
# Objective : 階層線形モデル
# Created by: Owner
# Created on: 2021/02/22
# Page      : P295 - P307
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞


# ＜目次＞
# 0 準備
# 1 線形モデルの構築
# 2 階層線形モデルの構築
# 3 完全な階層線形モデル


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(broom.mixed)
library(lme4)
library(arm)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
conjoint.df <- read_csv("book/marketing_research_analysis/data/conjoint_df.csv")

# データ加工
conjoint.df <-
  conjoint.df %>%
    mutate(speed = factor(speed),
           height = factor(height))

# データ概要
conjoint.df %>% print()
conjoint.df %>% glimpse()


# 1 線形モデルの構築 ----------------------------------------------------------------------------

# サマリー
conjoint.df %>% summary()

# データ集計
# --- 高さごとのレーティング
# --- 中間的な高さで評価が高まる
conjoint.df %>%
  group_by(height) %>%
  summarise(rating = mean(rating)) %>%
  ungroup()

# 線形モデルの構築
# --- 目的変数をratingとする
ride.lm <- conjoint.df %>% lm(rating ~ speed + height + const + theme, data = .)

# サマリー
ride.lm %>% summary()
ride.lm %>% tidy()


# 2 階層線形モデルの構築 --------------------------------------------------------------------------

# ＜lme4の記法＞
# - + (predoctors | group)
# - ランダム効果(predoctors)とグループ化変数(group)を選ぶ

# データ確認
conjoint.df %>% print()

# resp.id
# --- 個人が16種類のコースターを評価している
conjoint.df %>% group_by(resp.id) %>% tally()

# 線形モデルの構築
# --- 定数項のみを階層化するので1を指定
# --- グループはresp.id（個人を特定）
ride.hlm1 <-
  conjoint.df %>% lmer(rating ~ speed + height + const + theme + (1 | resp.id), data = .)

# サマリー
ride.hlm1 %>% summary()
ride.hlm1 %>% tidy()

# 固定効果
ride.hlm1 %>% fixef()

# 個人のランダム効果
# --- 200人分
ride.hlm1 %>% ranef() %>% use_series(resp.id) %>% head()

# 個人の固定効果
# --- 今回は定数項のみ
# --- 200人分
ride.hlm1 %>% coef() %>% use_series(resp.id) %>% head()


# 3 完全な階層線形モデル --------------------------------------------------------------------------

# model with random intercept & slope by respondent = (predictors | resp.id)
#
# consider 1M+ iterations; using 100K for somewhat faster time (~5 min)
# WARNING: slow, takes several minutes!
#
# NOTE: limited degrees of freedom and overfitting in this data set!
#       we will use the "Nelder-Mead" algorithm to force optimization
#

# モデル構築
# --- 各変数を階層化する
# --- グループはresp.id（個人を特定）
ride.hlm2 <-
  lmer(rating ~ speed + height + const + theme + (speed + height + const + theme | resp.id),
       data = conjoint.df,
       control = lmerControl(optCtrl = list(maxfun = 100000),
                             optimizer = "Nelder_Mead"))

# サマリー
ride.hlm2 %>% summary()
ride.hlm2 %>% tidy()

# 固定効果
ride.hlm2 %>% fixef()

# 個人のランダム効果
# --- 200人分
ride.hlm2 %>% ranef() %>% use_series(resp.id) %>% head()

# 個人の固定効果
# --- 今回は定数項のみ
# --- 200人分
ride.hlm2 %>% coef() %>% use_series(resp.id) %>% head()


# 総合スコアの算出
# --- 固定効果 + ランダム効果
fixef(ride.hlm2) + ranef(ride.hlm2)$resp.id[196, ]
coef(ride.hlm2)$resp.id[196, ]

# 信頼区間の算出
# --- 書籍に記載なし
head(se.ranef(ride.hlm2)$resp.id)
ranef(ride.hlm2)$resp.id[196, ] - 1.96 * se.ranef(ride.hlm2)$resp.id[1,]
ranef(ride.hlm2)$resp.id[196, ] + 1.96 * se.ranef(ride.hlm2)$resp.id[1,]

