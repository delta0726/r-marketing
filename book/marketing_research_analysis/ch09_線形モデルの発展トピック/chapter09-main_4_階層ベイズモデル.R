# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（メイン）
# Objective : 階層ベイズモデル
# Created by: Owner
# Created on: 2021/02/22
# Page      : P307 - P315
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 階層モデルは{lme4}を用いた古典的な手法でも推定できるが、本来はベイズ推定のほうが適している
#   --- ベイズ推定は各個体の観測値がほとんどなくても最良の推定値を得ることができる
#   --- 階層モデルは個人のサンプル数が少なくなる可能性がある


# ＜目次＞
# 0 準備
# 1 ベイズ線形モデル
# 2 MCMCによる階層線形モデル
# 3 嗜好の分布を調べる


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(broom.mixed)
library(MCMCpack)
library(arm)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
# --- ジェットコースター16種類に対する200人のアンケート（3200レコード）
conjoint.df <- read_csv("book/marketing_research_analysis/data/conjoint_df.csv")

# データ加工
conjoint.df <-
  conjoint.df %>%
    mutate(speed = factor(speed),
           height = factor(height))

# データ概要
conjoint.df %>% print()
conjoint.df %>% glimpse()


# 1 ベイズ線形モデル --------------------------------------------------------------------------

# ＜ポイント＞
# - ベイズ線形モデルを用いてデータ全体でモデリングする
#   --- 非階層モデルとして推定


# モデル構築
# --- MCMCpack::MCMCregress()
set.seed(97439)
ride.mc1 <- conjoint.df %>% MCMCregress(rating ~ speed + height + const + theme, data = .)

# サマリー
# --- 頻度論の線形回帰モデルとほぼ同じ結果
ride.mc1 %>% summary()
ride.mc1 %>% tidy()

# 参考：線形回帰モデル
ride.lm <- conjoint.df %>% lm(rating ~ speed + height + const + theme, data = .)
ride.lm %>% tidy()


# 2 MCMCによる階層線形モデル -------------------------------------------------------------------

# ＜ポイント＞
# - MCMChregress()は関数名にhが含まれており、階層モデルであることを意味している
# - {lme4}とは記法が異なるので注意


# ＜階層ベイズモデル＞
# - 階層モデルでは、各回答者は｢取り得るすべての嗜好を構成する大きな分布から抽出した1つの嗜好を持つ｣と仮定
# - ｢各個人の係数｣と｢それらの個人を最もよく表現する高次分布｣の両方を推定する
# - カテゴリの回答者が少ない場合、MCMC法では回答者にわたって情報をプールして推定を行う
#   --- 引数r, Rでコントロールする


# ＜引数：MCMChregress＞
# fixed  ：全ての回答者で同一の上位レベルの固定効果を表すformula
# random ：回答者ごとに推定される段田無効化を表すformula
# group  ：ランダム効果をグループ化するためのカテゴリ情報
# data   ：観測値のデータフレーム
# r      ：モデルのパラメータ数
# R      ：対角成分にモデルのパラメータ数を持つ対角行列を設定


# モデル構築
set.seed(97439)
ride.mc2 <-
  conjoint.df %>%
    MCMChregress(fixed = rating ~ speed + height + const + theme,
                 random = ~ speed + height + const + theme,
                 group = "resp.id", data = ., r = 8, R = diag(8))

# データ構造
# --- mcmcオブジェクトには
ride.mc2 %>% glimpse()
ride.mc2$mcmc %>% dim()

# 個別データの確認
ride.mc2$mcmc[ ,1:8] %>% summary()

