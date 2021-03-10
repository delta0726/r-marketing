# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（メイン）
# Objective : 多重共線性
# Created by: Owner
# Created on: 2021/02/22
# Page      : P275 - P282
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 多重共線性問題は最も認知された線形回帰モデルの落とし穴といえる
# - ここでは｢変数除外｣と｢PCA変換｣について学ぶ
#   --- 特徴量選択の機能を内包する正則化回帰がより実践的なソリューションといえる


# ＜目次＞
# 0 準備
# 1 最初の線形モデル
# 2 データ加工
# 3 多重共線性の確認
# 4 多重共線性への対処1：変数除外
# 5 多重共線性への対処2：PCA変換


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(psych)
library(car)
library(forecast)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
# --- Y：オンライン支出
cust.df <- read_csv("book/marketing_research_analysis/data/cust.df.csv")

# データ概要
cust.df %>% print()
cust.df %>% glimpse()

# サマリー
cust.df %>% summary()


# 1 最初の線形モデル ----------------------------------------------------------------------

# ＜ポイント＞
# - 何も考えずに元データを線形回帰モデルに適用
#   --- 線形回帰の前提を満たさないので、解釈しにくい結果が出てくる

# モデル定義
# --- 基準化もしていない状態
spend.m1 <-
   cust.df %>%
     select(-cust.id) %>%
     filter(online.spend > 0) %>%
     lm(online.spend ~ ., data = .)

# データ確認
# --- 前処理していないので参考にならない
spend.m1 %>% summary()
spend.m1 %>% tidy()


# プロット作成
# --- 元データ(数値データのみ)
# --- 対数変換が必要そうなデータが散見される
cust.df %>%
  select_if(is.numeric) %>%
  scatterplotMatrix()


# 2 データ加工 ---------------------------------------------------------------------------

# 関数定義
# --- Box-Cox変換してZスコア化
autoTransform <- function(x) {
  return(as.vector(scale(BoxCox(x, BoxCox.lambda(x)))))
}

# データ加工
cust.df.bc <-
  cust.df %>%
    drop_na() %>%
    select(-cust.id) %>%
    filter(online.spend > 0) %>%
    mutate_if(is.numeric, autoTransform)

# データ確認
cust.df.bc %>% print()
cust.df.bc %>% glimpse()
cust.df.bc %>% describe()

# プロット作成
# --- だいぶん歪みが解消された
cust.df.bc %>%
  select_if(is.numeric) %>%
  scatterplotMatrix()


# 3 多重共線性の確認 ---------------------------------------------------------------------------

# ＜ポイント＞
# - モデル2は多重共線性が生じている


# モデル定義2
spend.m2 <-
  cust.df.bc %>%
     lm(online.spend ~ ., data = .)

# データ確認
# --- online.transが圧倒的に有意
spend.m2 %>% summary()
spend.m2 %>% tidy()

# モデル定義3
# --- online.transのみでモデル化
spend.m3 <-
  cust.df.bc %>%
     lm(online.spend ~ online.trans, data = .)

# データ確認
# --- online.transの傾きがほとんど1
spend.m3 %>% summary()
spend.m3 %>% tidy()

# 分散分析
anova(spend.m3, spend.m2)

# VIFの確認
# --- 10以上が複数で確認される
spend.m2 %>% vif()

# 相関係数行列
# --- 相関が非常に高い変数が含まれている
cust.df.bc %>%
  select_if(is.numeric) %>%
  cor() %>%
  corPlot()


# 4 多重共線性への対処1：変数除外 -------------------------------------------------------------------

# VIFの確認（再掲）
spend.m2 %>% vif()

# モデル定義4
# --- VIFが10を超える変数を削除
spend.m4 <-
  cust.df.bc %>%
     lm(online.spend ~ . -online.trans -store.trans, data = .)

# VIFの確認
# --- 10以上がなくなった
spend.m4 %>% vif()

# データ確認
spend.m4 %>% summary()
spend.m4 %>% tidy()


# 5 多重共線性への対処2：PCA変換 ------------------------------------------------------------------

# solution 2: principal components

# 相関係数の確認
# --- 高相関であることを確認
cust.df.bc %>% select(online.visits, online.trans) %>% cor()
cust.df.bc %>% select(store.trans, store.spend) %>% cor()

# データ加工
# --- 高相関ペアのPC1を特徴量に追加
# --- 変数の性質に応じて名前を付けている
cust.df.bc.pc <-
  cust.df.bc %>%
    mutate(online = prcomp(select(cust.df.bc, online.visits, online.trans))$x[ , 1],
           store  = prcomp(select(cust.df.bc, store.trans, store.spend))$x[ , 1])

# データ確認
cust.df.bc.pc %>% glimpse()

# モデル定義4
# --- 高相関ペアの説明変数を削除
# --- PCAで合成した説明変数を追加
spend.m5 <-
  cust.df.bc.pc %>%
     lm(online.spend ~ email + age + credit.score + distance.to.store +
                      sat.service + sat.selection + online + store, data = .)

# VIFの確認
# --- 最大で約1.5程度となった
spend.m5 %>% vif()

# データ確認
# --- online.transの傾きがほとんど1
spend.m5 %>% summary()
spend.m5 %>% tidy()

