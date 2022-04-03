# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（メイン）
# Objective : 多重共線性
# Created on: 2021/02/22
# Page      : P275 - P282
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 多重共線性問題は最も認知された線形回帰モデルの落とし穴となっている
# - ここでは多重共線性の認知方法と対処方法を学ぶ
#   --- 認知方法 ：VIF
#   --- 対処方法 ：変数除外 / PCA変換(主成分合成)
#   --- 正則化回帰がより実践的なソリューションといえる（特徴量選択の機能）


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
library(magrittr)
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
cust.df <- read_csv("data/cust.df.csv")

# データ概要
cust.df %>% print()
cust.df %>% glimpse()

# サマリー
cust.df %>% describe()


# 1 最初の線形モデル ----------------------------------------------------------------------

# ＜ポイント＞
# - 元データを基準化せずに線形回帰モデルに適用
#   --- 線形回帰の前提を満たさないので、解釈しにくい結果が出てくる


# データ作成
model_df <-
  cust.df %>%
     select(-cust.id) %>%
     filter(online.spend > 0)

# モデル定義
# --- 基準化をしていない状態で線形回帰モデルを定義
spend.m1 <- lm(online.spend ~ ., data = model_df)

# データ確認
spend.m1 %>% summary()
spend.m1 %>% listviewer::reactjson(collapsed = TRUE)

# データ確認
# --- 回帰係数
# --- モデル統計量
spend.m1 %>% summary()
spend.m1 %>% glance()


# 2 データ加工 ---------------------------------------------------------------------------

# ＜ポイント＞
# - データの外観を確認して必要な前処理を適用する
#   --- Box-Cox変換：分布の正規化
#   --- Zスコア変換：数値の水準/分散の調整


# プロット作成
# --- 元データ(数値データのみ)
# --- 対数変換が必要そうなデータが散見される
cust.df %>%
  select_if(is.numeric) %>%
  scatterplotMatrix()

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
# --- 前処理適用後のデータを使用
spend.m2 <- lm(online.spend ~ ., data = cust.df.bc)

# データ確認
# --- online.transが圧倒的に有意
# --- 多くの回帰係数のp値が大きく「モデル間に違いがないという帰無仮説」を棄却できない
spend.m2 %>% tidy() %>% mutate_if(is.numeric, round, 5)
spend.m2 %>% glance()

# VIFの確認
# --- 10以上が複数で確認される（store.trans, store.spend）
spend.m2 %>% vif() %>% round(2)


# モデル定義3
# --- online.transのみでモデル化
spend.m3 <- lm(online.spend ~ online.trans, data = cust.df.bc)

# データ確認
# --- online.transの傾きがほとんど1（これだけで説明可能）
spend.m3 %>% tidy() %>% mutate_if(is.numeric, round, 5)
spend.m3 %>% glance()


# 分散分析
anova_result <- spend.m3 %>% anova(spend.m2)
anova_result %>% tidy()
anova_result %>% glance()


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

# ＜ポイント＞
# - データにおける多重共線性の程度はVIFにより判断することができる
#   --- 単回帰の場合と比べて線形モデルの回帰係数の標準誤差がどれだけ増加するか
#   --- VIFが10を超える変数を削除することで多重共線性を回避する


# VIFの確認
# --- VIFが10を超える変数を確認
spend.m2 %>% vif()
spend.m2 %>% vif() %>% .[. > 10] %>% names()

# モデル定義4
# --- VIFが10を超える2変数を削除
spend.m4 <- lm(online.spend ~ . -online.trans -store.trans, data = cust.df.bc)

# VIFの確認
# --- 10以上がなくなった
spend.m4 %>% vif()

# データ確認
spend.m4 %>% tidy()
spend.m4 %>% glance()


# 5 多重共線性への対処2：PCA変換 ------------------------------------------------------------------

# ＜ポイント＞
# - 特徴量をPCA変換すると主成分ごとの相関係数がゼロとなるため多重共線性は生じない
# - 元の特徴量を使わないので｢解釈面｣での利便性が損なわれる
#   --- 対応策として類似した特徴量のみにPCAを適用することで合成変量に変換する


# 相関係数の確認
# --- 高相関ペアであることを確認（PCA合成の対象）
cust.df.bc %>% select(online.visits, online.trans) %>% cor()
cust.df.bc %>% select(store.trans, store.spend) %>% cor()

# PCA合成
# --- 変数の性質に応じて名前を付けている
extract_pc1 <- function(df) df %>% prcomp() %>% use_series(x) %>% .[, 1]
online <- cust.df.bc %>% select(online.visits, online.trans) %>% extract_pc1()
store <- cust.df.bc %>% select(store.trans, store.spend) %>% extract_pc1()

# データ加工
# --- 高相関ペアのPC1を特徴量に追加
cust.df.bc.pc <-
  cust.df.bc %>%
    mutate(online = online,
           store  = store) %>%
    select(-online.visits, -online.trans, -store.trans, -store.spend)

# データ確認
# --- 高相関の変量がなくなっている
cust.df.bc.pc %>% glimpse()
cust.df.bc.pc %>% select(-online.spend) %>% select_if(is.numeric) %>% cor() %>% corPlot()

# モデル定義4
# --- 高相関ペアの説明変数を削除
# --- PCAで合成した説明変数を追加
spend.m5 <- lm(online.spend ~ ., data = cust.df.bc.pc)

# VIFの確認
# --- 最大で約1.5程度となった
spend.m5 %>% vif()

# データ確認
spend.m5 %>% tidy()
spend.m5 %>% glance()
