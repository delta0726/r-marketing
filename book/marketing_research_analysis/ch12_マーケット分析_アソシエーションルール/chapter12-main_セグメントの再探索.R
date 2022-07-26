# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 12 マーケットバスケット分析とアソシエーションルール
# Theme     : 4 非トランザクションデータのルール（セグメントの再探索）
# Created on: 2022/07/27
# Page      : P447 - P452
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - アソシエーションルールはデータフレーム形式のデータにも適用することができる
#   --- 消費者セグメンテーションを調べる方法としてアソシエーションルールを使用する


# ＜目次＞
# 0 準備
# 1 連続データの離散化
# 2 トランザクションデータへの変換
# 3 アソシエーションルールの適用
# 4 ルールの可視化


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(arules)
library(arulesViz)
library(arulesSequences)
library(car)
library(conflicted)

conflict_prefer("some", "car", quiet = TRUE)


# データロード
seg.fac.raw <- read_csv("data/seg_df.csv")

# 確認
seg.fac.raw %>% print()
seg.fac.raw %>% glimpse()


# 1 連続データの離散化 ----------------------------------------------------------------------

# ＜ポイント＞
# - アソシエーションルールは離散データに対して動作するため、連続データの離散化を行う


# サマリー
# --- 連続データを含む列が存在する
seg.fac.raw %>% summary()

# 連続データの離散化
seg.fac <-
  seg.fac.raw %>%
    mutate(age = cut(age,
                     breaks = c(0, 25, 35, 55, 65, 100),
                     labels = c("19-24", "25-34", "35-54", "55-64", "65+"),
                     right = FALSE, ordered_result = TRUE),
           income = cut(income,
                        breaks = c(-100000, 40000, 70000, 1000000),
                        labels = c("Low", "Medium", "High"),
                        right = FALSE, ordered_result = TRUE),
           kids = cut(kids,
                      breaks = c(0, 1, 2, 3, 100),
                      labels = c("No kids", "1 kid", "2 kids", "3+ kids"),
                      right = FALSE, ordered_result = TRUE)) %>%
    mutate_if(is.character, as.factor)

# 確認
seg.fac %>% print()
seg.fac %>% glimpse()

# サマリー
seg.fac %>% summary()


# 2 トランザクションデータへの変換 ---------------------------------------------------------

# ＜ポイント＞
# - カテゴリカルデータのみのデータフレームはトランザクションデータに変換することができる
#   --- レコード数は｢transaction｣に対応
#   --- 総カテゴリ数は｢items｣に対応


# データ全体のカテゴリ数
# --- レコード数：300
# --- 総カテゴリ数：22
seg.fac %>% nrow()
seg.fac %>% unlist() %>% unique() %>% length()

# トランザクションデータへの変換
# --- 300 transactions (rows) and
# --- 22 items (columns)
seg.trans <- seg.fac %>% as( "transactions")

# 確認
seg.trans %>% print()
seg.trans %>% summary()

# オブジェクト構造
seg.trans %>% listviewer::reactjson(collapsed = TRUE)


# 3 アソシエーションルールの適用 -----------------------------------------------------------

# ＜ポイント＞
# - マーケットバスケットデータと同じ方法で検出する


# モデル構築
seg.rules <-
  seg.trans %>%
    apriori(parameter = list(support = 0.1, conf = 0.4, target = "rules"))

# 確認
seg.rules %>% print()
seg.rules %>% summary()

# オブジェクト構造
seg.rules %>% listviewer::reactjson(collapsed = TRUE)


# 4 ルールの可視化 ------------------------------------------------------------------------

# プロット
# --- 全体像
seg.rules %>% plot(interactive = TRUE, engine = "plotly")

# 上位ルールの抽出
seg.hi <- seg.rules %>% sort(by = "lift") %>% head(35)
seg.hi %>% inspect()

# プロット
# --- 上位ルールの可視化
seg.hi %>%
  plot(method = "graph", control = list(type = "items"))


# 次点ルールの抽出
seg.next <- seg.rules %>% sort(by = "lift") %>% .[36:60]

# プロット
# --- 次点ルールの可視化
seg.next %>%
  plot(method = "graph", control = list(type = "items"))


# 特定ルールの抽出
seg.sub <-
  seg.rules %>%
    subset(subset = (rhs %pin% "Urban" | rhs %pin% "subscribe") & lift > 1) %>%
    sort(by = "lift") %>%
    head(100)

# サマリー
seg.sub %>% summary()

# プロット作成
seg.sub %>% plot(method = "grouped")
