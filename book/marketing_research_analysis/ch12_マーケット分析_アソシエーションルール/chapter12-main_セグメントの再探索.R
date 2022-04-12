# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 12 マーケットバスケット分析とアソシエーションルール
# Theme     : 4 非トランザクションデータのルール（セグメントの再探索）
# Created on: 2021/04/13
# Page      : P447 - P452
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 消費者セグメンテーションを調べる方法としてアソシエーションルールを使用する


# ＜目次＞
# 0 準備
# 1 データ加工
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
seg.fac.raw %>% summary()


# 1 データ加工 --------------------------------------------------------------------------

# ＜ポイント＞
# - アソシエーションルールは離散データに対して動作するためデータ変換を行う


# データ加工
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
                      right = FALSE, ordered_result = TRUE))

# 確認
seg.fac %>% print()
seg.fac %>% summary()



# 2 トランザクションデータへの変換 ---------------------------------------------------------

# ＜ポイント＞
# - カテゴリカルデータのみのデータフレームはトランザクションデータに変換することができる


# トランザクションデータへの変換
seg.trans <- seg.fac %>% as( "transactions")

# 確認
seg.trans %>% print()
seg.trans %>% summary()
seg.trans %>% listviewer::reactjson(collapsed = TRUE)


# 3 アソシエーションルールの適用 -----------------------------------------------------------

# ＜ポイント＞
# - マーケットバスケットデータと同じ方法で検出する


# モデル構築
seg.rules <-
  seg.trans %>%
    apriori(parameter = list(support = 0.1, conf = 0.4, target = "rules"))

# 確認
seg.rules %>% summary()
seg.rules %>% listviewer::reactjson(collapsed = TRUE)


# 4 ルールの可視化 ------------------------------------------------------------------------

# プロット
# --- 全体像
seg.rules %>% plot(interactive = TRUE)

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
