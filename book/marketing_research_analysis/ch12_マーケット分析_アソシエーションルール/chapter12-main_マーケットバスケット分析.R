# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 12 マーケットバスケット分析とアソシエーションルール
# Theme     : 2 マーケットバスケット分析
# Created on: 2022/07/26
# Page      : P428 - P446
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 1では{arules}に含まれるスーパーマーケットのトランザクションの小規模データセットを使う
#   --- データセットの構造は意識せず、分析プロセスと出力結果のイメージを確認する
# - 2以降では実際のトランザクションデータを用いて分析を行う


# ＜目次＞
# 0 準備
# 1 サンプルデータでアソシエーション分析
# 2 スーパーマーケットデータの確認
# 3 データ加工と変換
# 4 アソシエーションルールの検出
# 5 ルールの可視化
# 6 ルールをネットワークで可視化
# 7 マージンデータの作成
# 8 関数定義：マージンを調べる関数
# 9 トランザクションと利益マージンデータを組み合わせた分析


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(arules)
library(arulesViz)
library(car)
library(conflicted)

conflict_prefer("some", "car", quiet = TRUE)


# データロード
# --- Groceries：transactionsという独自オブジェクトのサンプルデータ
# --- retail.raw：行ごとに複数の購入商品が列挙されているテキストデータ
data("Groceries")
retail.raw <- readLines("data/retail.dat")


# 1 サンプルデータでアソシエーション分析 ------------------------------------------------------

# ＜ポイント＞
# - サンプルデータであるGroceriesを用いてアソシエーション分析を行う


# データ確認
# --- transactionsという独自オブジェクト
# --- 9835個のトランザクションと169個のユニークアイテム
Groceries %>% class()
Groceries %>% summary()
Groceries %>% listviewer::reactjson(collapsed = TRUE)

# アソシエーションの表示
Groceries %>% head(3) %>% inspect()

# アソシエーション分析
# --- 支持度(supp)がトランザクションの1％以上
# --- 確信度(conf)が0.3以上
groc.rules <-
  Groceries %>%
    apriori(parameter = list(supp = 0.01, conf = 0.3, target = "rules"))

# 確認
groc.rules %>% print()
groc.rules %>% summary()
groc.rules %>% listviewer::reactjson(collapsed = TRUE)

# アソシエーション確認
# --- リフト値が高いパターンを選択
groc.rules %>% subset(lift > 3) %>% inspect()


# 2 スーパーマーケットデータの確認 -----------------------------------------------------------

# ＜ポイント＞
# - トランザクションデータは購買者ごとの購入商品の組み合わせのデータセット
# - 以下のようなイメージ
#   --- Aさん{石鹸、水、ガム}
#   --- Bさん{クッキー、おむつ}
#   --- Cさん{缶詰、刺身、ビール、食パン}


# データ形式
# --- データの塊ごとに""で区切られている(表示形式によっては改行されている)
retail.raw %>% head()
retail.raw %>% class()
retail.raw %>% glimpse()

# データ確認
# --- アイテム集合ごとに""で区切られている
# --- アイテムはスペースで区切られている
retail.raw %>% head()
retail.raw %>% tail()
retail.raw %>% summary()


# 3 データ加工と変換 ----------------------------------------------------------------------------

# ＜ポイント＞
# - apriori()を適用するためtransactionsオブジェクトを作成する
#   --- 名前付きリストから作成


# リスト変換
# --- 取引ごとにリスト化して名前を付ける
retail.list <-
  retail.raw %>%
    strsplit(" ") %>%
    set_names(str_c("Trans", seq_along(.)))

# データ構造
retail.list %>% glimpse()

# データ確認
# --- some()はランダムに指定数のサンプルを取得する
retail.list %>% some()

# データ変換
# --- transactionsオブジェクトはリスト型から作成する
retail.trans <- retail.list %>% as("transactions")
retail.trans %>% class()

# サマリー
# --- 88162 rows (elements/itemsets/transactions) and 16470 columns (items)
# --- density of 0.0006257289 （ほとんどのアイテムはトランザクションで同時に買われていない）
retail.trans %>% summary()


# 4 アソシエーションルールの検出 ------------------------------------------------------------------

# ＜ポイント＞
# - transactionsオブジェクトからappriori()でルールを検出
#   --- パイパーパラメータは支持度(supp)と確信度(conf)の2つ


# モデル構築
retail.rules <-
  retail.trans %>%
    apriori(parameter = list(supp = 0.001, conf = 0.4))

# 確認
# --- 支持度(supp)と確信度(conf)から5944のルールを検出
retail.rules %>% summary()

# オブジェクト構造
retail.rules %>% listviewer::reactjson(collapsed = TRUE)


# 5 ルールの可視化 -------------------------------------------------------------------------------

# ＜ポイント＞
# - 支持度(support)と確信度(confidence)の2軸で散布図を作成する
# - 固定チャートではインプリケーションが少ないが、インタラクティブだと得れる情報も多くなる


# プロット作成
# --- ほどんどのルールはあまり頻繁に発生しない（支持度が低い）
# --- 確信度は比較的広範囲に分布している（0.4以上）
retail.rules %>% plot()

# プロット作成
# --- インタラクティブ
retail.rules %>% plot(interactive = TRUE, engine = "plotly")


# 6 ルールをネットワークで可視化 ------------------------------------------------------------------

# ＜ポイント＞
# - ルール自体を可視化する際にはネットワークを用いる


# ルールの抽出
# --- マーケットバスケット分析の目標は高いリフト値を持つルールを検出すること
# --- 単純に大きな数値を抽出すれば確認できる
retail.hi <- retail.rules %>% sort(by = "lift") %>% head(50)

# アソシエーション確認
retail.hi %>% inspect()

# アソシエーションのテーブル表示
retail.hi %>% inspectDT()

# ルールの一部を可視化
# --- ネットワークでルールを表示すると、より高いレベルでテーマやパターンを探すことができる
retail.hi %>% plot(method = "graph", control = list(type = "items"))


# 7 マージンデータの作成 ----------------------------------------------------------------

# ＜ポイント＞
# - ルールに対応する収益性を検証するためのデータセットを作成する
#   --- マージンデータ


# 全アイテムの取得
retail.itemnames <-
  retail.trans %>%
    as( "list") %>%
    unlist() %>%
    unique() %>%
    sort()

# データ確認
retail.itemnames %>% head()
retail.itemnames %>% tail()

# マージンデータの作成
# --- 全アイテムに対して正規乱数で生成
set.seed(03870)
retail.margin <-
  data.frame(margin = rnorm(length(retail.itemnames), mean = 0.30, sd = 0.30)) %>%
    set_rownames(retail.itemnames)

# データ確認
retail.margin %>% head(10)
retail.margin$margin %>% quantile()
retail.margin %>% some()

# マージンの確認
# --- 収益データなので合計することができる
retail.margin[c("39", "48"),]
retail.margin[c("39", "48"),] %>% sum()

# トランザクション抽出のマージン確認
basket.items <- retail.trans[3] %>% as("list") %>% .[[1]]
retail.margin[basket.items,]
sum(retail.margin[basket.items,])


# 8 関数定義：マージンを調べる関数 --------------------------------------------------------------

# ＜ポイント＞
# -


# ＜ステップ＞
# - 1.与えられたデータをアイテム名集合のリストに変換
# - 2.これらのアイテム名がマージンデータに含まれていることを確認
# - 3.マージンを取り出して合計を計算


retail.margsum <- function(itemMargins, items) {
  # Input: "itemMargins", a data frame of profit margin indexed by name
  #        "items" == item names, rules or transactions in arules format
  # Output: look up the item margins, and return the sum
  library(arules)

  # check the class of "items" and coerce appropriately to an item list
  if (class(items) == "rules") {
    tmp.items <- as(items(items), "list")       # rules ==> item list
  } else if (class(items) == "transactions") {
    tmp.items <- as(items, "list")              # transactions ==> item list
  } else if (class(items) == "list") {
    tmp.items <- items                          # it's already an item list!
  } else if (class(items) == "character") {
    tmp.items <- list(items)                    # characters ==> item list
  } else {
    stop("Don't know how to handle margin for class ", class(items))
  }

  # make sure the items we found are all present in itemMargins
  good.items <- unlist(lapply(tmp.items, function(x)
    all(unlist(x) %in% rownames(itemMargins))))

  if (!all(good.items)) {
    warning("Some items not found in rownames of itemMargins. ",
            "Lookup failed for element(s):\n",
            which(!good.items), "\nReturning only good values.")
    tmp.items <- tmp.items[good.items]
  }

  # and add them up
  return(unlist(lapply(tmp.items, function(x) sum(itemMargins[x,]))))
}


# 9 トランザクションと利益マージンデータを組み合わせた分析 ------------------------------------------

# 実行例
retail.margin %>% retail.margsum(items = c("39", "48"))
retail.margin %>% retail.margsum(items = list(t1 = c("39", "45"), t2 = c("31", "32")))
retail.margin %>% retail.margsum(items = retail.trans[101:103])
retail.margin %>% retail.margsum(items = retail.hi)

# エラー事例
# retail.margin %>% retail.margsum(c("hello", "world"))  # error!

# ワーニング事例
# --- only the first and third are OK
retail.margin %>%
  retail.margsum(list(a = c("39", "45"), b = c("hello", "world"), c = c("31", "32")))
