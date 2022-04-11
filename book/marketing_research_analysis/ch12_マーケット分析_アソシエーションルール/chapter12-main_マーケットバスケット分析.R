# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 12 マーケットバスケット分析とアソシエーションルール
# Theme     : 2 マーケットバスケット分析
# Created on: 2021/04/12
# Page      : P428 - P446
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 サンプルデータでアソシエーション分析
# 2 スーパーマーケットデータの確認
# 3 データ加工と変換
# 4 アソシエーションルールの検出
# 5 ルールの可視化
# 6 ルールをネットワークで可視化
# 7 トランザクションデータの作成
# 8 トランザクションと利益マージンデータを組み合わせた分析


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
data("Groceries")
retail.raw <- readLines("data/retail.dat")


# 1 サンプルデータでアソシエーション分析 ------------------------------------------------------

# データ確認
# --- 独自のオブジェクト
# --- 9835個のトランザクションと169個のユニークアイテム
Groceries %>% class()
Groceries %>% summary()
Groceries %>% listviewer::reactjson(collapsed = TRUE)

# アソシエーションの表示
Groceries %>% head(3) %>% inspect()

# アソシエーション分析
groc.rules <-
  Groceries %>%
    apriori(parameter = list(supp = 0.01, conf = 0.3, target = "rules"))

# 確認
groc.rules %>% summary()
groc.rules %>% listviewer::reactjson(collapsed = TRUE)

# アソシエーション確認
# --- リフト値が高いパターンを選択
groc.rules %>% subset(lift > 3) %>% inspect()


# 2 スーパーマーケットデータの確認 -----------------------------------------------------------

# データ形式
retail.raw %>% class()
retail.raw %>% glimpse()

# データ確認
# --- アイテム集合ごとに""で区切られている
# --- アイテムはスペースで区切られている
retail.raw %>% head()
retail.raw %>% tail()
retail.raw %>% summary()


# 3 データ加工と変換 ----------------------------------------------------------------------------

# リスト変換
retail.list <-
  strsplit(retail.raw, " ") %>%
    set_names(str_c("Trans", seq_along(.)))

# データ構造
retail.list %>% glimpse()

# データ確認
retail.list %>% some()

# データ変換
retail.trans <- retail.list %>% as("transactions")
retail.trans %>% summary()
retail.trans %>% class()


# 4 アソシエーションルールの検出 ------------------------------------------------------------------

# モデル構築
retail.rules <-
  retail.trans %>%
    apriori(parameter = list(supp = 0.001, conf = 0.4))

# 確認
retail.rules %>% summary()
retail.rules %>% listviewer::reactjson(collapsed = TRUE)


# 5 ルールの可視化 -------------------------------------------------------------------------------

# ＜ポイント＞
# - ルール分布を理解する

# プロット作成
retail.rules %>% plot()

# プロット作成
# --- インタラクティブ
retail.rules %>% plot(interactive = TRUE)


# 6 ルールをネットワークで可視化 ------------------------------------------------------------------

# ルールの抽出
retail.hi <- retail.rules %>% sort(by = "lift") %>% head(50)

# アソシエーション確認
retail.hi %>% inspect()

# ルールの一部を可視化
retail.hi %>% plot(method = "graph", control = list(type = "items"))


# 7 トランザクションデータの作成 ---------------------------------------------------------

# 全アイテムの取得
retail.itemnames <- retail.trans %>% as( "list") %>% unlist() %>% unique() %>% sort()

# データ確認
retail.itemnames %>% head()
retail.itemnames %>% tail()

# マージンデータの作成
# --- 正規乱数で生成
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


# 8 トランザクションと利益マージンデータを組み合わせた分析 ------------------------------------------

retail.margsum <- function(items, itemMargins) {
  # Input: "items" == item names, rules or transactions in arules format
  #        "itemMargins", a data frame of profit margin indexed by name
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

retail.margsum(c("39", "48"), retail.margin)
retail.margsum(list(t1 = c("39", "45"), t2 = c("31", "32")), retail.margin)
retail.margsum(retail.trans[101:103], retail.margin)
retail.margsum(retail.hi, retail.margin)

retail.margsum(c("hello", "world"), retail.margin)  # error!
retail.margsum(list(a = c("39", "45"), b = c("hello", "world"), c = c("31", "32")),
               retail.margin)    # only the first and third are OK

