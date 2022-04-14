# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 14 行動シーケンス
# Theme     : 4 行動遷移に対するマルコフ連鎖
# Created on: 2022/4/15
# Page      : P526 - P537
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - マルコフ連鎖を使って行動状態間の遷移をモデル化する
#   --- マルコフ連鎖(MC)は解釈可能な行動状態の遷移メカニズムと考えられる
#   --- マルコフ連鎖モンテカルロ(MCMC)は別の目的で使われる計算プロセス（今回の対象ではない）


# ＜目次＞
# 0 準備
# 1 行動遷移の考え方
# 2 データ加工
# 3 モデルデータの作成1
# 4 モデルデータの作成2
# 5 モデルデータの作成3
# 6 マルコフ連鎖の推定
# 7 マルコフ連鎖の可視化
# 8 より高次の連鎖と予測


# 0 準備 -------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(scales)
library(expm)
library(car)
library(clickstream)
library(superheat)
library(conflicted)

conflict_prefer("filter", "dplyr", quiet = TRUE)


# データロード
epa.df <- read_csv("data/epa_df.csv")
epa.ordered <- read_csv("data/epa_ordered.csv")

# データ確認
epa.df %>% print()
epa.ordered %>% print()

# データ概要
epa.df %>% glimpse()
epa.ordered %>% glimpse()


# 1 行動遷移の考え方  ----------------------------------------------------

# ＜ポイント＞
# - マルコフ連鎖の性質の1つとして定常性が挙げられる
#   --- 長期の予測が複数ステップ後に安定してきて、初期状態に依存しないものとなる


# 初期状態
# --- ユーザーの70％はページ1、20％はページ2、10％はページ3から始まる
p.start <- c(0.7, 0.2, 0.1)

# 遷移確率（ステップ1）
# --- 転置行列で表記
p.trans <- matrix(c(0.1, 0.5, 0.2,
                    0.6, 0.4, 0.8,
                    0.3, 0.1, 0.0), nrow = 3, byrow = TRUE)

# 状態確率
# --- ステップ1の終了後
p.trans %*% p.start

# 状態確率
# --- 手動計算
0.7 * 0.1 + 0.2 * 0.5 + 0.1 * 0.2
0.7 * 0.6 + 0.2 * 0.4 + 0.1 * 0.8
0.7 * 0.3 + 0.2 * 0.1 + 0.1 * 0.0

# 状態確率
# --- ステップ2の終了後（遷移確率を2かける）
p.trans %*% p.trans %*% p.start

# 遷移確率を100ステップ適用
# --- 回数を重ねると定常状態となる
p.trans %^% 100

# 定常性の確認
# --- 全く異なる初期状態でも回数を重ねると定常状態に収斂する
p.trans %^% 100 %*% c(1, 0, 0)
p.trans %^% 100 %*% c(0.333, 0.334, 0.333)


# 2 データ加工 ----------------------------------------------------------

# ＜ポイント＞
# - 全パターンの遷移行列の作成は計算も解釈も困難であるため上位パターンにフォーカスして分析する


# 上位ページの抽出
top.pages <-
  epa.df %>%
    filter(pagetype == "html") %>%
    use_series(page) %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    head(20) %>%
    names()

# レコード抽出
epa.html <-
  epa.ordered %>%
    filter(pagetype == "html" & page %in% top.pages)


# 3 モデルデータの作成1 ----------------------------------------------------------

# ＜ポイント＞
# - 全パターンの遷移行列の作成は計算も解釈も困難であるため上位パターンにフォーカスして分析する
# - {clickstream}で分析するためのデータ形式に変換する


# セッション単位に分割
# --- sessionごとにリスト化
epa.session <- epa.html %>% split(epa.html$session)

# セッション抽出
# --- 2レコード抽出以上のセッションを抽出
epa.stream.len <- epa.session %>% map(nrow)
epa.session <- epa.session[epa.stream.len > 1]

# データ確認
epa.session %>% head() %>% str()
epa.session %>% length()


# 4 モデルデータの作成2 ----------------------------------------------------------

# ＜ポイント＞
# - シーケンス分析では終了状態かどうかをどうやって認定するかを決めなければならない


# 関数定義
define_end_point <- function(x){
  paste0(unique(x$host), ",",
         paste0(unlist(x$page), collapse = ","),
         ",END")
}

# データ変換
epa.stream <-
  epa.session %>%
    lapply(define_end_point) %>%
    unlist() %>%
    gsub("/", "ii", .) %>%
    gsub(".html", "", ., fixed = TRUE)

# データ確認
epa.stream %>% head()


# 5 モデルデータの作成3 ----------------------------------------------------------

# ＜ポイント＞
# - 一時ファイルを作ってから読込んでいる
#   --- 見慣れない操作方法だがreadClickstreams()のヘルプも同様の方法を用いている


# モデルデータの作成
click.tempfile <- tempfile()
epa.stream %>% writeLines(click.tempfile)
epa.trans <- click.tempfile %>% readClickstreams(header = TRUE)

# データ確認
epa.stream %>% head(2)
epa.trans %>% head(2)
epa.stream %>% tail(2)
epa.trans %>% tail(2)

# 頻度行列の確認
epa.trans %>% frequencies() %>% .[1:20] %>% head(10)


# 6 マルコフ連鎖の推定 ---------------------------------------------------------------------

# モデル構築
epa.mc <- epa.trans %>% fitMarkovChain(order = 1)

# 確認
# --- 遷移行列
# --- オブジェクト構成
epa.mc@transitions
epa.mc %>% listviewer::reactjson(collapsed = TRUE)


# 7 マルコフ連鎖の可視化 --------------------------------------------------------------------

# d. visualize it, heatmap
epa.mc.mat <-
  epa.mc@transitions[[1]] %>%
    t() %>%
    set_rownames(gsub("ii", "/", rownames(.))) %>%
    set_colnames(gsub("ii", "/", colnames(.)))

# ヒートマップの作成
set.seed(70510)
epa.mc.mat[-1,] %>%
  superheat(bottom.label.size = 0.4,
            bottom.label.text.size = 3.5,
            bottom.label.text.angle = 270,
            left.label.size = 0.3,
            left.label.text.size = 4,
            heat.col.scheme = "red",
            n.clusters.rows = 5, n.clusters.cols = 5,
            left.label = "variable", bottom.label = "variable",
            title = "Transitions, in sequences of top 20 pages (Row-to-Col)")


# グラフの作成
set.seed(59911)
epa.mc %>% plot(minProbability = 0.25)

# グラフの作成
# --- スターレイアウト
epa.mc %>% plot(minProbability = 0.25, layout = layout_as_star)


# 8 より高次の連鎖と予測 -----------------------------------------------------------------

# モデル作成
epa.trans.ge3 <- epa.trans[lapply(epa.trans, length) >= 4]
epa.mc2 <- epa.trans.ge3 %>% fitMarkovChain(order = 2)

# 予測
epa.trans[160]
epa.ex <- new("Pattern", sequence = head(unlist(epa.trans[160]), -1)) # example observation
epa.mc2 %>% predict(epa.ex, dist = 1)
epa.mc2 %>% predict(epa.ex, dist = 4)
