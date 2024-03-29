# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 11 セグメンテーション：クラスタリングと分類（メイン）
# Objective : クラスタリング（階層クラスタリング）
# Created on: 2022/04/05
# Page      : P376 - P386
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - セグメンテーションには明確な手順はなくタスクごとに視点は異なる
# - クラスタリングは｢教師なし学習｣のため明確な正解(ラベル)がないので、最終的に主観的な判断が必要となる
#   --- データを客観的に視覚化することでデータ理解や判断材料となる
#   --- 複数のアルゴリズムを使って結果比較するほうがよい


# ＜クラスタリング手法＞
# - 距離に基づくクラスタリング
#   --- 階層クラスタリング（ツリー構造：hclust）
#   --- 分割型クラスタリング（グループ重心：kmeans）
# - モデルに基づくクラスタリング
#   --- データを正規分布の混合としてモデル化（Mclust）
#   --- カテゴリカル変数の潜在クラスモデル（poLCA）


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 ユーグリッド距離の測定
# 3 カテゴリカル変数を含む距離測定
# 4 階層クラスタリングの実行
# 5 デンドログラムの剪定
# 6 階層クラスタリングのからのグループ取得
# 7 階層クラスタリングの結果解釈
# 8 プロットによるクラスタリング結果の解釈


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(cluster)
library(GGally)
library(psych)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("map", "purrr", quiet = TRUE)


# データ準備
# --- セグメンテーションデータ
seg.raw <- read_csv("data/seg_raw.csv")

# データ確認
seg.raw %>% print()
seg.raw %>% glimpse()


# 1 データ加工 ----------------------------------------------------------------------------

# ＜ポイント＞
# - 階層クラスタリングは数値データと順序データの両方を扱うことができる
#   --- カテゴリカルデータは順序データに変換すると扱うことができる
#   --- バイナリデータのように順序化する際の意味づけが明確にする必要がある


# データ加工
# --- ラベルデータであるSegmentを除外
seg.df <-
  seg.raw %>%
    select(-Segment) %>%
    mutate_if(is.character, as.ordered)

# データ確認
seg.df %>% print()
seg.df %>% glimpse()
seg.df %>% select_if(is.ordered) %>% map(table)

# レベル確認
# --- orderedデータには順序が定義されている
seg.df$gender
seg.df$subscribe

# データサマリー
seg.df %>% summary()

# 参考：順序データを含むデータの階層クラスタリング
seg.df %>% dist() %>% hclust(method = "complete") %>% plot()


# 2 ユーグリッド距離の測定 ---------------------------------------------------------------

# ＜ポイント＞
# - 階層クラスタリングは隣接するクラスタの距離を測定して要素を結合していく
#   --- 最終的に1つのクラスタになるまで結合を続ける
#   --- 要素間の距離を測定して結合する要素を決定する
# - 距離尺度は様々なものがあるが最も代表的なのにユーグリッド距離がある
#   --- 数値データの場合のみ定義することができる
#   --- gower距離はカテゴリカルデータを扱うことができる（次項）


# 距離の計算
# --- ベクトルの差分
# --- 差分の2乗 ⇒ 合計
# --- 差分の2乗 ⇒ 合計 ⇒ 平方根（ユーグリッド距離）
c(1, 2, 3) - c(2, 3, 2)
(c(1, 2, 3) - c(2, 3, 2))^2 %>% sum()
(c(1, 2, 3) - c(2, 3, 2))^2 %>% sum() %>% sqrt()

# 距離の計算
# --- dist()を使用
# --- 行の間の距離を定義している
c(1, 2, 3) %>%
  rbind(c(2, 3, 2)) %>%
  dist(method = "euclidean")

# 距離行列のイメージ
# --- 対角成分はゼロとなる
seg.df %>%
  select(age, income, kids) %>%
  dist() %>%
  as.matrix() %>%
  .[1:5, 1:5]


# 3 カテゴリカル変数を含む距離測定 --------------------------------------------------------

# ＜ポイント＞
# - カテゴリカル変数を含む場合はgower距離を用いて測定する（計算プロセスでは数値化が行われている）
#   --- gower距離はカテゴリカルデータを0/1に変換して距離を測定する（影響度をコントロールしたい場合はweight引数を用いる）
#   --- 距離行列が計算できれば階層クラスタリングが可能となる


# データ確認
# --- カテゴリカル関数もクラスタ定義に含めるべき
seg.df %>% print()

# 距離測定
# --- cluster::daisy()を使用すると数値データ以外でも計算可能
# --- 数値列以外が含まれるとmetric引数が自動的に"gower"を選択
seg.dist <- seg.df %>% daisy(metric = "gower")

# 距離行列のイメージ
seg.dist %>% as.matrix() %>% .[1:5, 1:5]


# 4 階層クラスタリングの実行 ------------------------------------------------------------

# ＜ポイント＞
# - 階層クラスタリングは距離行列(数値)を定義すると計算することができる
#   --- カテゴリカルデータを含む場合は距離行列の作成方法で対応する（gower距離）


# 階層クラスタリング
# --- method引数ではクラスタ間の距離測定の方法を指定
seg.hc <-
  seg.df %>%
    daisy(metric = "gower") %>%
    hclust(method = "complete")

# データ確認
seg.hc %>% print()
seg.hc %>% summary()
seg.hc %>% listviewer::reactjson(collapsed = TRUE)

# デンドログラム作成
seg.hc %>% plot()


# 5 デンドログラムの剪定 ----------------------------------------------------------------+

# ＜ポイント＞
# - 階層クラスタリングのカテゴリはデンドログラムをカットする位置に応じて決定される


# デンドログラム剪定
# --- デンドログラムをカット
# --- Y軸の距離の水準で出力部分を決定して複数できるクラスタはリスト番号で選択
seg.hc %>%
  as.dendrogram() %>%
  cut(h = 0.5) %>%
  use_series(lower) %>%
  .[[1]] %>%
  plot()

# データチェック
# --- No101/107は類似
# --- No278/294は類似
# --- No173/141は非類似
seg.df %>% slice(101, 107)
seg.df %>% slice(278, 294)
seg.df %>% slice(173, 141)

# 適合度チェック
# --- cophenetic相関係数（コーフェン行列から定義した相関係数）
# --- デンドログラムと距離行列の一致度を確認
seg.hc %>% cophenetic() %>% cor(seg.dist)


# 6 階層クラスタリングのからのグループ取得 ------------------------------------------------

# ＜ポイント＞
# - 階層クラスタリングはカットする位置でグループ数が決まる
#   --- kmeansのように事前にグループ数を指定することはできない
#   --- Hight(Y軸)の水準に意味づけがないと判断しにくい


# デンドログラム表示（再掲）
seg.hc %>% plot()

# グループ追加
# --- デンドログラムは任意の高さでカットできる
# --- カットに応じてグループが作成される
seg.hc %>% rect.hclust(k = 4, border = "red")

# グループ作成
# --- 指定したクラスタ数の位置でカット
seg.hc.segment <- seg.hc %>% cutree(k = 4)
seg.hc.segment %>% table()


# 7 階層クラスタリングの結果解釈 --------------------------------------------------------

# ＜ポイント＞
# - クラスタリングが適切に行われているかはグループごとの重心(平均値)などが適切に分離しているかで判断する
# - cluster::daisy()のgower距離はカテゴリカル変数を0-1の間に変換して距離行列を作成する
#   --- カテゴリカル変数(性別/購読状況/住宅所有)は2値のカテゴリなので強い影響力を持ってしまった


# 関数定義
# --- 平均集計（数値データのみ）
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

# データ集計
# --- クラスタごとの平均値
# --- グループ1は全て男性、グループ2は全て女性
# --- 自明の結果を示している（P384の文章で確認）
seg.df %>% seg.summ(seg.hc.segment)


# 8 プロットによるクラスタリング結果の解釈 --------------------------------------------------

# カテゴリのプロット
# --- genderとsubscribeできれいにデータが分かれている
# --- ｢潜在クラス｣ではなく｢自明のクラス｣だった
val_x <- seg.df$gender %>% as.numeric() %>% jitter()
val_y <- seg.df$subscribe %>% as.numeric() %>% jitter()
plot(val_x ~ val_y, col = seg.hc.segment, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(1, at = c(1, 2), labels = c("Subscribe: No", "Subscribe: Yes"))
axis(2, at = c(1, 2), labels = levels(seg.df$gender))

# 散布図行列
# --- クラスタリングが適切に分離しているかは散布図行列が視覚的に確認しやすい
# --- データ型ごとに並べる方が見やすい
seg.df %>%
  select(where(is.numeric), where(is.ordered)) %>%
  ggpairs()
