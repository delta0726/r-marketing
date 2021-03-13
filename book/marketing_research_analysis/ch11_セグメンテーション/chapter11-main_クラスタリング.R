# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 11 セグメンテーション：クラスタリングと分類（メイン）
# Objective : クラスタリング
# Created by: Owner
# Created on: 2021/02/22
# Page      : P371 - P400
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - クラスタリングは｢教師なし学習｣のため明確な正解(ラベル)がないので、最終的に主観的な判断が必要となる
#   --- データを客観的に視覚化することでデータ理解や判断材料となる
#   --- 複数のアルゴリズムを使って結果比較するほうがよい


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 ユーグリッド距離の測定
# 3 カテゴリカル変数を含む距離測定
# 4 階層クラスタリングの実行
# 5 階層クラスタリングのグループ化
# 6 階層クラスタリングの結果解釈
# 7 平均に基づくクラスタリング(k-means)
# 8 モデルベースクラスタリング
# 9 潜在クラス分析
# 10 潜在クラス分析の解の比較


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(cluster)
library(mclust)
library(poLCA)
library(psych)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("map", "purrr")

# 関数定義
# --- 平均集計（数値データのみ）
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


# データ準備
# --- セグメンテーションデータ
seg.raw <- read_csv("book/marketing_research_analysis/data/seg_raw.csv")


# 1 データ加工 ----------------------------------------------------------------------------

# カテゴリカル変数をそのまま使用 *******************************************

# ＜ポイント＞
# - 階層クラスタリングで使用


# データ加工
seg.df  <- seg.raw %>% select(-7) %>% mutate_if(is.character, as.ordered)

# データ確認
seg.df %>% print()
seg.df %>% glimpse()
seg.df %>% select_if(is.ordered) %>% map(table)

# データサマリー
seg.df %>% summary()


# カテゴリカル変数を数値に変換 *******************************************

# ＜ポイント＞
# - k-meansとモデルベースクラスタリングで使用


# データ加工
# --- カテゴリカル変数を数値に変換
# --- 2値カテゴリを0/1に変換するのが最適というわけではない
seg.df.num <-
  seg.df %>%
    mutate(gender = ifelse(seg.df$gender=="Male", 0, 1),
           ownHome = ifelse(seg.df$ownHome=="ownNo", 0, 1),
           subscribe = ifelse(seg.df$subscribe=="subNo", 0, 1))

# データ確認
seg.df.num %>% print()
seg.df.num %>% glimpse()

# データサマリー
seg.df.num %>% summary()


# 数値をカテゴリカル変数に変換 *******************************************

# データ加工
# --- 数値をカテゴリカル変数に変換
seg.df.cut <-
  seg.df %>%
    mutate(age = factor(ifelse(seg.df$age < median(seg.df$age), 1, 2)),
           income = factor(ifelse(seg.df$income < median(seg.df$income), 1, 2)),
           kids = factor(ifelse(seg.df$kids < median(seg.df$kids), 1, 2)))

# データ確認
seg.df.cut %>% print()
seg.df.cut %>% glimpse()

# データサマリー
seg.df.cut %>% summary()


# 2 ユーグリッド距離の測定 ---------------------------------------------------------------

# ＜ポイント＞

# 距離の計算
# --- ベクトルの差分
# --- 差分の2乗⇒合計
# --- 差分の2乗⇒合計⇒平方根（ユーグリッド距離）
c(1, 2, 3) - c(2, 3, 2)
(c(1, 2, 3) - c(2, 3, 2)) ^ 2 %>% sum()
(c(1, 2, 3) - c(2, 3, 2)) ^ 2 %>% sum() %>% sqrt()

# 距離の計算
# --- dist()を使用
c(1, 2, 3) %>% rbind(c(2, 3, 2)) %>% dist(method = "euclidean")

# 距離行列の作成
# --- イメージ
seg.df %>%
  select(age, income, kids) %>%
  dist() %>%
  as.matrix() %>%
  .[1:5, 1:5]


# 3 カテゴリカル変数を含む距離測定 --------------------------------------------------------

# ＜ポイント＞
# - カテゴリカル変数を含む場合はgower距離を用いて測定する
#   --- cluster::daisy()で計算可能
#   --- 距離行列が計算できれば階層クラスタリングは可能


# データ確認
# --- カテゴリカル関数もクラスタ定義に含めるべき
seg.df %>% print()

# 距離測定
# --- cluster::daisy()を使用すると数値データ以外でも計算可能
# --- 数値列以外が含まれるとmetric引数が自動的に"gower"を選択
seg.dist <- seg.df %>% daisy()

# 距離行列の作成
# --- イメージ
seg.dist %>% as.matrix() %>% .[1:5, 1:5]


# 4 階層クラスタリングの実行 ------------------------------------------------------------

# 階層クラスタリング
# --- method引数ではクラスタ間の距離測定の方法を指定
seg.hc <-
  seg.dist %>%
    hclust(method = "complete")

# デンドログラム作成
seg.hc %>% plot()

# デンドログラム剪定
# --- デンドログラムをカット
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
# --- cophenetic相関係数
# --- デンドログラムと距離行列の一致度を確認
seg.hc %>% cophenetic() %>% cor(seg.dist)


# 5 階層クラスタリングのグループ化 --------------------------------------------------------

# デンドログラム表示
seg.hc %>% plot()

# グループ追加
# --- デンドログラムは任意の高さでカットできる
# --- カットに応じてグループが作成される
seg.hc %>% rect.hclust(k = 4, border = "red")

# グループ作成
seg.hc.segment <- seg.hc %>% cutree(k = 4)
seg.hc.segment %>% table()


# 6 階層クラスタリングの結果解釈 --------------------------------------------------------

# ＜ポイント＞
# - cluster::daisy()は変数を0-1の間に変換して距離行列を作成する
#   --- カテゴリカル変数(性別/購読状況/住宅所有)は2値のカテゴリなので強い影響力を持ってしまった


# データ集計
# --- クラスタごとの平均値
# --- グループ1は全て男性、グループ2は全て女性
# --- 自明の結果を示している（P384の文章で確認）
seg.df %>% seg.summ(seg.hc.segment)

# カテゴリのプロット
# --- genderとsubscribeできれいにデータが分かれている
# --- ｢潜在クラス｣ではなく｢自明のクラス｣だった
val_x <- seg.df$gender %>% as.numeric() %>% jitter()
val_y <- seg.df$subscribe %>% as.numeric() %>% jitter()
plot(val_x ~ val_y, col = seg.hc.segment, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(1, at = c(1, 2), labels = c("Subscribe: No", "Subscribe: Yes"))
axis(2, at = c(1, 2), labels = levels(seg.df$gender))


# 7 平均に基づくクラスタリング(k-means) -------------------------------------------------

# ＜ポイント＞
# - k-means法は分類した各グループの重心とメンバーとの距離が最も小さくなるようにグループを定める手法
#   --- 距離はユーグリッド距離によって定義される
#   --- クラスタ数を分析者が指定する必要がある（結果を主観的に判断して最終決定）

# k-meansの実行
set.seed(96743)
seg.k <- seg.df.num %>% kmeans(centers = 4)

# データ集計
# --- カテゴリごとの差異が大きくなっている（主観）
seg.df %>% seg.summ(seg.k$cluster)

# ボックスプロット作成
# --- カテゴリごとのIncome(収入)
boxplot(seg.df.num$income ~ seg.k$cluster, ylab = "Income", xlab = "Cluster")

# プロット作成
# --- PC1/PC2で2次元にマッピング
# --- 階層クラスタリングより興味深い結果
seg.df %>%
  clusplot(seg.k$cluster, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "K-means cluster plot")


# 8 モデルベースクラスタリング --------------------------------------------------------------

# ＜ポイント＞
# - モデルベースクラスタリングでは、各観測値は異なる統計分布を持つグループから作成されたと仮定する
#   --- 観測されたデータを説明するのに最も適した分布の集合を見つける
#   --- mclust::Mclust()では正規分布の混合から観測値が抽出されたと仮定する

# 学習1
# --- 初期設定ではクラスタ数はモデルが探索
seg.mc <- seg.df.num %>% Mclust()
seg.mc %>% summary()

# 学習2
# --- クラスタ数を指定することも可能
# --- 学習1のクラスタ1が2つに分離した
seg.mc4 <- seg.df.num %>% Mclust(G = 4)
seg.mc4 %>% summary()

# ベイズ情報量基準(BIC)による比較
# --- BICが低いほどモデルとして優れている（0に近いほど優れている）
# --- BICはマイナスとなることもある（大きいほど優れている）
BIC(seg.mc, seg.mc4)

# データ集計
seg.df %>% seg.summ(seg.mc$class)

# 探索的な分析
seg.df.num %>% mclustBIC()

# プロット作成
seg.df %>%
  clusplot(seg.mc$class, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "Model-based cluster plot")


# 9 潜在クラス分析 -------------------------------------------------------------------


# フォーミュラ設定
seg.f <- seg.df.cut %>% with(cbind(age, gender, income, kids, ownHome, subscribe)~1)
seg.f

# モデル構築
set.seed(02807)
seg.LCA3 <- seg.f %>% poLCA(data = seg.df.cut, nclass = 3)
seg.LCA4 <- seg.f %>% poLCA(data = seg.df.cut, nclass = 4)

# ベイズ情報量基準(BIC)の確認
seg.LCA4$bic
seg.LCA3$bic

# データ集計
seg.df %>% seg.summ(seg.LCA3$predclass)
seg.df %>% seg.summ(seg.LCA4$predclass)

# カテゴリごとのサンプル数を確認
seg.LCA3$predclass %>% table()
seg.LCA4$predclass %>% table()

# プロット
seg.df %>%
  clusplot(seg.LCA3$predclass, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "LCA plot (K=3)")

seg.df %>%
  clusplot(seg.LCA4$predclass, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "LCA plot (K=4)")


# 10 潜在クラス分析の解の比較 -------------------------------------------------------------------

# 分割表の作成
# --- 見やすいが主観的判断となる
table(seg.LCA3$predclass, seg.LCA4$predclass)

# 定量的な比較
# --- モデル結果
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)
seg.LCA3$predclass %>% adjustedRandIndex(seg.LCA4$predclass)

# 定量的な比較
# --- ランダムデータ（ベンチマーク）
set.seed(11021)
random.data <- sample(4, length(seg.LCA4$predclass), replace=TRUE)
random.data %>% adjustedRandIndex(seg.LCA4$predclass)


# compare to known segments
table(seg.raw$Segment, seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass)
