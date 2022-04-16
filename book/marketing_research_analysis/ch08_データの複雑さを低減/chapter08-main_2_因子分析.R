# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 8 データの複雑さを低減する（因子分析）
# Created on: 2021/03/31
# Page      : P254 - P265
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 因子分析はデータに占める潜在因子数を予め決定して、その因子の説明力が高まるように因子を抽出する
# - 因子の抽出の際には、因子間の関係性の定義によって回転の方法を変える
# - 因子数を抽出する段階で元のカテゴリとの関係性を確認することで因子の解釈性が高まる


# ＜主成分分析との違い＞
# - 主成分分析は観測データの座標変換を目的としている（各成分は直交している）
# - 探索的因子分析は顕在的な変数によってできるだけ解釈可能な解を見つけようとする）
#   --- いずれも多くの変数を少ない因子(成分)で説明しようとしている
#   --- 因子分析は少数の因子負荷量が非常に高い値を持つ一方、その他の負荷量が低くなるような解を求めようとする


# ＜目次＞
# 0 準備
# 1 因子数の決定
# 2 因子分析の解を求める
# 3 因子の解釈
# 4 因子軸の回転
# 5 SEMプロットによる解釈
# 6 ブランドの因子得点


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(conflicted)
library(nFactors)
library(GPArotation)
library(listviewer)
library(gplots)
library(RColorBrewer)
library(semPlot)


# コンフリクト解消
conflict_prefer("select", "dplyr")


# データ準備
brand.ratings <- read_csv("data/brand.rating.csv")

# データ変換
# --- Zスコア変換
brand.sc <-
  brand.ratings %>%
    mutate_if(is.numeric, function(x) (x - mean(x)) / sd(x)) %>%
    as_tibble()


# 1 因子数の決定 -------------------------------------------------------------

# ＜ポイント＞
# - 因子分析の最初のステップでは推定する因子数を決定する
#   --- スクリープロット
#   --- 固有値の大きさが1以上
#      （固有値は説明された分散量を意味する）
#      （固有値｢1｣は単独の独立変数に起因する分散量であり、それを下回ると情報量が少ないと判断）


# スクリーテスト
# --- 探索的主成分分析または因子分析で保持する成分または因子の数の分析を返す
# --- 固有値が1より大きい値を因子数とする（固有値が1を超えると元の変数よりも情報を持つことを意味する）
brand.sc %>%
  select_if(is.numeric) %>%
  cor() %>%
  eigen() %>%
  use_series(values) %>%
  nScree()

# 因子数のイメージ確認
# --- 固有値の水準から求める
# --- 1以上が有効な因子
brand.sc %>%
  select_if(is.numeric) %>%
  cor() %>%
  eigen() %>%
  use_series(values)


# 2 因子分析の解を求める -------------------------------------------------------------

# ＜ポイント＞
# - 因子分析は複数パターンを比較して有せきと思われるものを選択する
#   --- クフ数パターンで因子数を指定して実行する
# - 因子負荷量は最も重要な情報で、因子ごとの大きいカテゴリに注目する
#   ---2因子の場合はvalueやleader


# 因子分析の実行
# --- factanal関数はデフォルトでバリマックス回転で相関のない因子を求める
brand.fa.2 <- brand.sc %>% select_if(is.numeric) %>% factanal(factors = 2)
brand.fa.3 <- brand.sc %>% select_if(is.numeric) %>% factanal(factors = 3)

# 確認
brand.fa.2 %>% print()
brand.fa.3 %>% print()

# 因子負荷量
brand.fa.2$loadings %>% print()
brand.fa.3$loadings %>% print()


# 3 因子の解釈 -------------------------------------------------------------

# ＜ポイント＞
# - 出力された因子がどのような意味を持つのかは事前には分からないので解釈が必要
#   --- 因子負荷量が大きい特徴量から解釈する


# ヒートマップの作成
# --- 2因子
brand.fa.2$loadings %>%
  heatmap.2(col = brewer.pal(9, "Greens"), trace="none", key=FALSE,
            dend="none", Colv=FALSE, cexCol = 1.2,
            main="\n\n\n\n\nFactor loadings for brand adjectives")

# ヒートマップの作成
# --- 3因子
brand.fa.3$loadings %>%
  heatmap.2(col = brewer.pal(9, "Greens"), trace="none", key=FALSE,
            dend="none", Colv=FALSE, cexCol = 1.2,
            main="\n\n\n\n\nFactor loadings for brand adjectives")


# 4 因子軸の回転 -------------------------------------------------------------------------

# ＜ポイント＞
# - 因子モデルでは同じ比率で分散を説明する別の因子負荷量を持つように因子軸を回転させることができる
# - 回転は各因子間に｢相関｣についての仮定の置き方によって決める
#   --- 因子間の相関なので指定することが可能（データから自然に決まるものではない）
#   --- デフォルトではバリマックス回転で相関のない因子を求めている
#   --- 回転方法を変換することでLoadingsが大きい特徴量が出てくることを期待する


# 因子分析の実行
# --- factanal関数はデフォルトでバリマックス回転で相関のない因子を求める
# --- オブリミン回転は各因子間で因子負荷(の2乗)の共分散が最小になるような形で因子の回転
# --- valueとleaderは相関を持つことが想定されるので、無相関を想定するバリマックス回転は不適切
brand.fa.ob <-
  brand.sc %>%
    select_if(is.numeric) %>%
    factanal(factors = 3, rotation = "oblimin")

# ヒートマップの作成
# --- 色の濃い領域が各ファクターで出現した（相関を認める方が良いことを示唆）
brand.fa.ob$loadings %>%
  heatmap.2(col = brewer.pal(9, "Greens"), trace = "none",
            key = FALSE, dend = "none", Colv = FALSE, cexCol = 1.2,
            main = "\n\n\n\n\nFactor loadings for brand adjectives")


# 5 SEMプロットによる解釈 ----------------------------------------------------

# ＜ポイント＞
# - 因子分析では潜在変数と各属性の関係をパス図を使って解釈することができる（SEMプロット）
#   --- パス図はLoadingが大きいものをパスで繋いでいる（因子負荷量のヒートマップでは関係性は表現できない）
#   --- 確認的因子分析や構造方程式モデルを扱う際に様々なパス図を使用する


# パス図の作成
brand.fa.ob %>%
  semPaths(what = "est", residuals = FALSE, cut = 0.3,
           posCol = c("white", "darkgreen"), negCol = c("white", "red"),
           edge.label.cex = 0.75, nCharNodes = 7)


# 6 ブランドの因子得点 -------------------------------------------------------------

# ＜ポイント＞
# - 因子分析では各観測対象の因子得点も推定することができる
#   --- 顧客ごとやグループ単位での特性の確認に役立つ


# 因子分析の実行
# --- 因子スコアはscores引数に値を設定することで出力される
brand.fa.ob <-
  brand.sc %>%
    select_if(is.numeric) %>%
    factanal(factors = 3, rotation = "oblimin", scores = "Bartlett")

# 因子スコアの出力
brand.scores <-
  brand.fa.ob$scores %>%
    as_tibble() %>%
    mutate(brand = brand.sc$brand)

# 確認
brand.scores %>% head()

# 因子スコアの集計
# --- カテゴリごとの平均スコア
brand.fa.mean <-
  brand.scores %>%
    group_by(brand) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    set_colnames(c("brand", "Leader", "Value", "Latest"))

# 確認
brand.fa.mean %>% print()

# ヒートマップの作成
# --- 因子とカテゴリの関係を確認
brand.fa.mean %>%
  select_if(is.numeric) %>%
  as.matrix() %>%
  heatmap.2(col = brewer.pal(9, "GnBu"), trace = "none",
            key = FALSE, dend = "none", cexCol = 1.2,
            main = "\n\n\n\n\nMean factor score by brand")
