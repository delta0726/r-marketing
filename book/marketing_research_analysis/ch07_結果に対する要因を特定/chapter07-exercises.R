# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 7 結果に対する要因を特定する（演習）
# Objective : TODO
# Created by: Owner
# Created on: 2021/03/09
# Page      : P231 - P234
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 変数分布の可視化
# 2 相関パターンの確認
# 3 順序尺度の相関係数
# 4 エリート会員特典の満足度と全体満足度の関係
# 5 他の要因をコントロール(4の続き)
# 6 ターゲット層を明確化したモデリング
# 7 絞り込みの問題点(6の続き)
# 8 結果の再考(6の続き)
# 9 カテゴリカルデータを含むモデル
# 10 両方の解釈が可能な変数
# 11 順序尺度と予測値からのプロット作成1
# 12 順序尺度と予測値からのプロット作成2(11の続き)
# 13 ベイズ回帰分析(8の続き)
# 14 ベイズ回帰モデルと古典的線形モデルのプロット比較(13の続き)


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(psych)
library(car)
library(corrplot)
library(MCMCpack)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# データ準備
# --- ホテルデータ
# --- 顧客満足度調査への回答とホテル滞在に関する基本的な会計情報
hotel.df <- read_csv("data/hotelsat-data.csv")

# データ変換
# --- 1で同様の操作あり
hotel.df.tr <-
  hotel.df %>%
    mutate(distanceTraveled     = log(distanceTraveled),
           nightsStayed         = log(nightsStayed),
           avgFoodSpendPerNight = log(avgFoodSpendPerNight + 1))

# データ確認
hotel.df.tr %>% print()
hotel.df.tr %>% glimpse()


# 1 変数分布の可視化 ---------------------------------------------------------------------------------

# 1. Visualize the distributions of the variables in the hotel satisfaction data. 
#    Are there variables that might be understood better if they are transformed? 
#    Which variables and what transforms would you apply? 
#    (Note: for efficiency, it may help to divide the data set into smaller sets of similar variables.)

# 詳細なデータサマリー
# --- psych::describe()
hotel.df %>% describe()

# 散布図行列
# --- 特徴量が多いので分けて表示
hotel.df %>% select(1:9) %>% scatterplotMatrix()
hotel.df %>% select(10:18) %>% scatterplotMatrix()
hotel.df %>% select(19:25) %>% scatterplotMatrix()

# ヒストグラム作成
# --- 移動距離(distanceTraveled)
# --- 対数変換が必要
par(mfrow = c(1, 2))
hotel.df$distanceTraveled %>% hist()
hotel.df$distanceTraveled %>% log() %>% hist()

# ヒストグラム作成
# --- 滞在日数(nightsStayed)
# --- 対数変換が必要
par(mfrow = c(1, 2))
hotel.df$nightsStayed %>% hist()
hotel.df$nightsStayed %>% log() %>% hist()

# ヒストグラム作成
# --- 食費(avgFoodSpendPerNight)
# --- 対数変換が必要
hotel.df$avgFoodSpendPerNight %>% hist()
hotel.df$avgFoodSpendPerNight %>% log() %>% hist()

# データ再定義
hotel.df.tr <-
  hotel.df %>%
    mutate(distanceTraveled     = log(distanceTraveled),
           nightsStayed         = log(nightsStayed),
           avgFoodSpendPerNight = log(avgFoodSpendPerNight + 1))

# プロット作成
par(mfrow=c(1, 1))
hotel.df.tr %>%
  select(distanceTraveled, nightsStayed, avgFoodSpendPerNight) %>%
  scatterplotMatrix()


# 2 相関パターンの確認 -------------------------------------------------------------------

# 相関係数プロット
# ---
hotel.df.tr %>%
  select(-21, -25) %>%
  cor() %>%
  corrplot()


# 3 順序尺度の相関係数 ------------------------------------------------------------------

# ＜ポイント＞
# - 連続データの相関係数にはピアソン相関係数を使う
# - 連続データをロバストに評価したい場合はスピアマン相関係数を使う
#   --- いずれも同順位がないことを想定している
# - 順序尺度(カテゴリカルデータ)は同じ値を持つためポリコック相関係数を用いる
#   --- 順序尺度の評価から背後の連続分布における相関係数を最尤推定で算出

# ＜参考＞
# - 質的変数の相関・因子分析
# https://www.slideshare.net/mitsuoshimohata/ss-24419059


# データ確認
# --- 当該データは順序尺度であることを確認
# --- 1-7で表現されているが、間隔が全て同じ意味合いかは保証されない
hotel.df %>% select(1:3) %>% map(table)

# ピアソン相関係数
# --- 連続値を想定している
hotel.df %>% select(satCleanRoom, satCleanBath, satCleanCommon) %>% cor()

# ポリコック相関係数
# --- 順序尺度を想定している
hotel.df %>% select(satCleanRoom, satCleanBath, satCleanCommon) %>% polychoric()


# 4 エリート会員特典の満足度と全体満足度の関係 -----------------------------------------------

# ＜ポイント＞
# - 測定したい変数をXとして回帰モデルを定義して回帰係数から解釈する
# - データが線形回帰モデルの前提を満たすかを確認しておく

# データ確認
hotel.df.tr %>%
  select(satOverall, satPerks) %>%
  glimpse()

# モデル定義
# --- Y：satOverall（全体満足度）
# --- X：satPerks（エリート会員特典の満足度）
hotel.perks.lm  <-
  hotel.df.tr %>%
     lm(satOverall ~ satPerks, data = .)

# 回帰係数の確認
# --- 回帰係数が0.42、t値が17,p値が0
# --- 関係性はあると判断できる
hotel.perks.lm %>% tidy()


# 5 他の要因をコントロール(4の続き) --------------------------------------------------------

# ＜ポイント＞
# - 変数間の相関は回帰係数の解釈を歪める要因となる
#   --- 相関がある変数をモデルに含めることで当該要因を取り除くことができる

# データ確認
hotel.df.tr %>%
  select(satOverall, satPerks, satCity, satFrontStaff) %>%
  glimpse()

# モデル定義
# --- Y：satOverall（全体満足度）
# --- X1：satPerks（エリート会員特典の満足度）
# --- X2：satCity（ロケーション満足度）
# --- X3：satFrontStaff（フロントスタッフ満足度）
hotel.perks.lm2 <-
   hotel.df.tr %>%
      lm(satOverall ~ satPerks + satCity + satFrontStaff, data = .)

# 回帰係数の確認
# --- 回帰係数が0.17、t値が6.57,p値が0
# --- 関係性は認められるが、調整変数の導入により回帰係数などは低下した
# --- X1がX2やX3と相関があることを示唆
hotel.perks.lm2 %>% tidy()

# 相関係数の確認
# --- 確かに相関が高い
hotel.df.tr %>%
  select(satPerks, satCity, satFrontStaff) %>%
  cor()


# 6 ターゲット層を明確化したモデリング -------------------------------------------------------

# ＜ポイント＞
# - データセットをターゲット層に絞り込むことでモデルの目的を明確化する

# データ確認
hotel.df.tr %>%
  select(satRecognition, satCleanRoom, satFrontStaff, satPoints, satPerks) %>%
  glimpse()

# モデル定義
# --- ゴールド会員とプレミアム会員に限定して分析する
# --- Y：satOverall（全体満足度）
# --- X1：satCleanRoom（部屋の清潔さ満足度）
# --- X2：satFrontStaff（フロントスタッフ満足度）
# --- X3：satPoints（ポイント満足度）
# --- X4：satPerks（エリート会員特典満足度）
hotel.rec.lm <-
  hotel.df.tr %>%
    filter(eliteStatus %in% c("Gold", "Platinum")) %>%
    lm(satRecognition ~ satCleanRoom + satFrontStaff + satPoints + satPerks, data = .)


# 回帰係数の確認
# --- 以下の順番で効果が認められる
# --- satPoints > satPerks > satFrontStaff > satCleanRoom
hotel.rec.lm %>% tidy()


# 7 絞り込みの問題点(6の続き) -------------------------------------------------------------

# ＜ポイント＞
# - モデル解釈では回帰係数だけでなくt値やp値などの信頼性指標にも配慮が必要

# t値に注目
# --- t値はこれまでの分析と比べて低くなっている
# --- 標準誤差が大きいためt値が下がっている
hotel.rec.lm %>% tidy()

# レコード数
# --- t値が下がる背景の一つはサンプル数が減っていることが挙げられる
hotel.df.tr %>% nrow()
hotel.df.tr %>% filter(eliteStatus %in% c("Gold", "Platinum")) %>% nrow()


# 8 結果の再考(6の続き) ---------------------------------------------------------------------

# ＜質問＞
# - 部屋の清潔さにもっと投資すべきか

# ＜回答＞
# - 全体でみても傾向は低いので、現時点ではやはり優先度は低いと判断

# モデル構築
# --- 全体で評価
hotel.rec.lm <-
  hotel.df.tr %>%
    lm(satRecognition ~ satCleanRoom + satFrontStaff + satPoints + satPerks, data = .)

# 回帰係数の確認
# --- 7の時よりもt値は上がった
hotel.rec.lm %>% tidy()



# 9 カテゴリカルデータを含むモデル --------------------------------------------------------------

# ＜ポイント＞
# - カテゴリカル変数を説明変数に採用すると自動的にダミー変数として扱われる

# データ確認
hotel.df.tr %>%
  select(avgFoodSpendPerNight, eliteStatus, satDiningPrice) %>%
  glimpse()

# カテゴリの確認
hotel.df.tr %>% select(eliteStatus) %>% table()

# モデル定義
# --- Y：avgFoodSpendPerNight（レストラン支出）
# --- X1：eliteStatus（エリートステータス）※カテゴリカルデータ
# --- X2：satDiningPrice（食事価格の満足度）
hotel.food.lm <-
   hotel.df.tr %>%
      lm(avgFoodSpendPerNight ~ eliteStatus + satDiningPrice, data = .)

# 回帰係数の確認
# --- 回帰係数/t値それぞれの観点から関係は薄いと思われる
hotel.food.lm %>% tidy()


# 10 両方の解釈が可能な変数 --------------------------------------------------------------

# ＜質問＞
# - 食事代は一般的に安いほうが満足度が高いが、高級志向(食事代が高い)による満足度も考えられる
#   --- 今回のデータはどちらか？

# データ確認
hotel.df.tr %>%
  select(avgFoodSpendPerNight, satDiningPrice) %>%
  glimpse()

# モデル定義
# --- Y：avgFoodSpendPerNight（レストラン支出）
# --- X1：satDiningPrice（食事価格満足度）
hotel.food.lm2 <-
   hotel.df.tr %>%
      lm(avgFoodSpendPerNight ~ satDiningPrice, data = .)

# 回帰係数の確認
# --- 回帰係数は0.146とプラス
# --- 価格が高いほうが満足度が高いことを示唆している
hotel.food.lm2 %>% tidy()


# 11 順序尺度と予測値からのプロット作成1 ----------------------------------------------------------

# ＜ポイント＞
# - 予測モデルの結果は散布図で表現すると分かりやすい
# - 順序尺度のように同じ場所に系列が多くプロットされるのを避けたい場合はjitterを用いる


# ＜参考＞
# jitter plot
# https://stats.biopapyrus.jp/python/jitterplot.html


# データ確認
hotel.df.tr %>%
  select(avgFoodSpendPerNight, nightsStayed) %>%
  glimpse()

# モデル定義
# --- Y：avgFoodSpendPerNight（レストラン支出）
# --- X1：nightsStayed（滞在日数）
hotel.food.lm.bynights <-
  hotel.df.tr %>%
     lm(avgFoodSpendPerNight ~ nightsStayed, data = .)

# 系列作成
# --- jitter()で順序データに若干の揺らぎを与えてプロットが見やすくする
x_val <- hotel.df.tr$nightsStayed %>% exp() %>% jitter()
y_val <- hotel.food.lm.bynights %>% fitted() %>% exp() %>% jitter()

# プロット作成
# --- 滞在に数が多いほど夕食代が高くなる傾向
plot(x_val, y_val,
     xlab = "Nights stayed", ylab = "Mean Food Spend per Night ($)")

# 予測値の作成
# --- 40拍した場合の食事代
# --- nightsStayedは対数値で入力されていることに注意
hotel.food.lm.bynights %>% predict(tibble(nightsStayed = log(40))) %>% exp()


# 12 順序尺度と予測値からのプロット作成2(11の続き) ------------------------------------------------

# データ確認
hotel.df.tr %>%
  select(avgFoodSpendPerNight, nightsStayed) %>%
  glimpse()

# モデル定義
# --- プラチナメンバーに限定
# --- Y：avgFoodSpendPerNight（レストラン支出）
# --- X1：nightsStayed（滞在日数）
hotel.food.lm.bynights.pl <-
  hotel.df.tr %>%
    filter(eliteStatus == "Platinum") %>%
    lm(avgFoodSpendPerNight ~ nightsStayed, data = .)

# 回帰係数の確認
hotel.food.lm.bynights %>% tidy()
hotel.food.lm.bynights.pl %>% tidy()

# 系列作成
# --- jitter()で順序データに若干の揺らぎを与えてプロットが見やすくする
x_val <- hotel.df.tr$nightsStayed %>% exp() %>% jitter()
y_val <- hotel.food.lm.bynights %>% fitted() %>% exp() %>% jitter()
x_val_pl <- hotel.df.tr$nightsStayed[hotel.df.tr$eliteStatus == "Platinum"] %>% exp() %>% jitter()
y_val_pl <- hotel.food.lm.bynights.pl %>% fitted() %>% exp() %>% jitter()

# プロット作成
# --- プラチナ会員は食事代が高い傾向
plot(x_val, y_val, col="red", xlab="Nights stayed", ylab="Mean Food Spend per Night ($)")
points(x_val_pl, y_val_pl, col="blue")
legend("topleft", legend = c("All visitors", "Platinum members"), col = c("red", "blue"), pch = 1)


# 13 ベイズ回帰分析(8の続き) ----------------------------------------------------------------

# データ確認
hotel.df.tr %>%
  select(satRecognition, satCleanRoom, satFrontStaff, satPoints, satPerks) %>%
  glimpse()

# モデル定義
# --- ベイズ回帰分析
# --- Y：satOverall（全体満足度）
# --- X1：satCleanRoom（部屋の清潔さ満足度）
# --- X2：satFrontStaff（フロントスタッフ満足度）
# --- X3：satPoints（ポイント満足度）
# --- X4：satPerks（エリート会員特典満足度）
hotel.rec.lm.b <-
  hotel.df.tr %>%
    filter(eliteStatus %in% c("Gold", "Platinum")) %>%
    MCMCregress(satRecognition ~ satCleanRoom + satFrontStaff + satPoints + satPerks, data = .)


# サマリー
hotel.rec.lm.b %>% summary()
hotel.rec.lm %>% tidy()


# 14 ベイズ回帰モデルと古典的線形モデルのプロット比較(13の続き) ----------------------------------

# データフレーム作成
# --- 2つのモデルの回帰係数
hotel.rec.compare <-
  data.frame(classical = coef(hotel.rec.lm)[-1],
             bayesian  = summary(hotel.rec.lm.b)$statistics[c(-1, -6), 1])

# 確認
hotel.rec.compare %>% print()

# 散布図作成
# --- 45度線に乗っていると完全一致
hotel.rec.compare %>% plot(bayesian ~ classical, data = .)
abline(0,1)

# 相関係数
# --- 概ね1に近い
# --- 示唆している内容自体は大きく変わらない
hotel.rec.compare %$% cor(classical, bayesian)
