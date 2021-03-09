# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 7 結果に対する要因を特定する（メイン）
# Objective : TODO
# Created by: Owner
# Created on: 2021/03/08
# Page      : P193 - P231
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜ポイント＞
# - 線形モデルは目的変数と説明変数の関係性を明らかにして要因分析を可能とする
#   --- 機械学習は｢予測｣を目的とするのに対し、統計学では｢関係性の分析｣を目的とすることが多い
# - 線形モデルで導き出された｢線形関係｣は｢因果関係｣を示唆するわけではない
#   --- ドメイン知識に基づく注意深い解釈が求められる


# ＜目次＞
# 1 データ確認
# 2 線形回帰の導入
# 3 lmオブジェクト
# 4 信頼区間
# 5 モデル適合性のチェック
# 6 複数の予測因子による線形モデル近似
# 7 モデル比較
# 8 モデル予測
# 9 回帰係数の標準化
# 10 因子型変数を説明変数に使う
# 11 相互効果の追加
# 12 オーバーフィットの回避
# 13 ベイズ線形モデル


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(car)
library(coefplot)
library(corrplot)
library(MCMCpack)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr")


# データ準備
sat.df <- read_csv("book/marketing_research_analysis/data/rintro-chapter7.csv")

# データ確認
sat.df %>% print()
sat.df %>% glimpse()


# 1 データ確認 ----------------------------------------------------------------------------

# データサマリー
# --- 8変数
sat.df %>% summary()

# データ可視化
# --- distが対数正規分布のため偏りがある
sat.df %>%
  select_if(is.numeric) %>%
  scatterplotMatrix()

# 列の作成
# --- distの対数列を追加
sat.df <-
  sat.df %>%
    mutate(logdist = log(sat.df$distance))

# データ可視化
# --- logdistが正規的になった
sat.df %>%
  select_if(is.numeric) %>%
  scatterplotMatrix()

# 相関プロット
sat.df %>%
  select(num.child, rides:logdist) %>%
  cor() %>%
  corrplot.mixed(upper = "ellipse")


# 2 線形回帰の導入 ------------------------------------------------------------------

# 相関プロット
# --- 全体満足度 vs 乗り物満足度
plot(overall ~ rides, data = sat.df,
     xlab = "Satisfaction with Rides", ylab = "Overall Satisfaction")

# 線形回帰
lm(overall ~ rides, data = sat.df)

# 全体満足度の予測
# --- 乗り物満足度を95とする
-94.962 + 1.703 * 95


# 3 lmオブジェクト ------------------------------------------------------------------

# 線形回帰（再掲）
m1 <- lm(overall ~ rides, data = sat.df)

# プロット表示
# --- 線形回帰のイメージ
plot(overall~rides, data = sat.df,
     xlab = "Satisfaction with Rides", ylab = "Overall Satisfaction")

abline(m1, col = 'blue')

# オブジェクト構造
# --- 様々な要素を格納しているので複雑
m1 %>% glimpse()
m1$coefficients

# サマリー
m1 %>% summary()


# 4 信頼区間 ------------------------------------------------------------------------

# 手動計算
# --- 1.73 ： 推定した回帰係数(傾き)
# --- 1.96 ： 95%信頼区間の値
# --- 0.106： 回帰係数の誤差（標準誤差）
1.703 - 1.96 * (0.106)
1.703 + 1.96 * (0.106)

# 信頼区間の計算
m1 %>% confint()

cor(sat.df$overall, sat.df$rides)^2
# Try it: anova(m1)


# 5 モデル適合性のチェック --------------------------------------------------------------

# ＜ポイント＞
# - 線形回帰にはモデル妥当性を評価する仕組みが用意されている
#   --- 線形回帰は簡単に計算できるため、分析者はモデル妥当性を考慮することなく安易に使うことが多い
#   --- 線形回帰モデルの｢前提｣が守られているかをチェックしていく

# ＜線形回帰の仮定＞
# 1 独立性（データそれぞれが独立）
# 2 等分散性（説明変数にかかわらず分散が一定）
# 3 正規性（誤差自体が正規分布している）
# 4 線形性（説明変数と目的変数の関係は直線で近似できる）


# 線形性の仮定 *******************************************************

# 準備
dev.off()

# 仮想モデル
# --- 非線形モデル（二次関数）
x <- rnorm(500)
y <- x ^ 2 + rnorm(500)
toy.model <- lm(y~x)

# プロット表示
plot(y ~ x)
abline(toy.model, col = 'blue')

# 分析プロット作成
# --- モデルの線形性を確認
# --- 予測値と残差の関係
toy.model %$% plot(fitted.values, residuals)


# 評価プロット *******************************************************

# 回帰モデルの評価
# --- lmオブジェクトを直接プロット
# --- 前提をチェックするための4つのプロットが出力
par(mfrow = c(2,2))
m1 %>% plot()

# 外れ値
sat.df[c(57, 129, 295), ]


# 6 複数の予測因子による線形モデル近似 -------------------------------------------------

# モデル定義
m2 <- lm(overall ~ rides + games + wait + clean, data = sat.df)

# モデルサマリー
m2 %>% summary()

# 評価プロット
m2 %>% plot()


# 信頼区間プロット
par(mfrow = c(1,1))
m2 %>%
  coefplot(intercept = FALSE, outerCI = 1.96, lwdOuter = 1.5,
           ylab = "Rating of Feature",
           xlab = "Association with Overall Satisfaction")


# 7 モデル比較 --------------------------------------------------------------------

# 決定係数(R2)
# --- 説明変数が多くなると高くなるバイアスを持つ
m1 %>% summary() %>% use_series(r.squared)
m2 %>% summary() %>% use_series(r.squared)

# 自由度調整済の決定係数（adjR2）
m1 %>% summary() %>% use_series(adj.r.squared)
m2 %>% summary() %>% use_series(adj.r.squared)


# プロット作成
# --- 全体満足度
# --- モデル1：赤
plot(sat.df$overall, fitted(m1), col = 'red',
     xlim = c(0,100), ylim = c(0,100),
     xlab = "Actual Overall Satisfaction", ylab = "Fitted Overall Satisfaction")

# プロット追加
# --- モデル2：赤
points(sat.df$overall, fitted(m2), col = 'blue')

# 系列表示
legend("topleft", legend = c("model 1", "model 2"),
        col = c("red", "blue"), pch = 1)

# ANOVA分析
anova(m1, m2)


# 8 モデル予測 --------------------------------------------------------------------

# 予測値の計算
# --- 手動
coef(m2)["(Intercept)"] +
  coef(m2)["rides"] * 100 +
  coef(m2)["games"] * 100 +
  coef(m2)["wait"] * 100 +
  coef(m2)["clean"] * 100

# 予測値の計算
# --- 行列
coef(m2) %*% c(1, 100, 100, 100, 100)

# 予測値の計算
# --- 与えたデータに対してモデルから予測値を計算
m2 %>% predict(sat.df[1:10,])

# 予測値の取得
# --- モデル計算時にインサンプルの予測値は計算済
m2 %>% fitted() %>% .[1:10]


# 9 回帰係数の標準化 -----------------------------------------------------------------

# データ基準化
sat.df %$% (rides - mean(rides)) / sd(rides)
sat.df$rides %>% scale()


# データ再定義
# --- distance列を削除
# --- 数値列を基準化
sat.std <-
  sat.df %>%
    select(-distance) %>%
    mutate_at(3:7, function(x) (x - mean(x)) / sd(x))

# データ確認
sat.std %>% head()
sat.std %>% summary()


# 10 因子型変数を説明変数に使う -----------------------------------------------------------

# モデル3：weekend(因子)を追加 ******************************************

# データ確認
sat.std %>% glimpse()

# モデル定義
# --- 因子ファクター(weekend)を追加
# --- num.childは数値として扱っている
m3 <-
   lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child,
      data  =  sat.std)

# モデルサマリー
m3 %>% summary()


# モデル4：num.child(因子)を追加 ******************************************

# データ再定義
# --- num.childをファクターに変換
# --- 数値列を基準化
sat.std.2 <-
  sat.std %>%
    mutate(num.child.factor = factor(num.child))

# モデル定義
m4 <-
   lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child.factor,
      data = sat.std.2)

# モデルサマリー
m4 %>% summary()


# num.childのフラグ化 ******************************************

# データ再定義
# --- num.childが0より大きいかでフラグ化
sat.std.3 <-
  sat.std.2 %>%
    mutate(has.child = factor(num.child > 0))

# モデル定義
m5 <-
   lm(overall ~ rides + games + wait + clean + logdist + has.child,
      data = sat.std)

# モデルサマリー
m5 %>% summary()


# 11 相互効果の追加 -----------------------------------------------------------

# モデル定義
# --- 相互効果を追加
m6 <-
   lm(overall ~ rides + games + wait + clean + weekend + logdist + has.child +
     rides:has.child + games:has.child + wait:has.child +
     clean:has.child + rides:weekend + games:weekend +
     wait:weekend + clean:weekend, data = sat.std)

m6 %>% summary()



# 12 オーバーフィットの回避 -----------------------------------------------------------

####
# reduced model
m7 <- lm(overall ~ rides + games + wait + clean + logdist + has.child +
                   wait:has.child,
         data = sat.std)
m7 %>% summary()

# TRY: coef(m7)["wait"] + coef(m7)["wait:has.childTRUE"]

# Try it!
# anova(m5, m7)
# plot(m7)



m7 %>%
  coefplot(intercept = FALSE, outerCI = 1.96, lwdOuter = 1.5,
           ylab = "Rating of Feature",
           xlab = "Association with Overall Satisfaction")



# 13 ベイズ線形モデル -----------------------------------------------------------

####
# Bayesian Linear Model

m7.bayes <- MCMCregress(overall ~ rides + games + wait + clean + logdist +
                          has.child + wait:has.child,
                        data = sat.std)
m7.bayes %>% summary()

