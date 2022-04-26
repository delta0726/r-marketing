# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（メイン）
# Objective : ロジスティック回帰
# Created on: 2022/04/27
# Page      : P282 - P295
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 2値データを目的変数とするモデルはロジスティック回帰でアプローチする
#   --- 結果の予測確率を予測変数の指数関数と関連付ける
#   --- 予測確率として0-1の範囲で出力され、予測確率の水準に応じて2値分類を行う


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 ロジスティックの計算
# 3 最も単純なモデル
# 4 モデル再考
# 5 変数を追加したモデル
# 6 交互効果を考慮したモデル


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(psych)
library(car)
library(vcd)
library(vcdExtra)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
# --- 遊園地のシーズンパスを購入するかどうか
pass.df <- read_csv("data/pass_df.csv")

# データ加工
pass.df <-
  pass.df %>%
    mutate(Promo = factor(Promo, levels = c("NoBundle", "Bundle")))


# 1 データ確認 --------------------------------------------------------------------------

# データ概要
pass.df %>% print()
pass.df %>% glimpse()

# サマリー
pass.df %>% summary()


# 2 ロジスティックの計算 -------------------------------------------------------------------

# ロジスティックの計算
# --- 手動計算
# --- computing logistic by hand; could use plogis()
# --- infinite dispreference = likelihood 0
# --- moderate preference = 88% chance (e.g., of purchase)
# --- weak dispreference
exp(0) / (exp(0) + 1)
plogis(-Inf)
plogis(2)
plogis(-0.2)

# ロジットモデル
# --- 相対確率の対数からロジスティックを求める
# --- 1 : indifference = 50% likelihood = 0 utility
# --- 2 : moderate high likelihood
# --- 3 : equivalent to hand computation
log(0.5 / (1 - 0.5))
log(0.88 / (1 - 0.88))
qlogis(0.88)


# 3 最も単純なモデル ---------------------------------------------------------------------

# ＜ポイント＞
# - ロジスティック回帰モデルは線形回帰モデルと同様の手順で一般化線形モデル(GLM)として推定される
#   --- 目的変数は正規分布である必要はない（2値分類でも問題ない）
#   --- GLMはリンク関数を使用して正規分布に従う説明変数を正規分布しない目的変数に関連付ける


# モデル構築
# --- 2値分類は二項分布が適切（確率分布にbinomialを指定する）
pass.m1 <- glm(Pass ~ Promo, data = pass.df, family = binomial)

# サマリー
# --- PromoBundleの回帰係数の0.389
pass.m1 %>% glance()
pass.m1 %>% tidy()

# オッズ比
# --- PromoBundleのオッズ比は1.475（販売促進があれば購入確率は1.475倍になる）
pass.m1 %>% coef() %>% exp()

# 信頼区間
pass.m1 %>% confint() %>% exp()


# 4 モデル再考 ---------------------------------------------------------------------------------

# ＜ポイント＞
# - 販売促進の有無に関わらず、シーズンパスが最も売れているのは園内販売となっている
#   --- 販売チャネルごとに売れ行きが異なることが確認できる（Channelをモデルに追加する必要がある）


# 混合行列
table(pass.df$Pass, pass.df$Channel)

# モザイクプロットの作成
# --- 分割表の確認はモザイクプロットが分かりやすい
pass.df %>% table() %>% doubledecker()


# 5 変数を追加したモデル ----------------------------------------------------------------------

# ＜ポイント＞
# - モデル再考の知見によりChannelをモデルに追加する
#   --- プロモーションをすると売上が減少するという矛盾が発生する（シンプソンのパラドックス）


# モデル構築
pass.m2 <- glm(Pass ~ Promo + Channel, data = pass.df, family = binomial)

# サマリー
pass.m2 %>% tidy()

# 回帰係数
# --- salesのオッズ比
pass.m2 %>% coef() %>% exp()

# 信頼区間
# --- プロモーションをすると0.47-0.68倍となる
# --- 公園内だと30-56倍になる
pass.m2 %>% confint() %>% exp()


# 6 交互効果を考慮したモデル -----------------------------------------------------------------

# ＜ポイント＞
# - プロモーションをすると売上が減少するという矛盾を除くため交互効果を導入してみる


# モデル構築
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, data = pass.df, family = binomial)

# サマリー
# --- 各係数がプラス係数になった一方、交互効果がマイナス係数となった
pass.m3 %>% tidy()

# 信頼区間
pass.m3 %>% confint() %>% exp()
