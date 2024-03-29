# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（メイン）
# Objective : 階層線形モデル
# Created on: 2022/11/25
# Page      : P295 - P307
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 階層線形モデルを用いると｢母集団レベルの効果｣と｢個体レベルの効果｣の両方を推定することができる
#   --- 母集団レベルの効果を｢固定効果｣といい、｢個体レベルの効果｣を｢ランダム効果｣という
#   --- 全体を推定した後に個体を推定している
#   --- 個体ごとに複数回の測定を行っている必要がある
#   --- {lme4}を使って階層線形モデルにアプローチする


# ＜発展＞
# - 階層モデルは個人のサンプル数が少なくなる可能性があるのでMCMCを用いると頑健（階層ベイズモデル）
# - 階層ごとに効果を完全に分断するモデルもある（ネストモデル）


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 線形モデルの構築
# 3 階層線形モデルの構築
# 4 完全な階層線形モデル


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(broom.mixed)
library(lme4)
library(arm)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("extract", "magrittr")


# データ準備
# --- ジェットコースター16種類に対する200人のアンケート（3200レコード）
conjoint.df <- read_csv("data/conjoint_df.csv")

# データ加工
conjoint.df <-
  conjoint.df %>%
    mutate(speed = factor(speed),
           height = factor(height))


# 1 データ確認 ----------------------------------------------------------------------------

# データ確認
# --- カテゴリカルjデータのみで構成されている（前処理不要）
conjoint.df %>% print()
conjoint.df %>% glimpse()
conjoint.df %>% map(table)

# サマリー
conjoint.df %>% summary()

# データ集計
# --- 高さごとのレーティング
# --- 中間的な高さで評価が高まる
conjoint.df %>%
  group_by(height) %>%
  summarise(rating = mean(rating)) %>%
  ungroup()

# データ集計
# --- スピードごとのレーティング
# --- スピードが速いほど評価が高まる
conjoint.df %>%
  group_by(speed) %>%
  summarise(rating = mean(rating)) %>%
  ungroup()

# データ集計
# --- 素材ごとのレーティング
# --- 素材の違いに大差はない
conjoint.df %>%
  group_by(const) %>%
  summarise(rating = mean(rating)) %>%
  ungroup()

# データ集計
# --- テーマごとのレーティング
# --- テーマの違いに大差はない
conjoint.df %>%
  group_by(theme) %>%
  summarise(rating = mean(rating)) %>%
  ungroup()



# 2 線形モデルの構築 ----------------------------------------------------------------------------

# ＜ポイント＞
# - 線形回帰モデルでは3項目が統計的に有意
#   --- 3つの有意性は望ましくない属性との対比として現れたもの
#   --- 有意性の高い3つの属性に絞ればよいというわけではない

# ＜考察＞
# - constWoodは回帰係数がゼロ付近となっており、全体でみるとほどんど関心がないように見える
#   --- 全員が無関心なのか、平均すると無関心なのか、は知っておく必要がある
#   --- 階層線形モデルによりアプローチ


# 線形モデルの構築
# --- Y：rating
ride.lm <- lm(rating ~ speed + height + const + theme, data = conjoint.df)

# 回帰係数
# --- いずれもt値は非常に高い
# --- speed70, height300, theme: dragon が特に有意となっている
ride.lm %>% tidy()


# 3 階層線形モデルの構築 --------------------------------------------------------------------------

# ＜ポイント＞
# - 線形回帰モデルは固定効果のみを持つが、階層線形モデル(HLM)では個人レベルの効果を追加する
#   --- 各個人に対して定数項のみの変化を許容する


# ＜lme4の記法＞
# - + (predoctors | group)
# - ランダム効果(predoctors)とグループ化変数(group)を選ぶ


# データ確認
# --- resp.idは個人を特定する
conjoint.df %>% head()
conjoint.df$resp.id %>% unique()

# 階層線形モデルの構築
# --- 定数項のみを階層化する（定数項は1で指定）
# --- グループはresp.id（個人を特定）
# --- (1 | resp.id) と記述する
ride.hlm1 <- lmer(rating ~ speed + height + const + theme + (1 | resp.id), data = conjoint.df)

# サマリー
ride.hlm1 %>% summary()
ride.hlm1 %>% tidy()

# 固定効果
# --- lmで推定されたものと同様
ride.hlm1 %>% fixef()
ride.lm %>% coef()

# ランダム効果（個人ごと）
# --- (Intercept)のみがランダム効果
ride.hlm1 %>% ranef() %>% use_series(resp.id) %>% as_tibble()

# 個人効果
# --- 固定効果とランダム効果を同時に出力
# --- resp.idごとに切片項のみランダムとなっている
ride.hlm1 %>% coef() %>% use_series(resp.id) %>% as_tibble()

# 個人効果
# --- 個人ごとの定数項（全体の定数項(固定効果)をランダム効果で調整）
# --- 固定効果 + ランダム効果
fixed_effect <- ride.hlm1 %>% fixef() %>% extract("(Intercept)")
random_effect <- ride.hlm1 %>% ranef() %>% use_series(resp.id) %>% extract("(Intercept)")

# 効果合計
tibble(fixed_effect = fixed_effect,
       random_effect = random_effect) %>%
  mutate(total_effect = fixed_effect + random_effect)


# 4 完全な階層線形モデル --------------------------------------------------------------------------

# ＜ポイント＞
# - 一般的なリサーチでは全ての回帰係数に対してランダム効果を推定するのがセオリー
#   --- 切片のみの階層モデルよりもはるかに複雑（コントロール変数を調整して高速化）


# モデル構築
# --- 各変数を階層化する
# --- 8つの固定効果と1600(8*200)のランダム効果が出力される
ride.hlm2 <-
  lmer(rating ~ speed + height + const + theme + (speed + height + const + theme | resp.id),
       data = conjoint.df,
       control = lmerControl(optCtrl = list(maxfun = 100000),
                             optimizer = "Nelder_Mead"))

# サマリー
# --- 全ての変数でランダム効果が計算されている
ride.hlm2 %>% summary()


# 固定効果
# --- lmで推定されたものと同様
ride.hlm2 %>% fixef()
ride.lm %>% coef()

# 個人のランダム効果
# --- グループ化したレベルごとに出力（resp.id）
ride.hlm2 %>% ranef() %>% use_series(resp.id) %>% as_tibble()

# 個人の固定効果
# --- 今回は定数項のみ
# --- グループ化したレベルごとに出力（resp.id）
ride.hlm2 %>% coef() %>% use_series(resp.id) %>% as_tibble()


# 総合スコアの算出
# --- 固定効果 + ランダム効果
fixef(ride.hlm2) + ranef(ride.hlm2)$resp.id[196, ]
coef(ride.hlm2)$resp.id[196, ]

# 信頼区間の算出
# --- 書籍に記載なし
se.ranef(ride.hlm2)$resp.id %>% as_tibble()
ranef(ride.hlm2)$resp.id[196, ] - 1.96 * se.ranef(ride.hlm2)$resp.id[1,]
ranef(ride.hlm2)$resp.id[196, ] + 1.96 * se.ranef(ride.hlm2)$resp.id[1,]
