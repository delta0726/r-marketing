# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（エクササイズ）
# Objective : TODO
# Created by: Owner
# Created on: 2021/03/12
# Page      : P327 - P328
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************




# ＜目次＞
# 0 準備
# 1 線形モデルによる予測(11)
# 2 階層線形モデルによる予測(12)
# 3 モデルによる推定(13)


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(lme4)
library(psych)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
conjoint.df <- read_csv("book/marketing_research_analysis/data/conjoint_df2.csv")

# データ確認
conjoint.df %>% print()
conjoint.df %>% glimpse()

# サマリー
conjoint.df %>% summary()


# 1 線形モデルによる予測(11) --------------------------------------------------------------------

# モデル構築
bag.lm <-
   conjoint.df %>%
      lm(rating ~ price + color + zipper + finish, data = .)

# サマリー
# --- どの変数も有意のようだ
bag.lm %>% tidy()


# 2 階層線形モデルによる予測(12) ----------------------------------------------------------------

# モデル構築
# --- 各変数で階層化
bag.hlm1 <-
  conjoint.df %>%
    lmer(rating ~ price + color + zipper + finish +
        (price + color + zipper + finish | resp.id),
         data = ., control = lmerControl(optCtrl = list(maxfun = 100000)))

# 固定効果
bag.hlm1 %>% fixef()

# ランダム効果
bag.hlm1 %>% ranef() %>% use_series(resp.id) %>% as_tibble()


# 3 モデルによる推定(13) ----------------------------------------------------------------

# 13. What is the estimated rating for a black bag with matte finish and a
#     gold zipper, priced at $15?
bag.hlm1 %>% fixef()                  # which coefs do we want?
bag.hlm1 %>% fixef() %>% .[c(1, 6)] %>% sum()    # does this total make sense?


# 4 モデルによる推定(14) ----------------------------------------------------------------

# 14. Which respondents are most and least interested in a navy handbag?
bag.hlm1 %>% ranef() %>% glimpse()

# find cutoffs for the top 5% and bottom 5%
bag.hlm1 %>%
  ranef() %>%
  use_series(resp.id) %>%
  use_series(colornavy) %>%
  quantile(pr = 0:20/20)

# get those IDs
which(ranef(bag.hlm1)$resp.id$colornavy < -1.426 | ranef(bag.hlm1)$resp.id$colornavy > 1.7332)
