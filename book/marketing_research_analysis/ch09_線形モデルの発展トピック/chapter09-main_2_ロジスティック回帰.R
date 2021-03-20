# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（メイン）
# Objective : ロジスティック回帰
# Created by: Owner
# Created on: 2021/02/22
# Page      : P282 - P295
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞



# ＜目次＞
# 0 準備
# 1 基礎知識
# 2 売上データの作成
# 3 モデル構築(モデル1)
# 4 モデル構築(モデル2)
# 5 モデル構築(モデル3)


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(psych)
library(car)
library(vcdExtra)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
# --- 遊園地のシーズンパスを購入するかどうか
pass.df <- read_csv("book/marketing_research_analysis/data/pass_df.csv")

# データ加工
pass.df <-
  pass.df %>%
    mutate(Promo = factor(Promo, levels = c("NoBundle", "Bundle")))

# データ概要
pass.df %>% print()
pass.df %>% glimpse()

# サマリー
pass.df %>% summary()


# 1 基礎知識 ----------------------------------------------------------------------------

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



# 2 売上データの作成 ----------------------------------------------------------------------

# テーブル作成
# --- 売上データ
pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list(Channel=c("Mail", "Park", "Email"),
                           Promo=c("Bundle", "NoBundle"),
                           Pass=c("YesPass", "NoPass") )
# データ確認
pass.tab %>% print()

# テーブルをデータフレームに変換
pass.df <-
  pass.tab %>%
    expand.dft() %>%
    mutate(Promo = factor(Promo, levels = c("NoBundle", "Bundle")))

# データ確認
pass.df %>% glimpse()

# 混合行列
table(pass.df$Pass, pass.df$Promo)


# 3 モデル構築(モデル1) ----------------------------------------------------------------------

# モデル構築
pass.m1 <- glm(Pass ~ Promo, data = pass.df, family = binomial)

# サマリー
pass.m1 %>% tidy()

# 回帰係数
# --- salesのオッズ比
pass.m1 %>% coef() %>% exp()

# 信頼区間
pass.m1 %>% confint() %>% exp()

# 混合行列
table(pass.df$Pass, pass.df$Channel)


# visualization
   # install if needed

pass.df %>% table() %>% doubledecker()


# 参考：オッズ比の算出

# how the coef translates to an odds ratio
plogis(0.3888)                          # outcome %
plogis(0.3888) / (1-plogis(0.3888))     # ratio of outcome % to alternative %
exp(0.3888)                             # identical



# 4 モデル構築(モデル2) ----------------------------------------------------------------------

# モデル構築
pass.m2 <- glm(Pass ~ Promo + Channel, data = pass.df, family = binomial)

# サマリー
pass.m2 %>% tidy()

# 回帰係数
# --- salesのオッズ比
pass.m2 %>% coef() %>% exp()

# 信頼区間
pass.m2 %>% confint() %>% exp()


# 5 モデル構築(モデル3) ----------------------------------------------------------------------

# Model 3: add the interaction of promotion and channel
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel,
               data = pass.df, family = binomial)

# サマリー
pass.m3 %>% tidy()

# 信頼区間
pass.m3 %>% confint() %>% exp()



##########################################################
####
#### ==> this section is NOT in the book
#### extras on visualization for logistic coefficients

# plot the coefs


pass.m2 %>%
  coefplot(intercept = FALSE, outerCI = 1.96, lwdOuter = 1.5,
           title = "Coefficients for Season Pass by Factor", ylab = "Factor")

#### plot the odds ratio confidence intervals
####
pass.ci <- pass.m2 %>% confint() %>% data.frame()     # coef confidence intervals
pass.ci$X50 <- coef(pass.m2)                # add the midpoint estimate

# plot odds

pass.ci$Factor <- rownames(pass.ci)           # for ggplot2 to use in its model
pass.ci

# ggplot of odds ratios
# first: a plot by factor (x=) of the midpoint (y), high (ymax) and low (ymin)
p <- ggplot(pass.ci[-1, ],
            aes(x=Factor, y=exp(X50), ymax=exp(X97.5..), ymin=exp(X2.5..)))

# ... displaying those elements as points & error bars
p <- p + geom_point(size=4) + geom_errorbar(width=0.25)

# ... adding a vertical line at an odds ratio of 1.0 (no change)
p <- p + geom_hline(yintercept=1, linetype="dotted", size=1.5, color="red")

# now plot it with titles
p + ylab("Likehood by Factor (odds ratio, main effect)") +
  ggtitle(paste("95% CI: Card sign up odds by factor")) + coord_flip()


### exercise for reader ==> NOT in book
### does this add anything to our interpretation? Intercept model
pass.m3 <- glm(Pass ~ Promo * Channel, data=pass.df, family=binomial)
summary(pass.m3)
