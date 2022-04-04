# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（エクササイズ）

# Created by: Owner
# Created on: 2021/02/22
# Page      :
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************




# ＜目次＞
# 0 準備
# ***** 多重共線性 ************
# 1 線形モデルによる予測
# 2 データ変換によるモデルの変化
# 3 高相関の特徴量をPCAで合成
# 4 コードなしの演習
# ***** ロジスティック回帰 ************


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(car)
library(psych)
library(forecast)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
sales.data.raw <- read_csv("book/marketing_research_analysis/data/chapter9-sales.csv")

# データ確認
sales.data.raw %>% print()
sales.data.raw %>% glimpse()

# サマリー
sales.data.raw %>% summary()


# 1 線形モデルによる予測 --------------------------------------------------------------------

# プロット
# --- 正規性のないデータも含まれる
sales.data.raw %>% pairs.panels()


# モデル構築
# --- とりあえず全変数でモデル化
mod.raw1 <- lm(spendMonth ~ ., sales.data.raw)

# サマリー
# --- 回帰係数で全体的にp値が高い
mod.raw1 %>% glance()
mod.raw1 %>% tidy() %>% mutate(p.value = round(p.value, 3))

# VIFの確認
# --- 5を超える特徴量が含まれている
# --- 多重共線性問題の疑いあり
mod.raw1 %>% vif()


# 修正版のモデル ********************************

# モデル再構築
# --- VIFが5を超える特徴量を削除
mod.raw2 <- lm(spendMonth ~ . - satSite - satPrice, sales.data.raw)

# サマリー
mod.raw2 %>% glance()
mod.raw2 %>% tidy() %>% mutate(p.value = round(p.value, 3))

# VIFの確認
# --- 5を超える特徴量が含まれている
# --- 多重共線性問題の疑いあり
mod.raw2 %>% vif()


# 2 データ変換によるモデルの変化 ------------------------------------------------

# 関数定義
autoTransform <- function(x) {
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

# データ変換
sales.data <-
  sales.data.raw %>%
    mutate_if(is.numeric, autoTransform)

# データ確認
sales.data %>% print()
sales.data %>% describe()

# モデル構築
mod.tr1 <- sales.data %>% lm(spendMonth ~ . - satSite - satPrice, data = .)

# サマリー
mod.tr1 %>% glance()
mod.tr1 %>% tidy()

# VIFの確認
mod.tr1 %>% vif()


# 3 高相関の特徴量をPCAで合成 ---------------------------------------------------

# 相関係数行列
# --- 満足関連指標(sat*)は相関が高い
sales.data %>% select(satSite:satOverall) %>% cor()

# 主成分分析
# --- PC1で大半が説明できる
sat.pc <- sales.data %>% select(satSite:satOverall) %>% prcomp()
sat.pc %>% summary()

# モデル構築
# --- PCAファクターを導入
mod.tr2 <-
  sales.data %>%
    mutate(satPC = sat.pc$x[ , 1]) %>%
    select(-starts_with("sat")) %>%
    lm(spendMonth ~ . , data = .)

# サマリー
mod.tr2 %>% glance()
mod.tr2 %>% tidy()


# 4 コードなしの演習 --------------------------------------------------------------------------

# 4. [thought exercise without code]. When the model is fit with region as a
#    predictor, it may show the West region with a large, possibly even the
#    largest, effect. Yet it is not statistically significant whereas smaller
#    effects are. Why could that be?


###
### Logistic Regression
###

# 5. Using logistic regression, what is the relationship between the coupon
#    being sent to some customers and whether the purchased the promoted product?
sales.data$purchase <- sales.data.raw$purchase
purchase.lr1 <- glm(purchase ~ coupon, data=sales.data, family=binomial)
summary(purchase.lr1)

# 6. How does that model change if region, satisfaction, and total spending are
#    added to the model?
purchase.lr2 <- glm(purchase ~ coupon + spendToDate + region + satPC, 
                    data=sales.data, family=binomial)
summary(purchase.lr2)

# 7. Is there an interaction between the coupon and satisfaction, in their 
#    relationship to purchase of the promoted product?
purchase.lr3 <- glm(purchase ~ coupon * satPC, 
                    data=sales.data, family=binomial)
summary(purchase.lr3)

# 8. What is the best estimate for how much a coupon is related to increased purchase, as
#    an odds ratio?
plogis(0.342) / 0.5   # using model 3


## EXTRAS not in book
##
# x1. What is the change in purchase likelihood, in relation to a change of 1 
#    unit of satisfaction? (Hint: what is a unit of satisfaction?) Approximately 
#    how many points would ``1 unit'' be, on the actual 1-10 rating scales?)
plogis(-0.64) / 0.5  # using model 3

# 1 unit is 1 sd, so look at the sd in the raw data
library(psych)   
describe(sales.data.raw[ , 5:8])

# x2. [thought exercise] For product strategy, what questions are suggested by 
#     this relationship between satisfaction and purchase? What possible 
#     explanations are there, or what else would you wish to know?
##
## END EXTRAS



####
#### Metric Conjoint and Hierarchical Linear Models
####

# conjoint.df <- read.csv("chapter9-bag.csv") # local
conjoint.df <- read.csv("https://goo.gl/gEKSQt") # online
summary(conjoint.df)

# 11. Using the handbag data, estimate the likelihood to purchase as a 
# function of the handbags' attributes, using a simple linear model
bag.lm <- lm(rating ~ price + color + zipper + finish, data=conjoint.df)
summary(bag.lm)

# 12. Now fit the same model as a classical hierarchical model, with 
#     individual level estimates for each attribute's utility.
library(lme4)
# model with random intercept & slope by respondent = (predictors | resp.id)  
bag.hlm1 <- lmer(rating ~ price + color + zipper + finish + 
                   (price + color + zipper + finish | resp.id),
                 data=conjoint.df,
                 control=lmerControl(optCtrl=list(maxfun=100000)))

# population estimate
fixef(bag.hlm1)


# 13. What is the estimated rating for a black bag with matte finish and a
#     gold zipper, priced at $15?
fixef(bag.hlm1)                  # which coefs do we want?
sum(fixef(bag.hlm1)[c(1, 6)])    # does this total make sense?

# 14. Which respondents are most and least interested in a navy handbag?
str(ranef(bag.hlm1))
# find cutoffs for the top 5% and bottom 5%
quantile(ranef(bag.hlm1)$resp.id$colornavy, pr=0:20/20)
# get those IDs
which(ranef(bag.hlm1)$resp.id$colornavy < -1.426 | ranef(bag.hlm1)$resp.id$colornavy > 1.7332)


####
####  Hierarchical Bayes linear model, metric conjoint analysis
####

# 15. Fit the hierarchical model again, using a Bayesian MCMC approach. How do
#     the upper level estimates compare with those from the classical model?

# hierarchical lm with MCMC
# WARNING: SLOW! Takes approx. 3 minutes on 2018 Macbook Pro
#
set.seed(97439)
library(MCMCpack)
bag.mc <- MCMChregress(fixed = rating ~ price + color + zipper + finish, 
                         random = ~ price + color + zipper + finish, 
                         group="resp.id", data=conjoint.df, r=6, R=diag(6))

str(bag.mc)

# overall estimates
summary(bag.mc$mcmc[ ,1:6])


# EXTRA not in book
# x3. In the MCMC results, what is the distribution of preference for a navy bag?
# estimates for Navy color
bag.colornavy <- summary(bag.mc$mcmc[ , grepl("b.colornavy", 
                                                colnames(bag.mc$mcmc))] 
                          + bag.mc$mcmc[ , "beta.colornavy"])

hist(bag.colornavy$statistics[,1], 
     main="Preference for Navy", 
     xlab="Rating points", ylab="Count of Respondents", xlim=c(-4,4),
     breaks = 40)

