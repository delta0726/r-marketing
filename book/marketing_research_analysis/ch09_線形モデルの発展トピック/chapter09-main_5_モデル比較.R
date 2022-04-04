# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 9 線形モデルの発展的トピックス（メイン）
# Objective : 階層ベイズモデル
# Created by: Owner
# Created on: 2021/02/22
# Page      : P315 - P319
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 階層モデルは{lme4}を用いた古典的な手法でも推定できるが、本来はベイズ推定のほうが適している
#   --- ベイズ推定は各個体の観測値がほとんどなくても最良の推定値を得ることができる


# ＜目次＞
# 0 準備
# 1 ベイズ線形モデル
# 2 MCMCによる階層線形モデル
# 3 嗜好の分布を調べる


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(broom.mixed)
library(MCMCpack)
library(arm)
library(conflicted)

# コンフリクト解消
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# データ準備
conjoint.df <- read_csv("book/marketing_research_analysis/data/conjoint_df.csv")

# データ加工
conjoint.df <-
  conjoint.df %>%
    mutate(speed = factor(speed),
           height = factor(height))

# データ概要
conjoint.df %>% print()
conjoint.df %>% glimpse()


# 1 ベイズ線形モデル --------------------------------------------------------------------------

# モデル構築
set.seed(97439)
ride.mc1 <- conjoint.df %>% MCMCregress(rating ~ speed + height + const + theme, data = .)

# サマリー
ride.mc1 %>% summary()
ride.mc1 %>% tidy()


# 2 MCMCによる階層線形モデル -------------------------------------------------------------------

# hierarchical lm with MCMC
# WARNING: SLOW! Takes approx. 2 minutes on 2018 Macbook Pro

# モデル構築
set.seed(97439)
ride.mc2 <-
  conjoint.df %>%
    MCMChregress(fixed = rating ~ speed + height + const + theme,
                 random = ~ speed + height + const + theme,
                 group = "resp.id", data = ., r = 8, R = diag(8))

# データ構造
ride.mc2 %>% glimpse()
ride.mc2$mcmc %>% dim()

# overall estimates
ride.mc2$mcmc[ ,1:8] %>% summary()


# 3 嗜好の分布を調べる -------------------------------------------------------------------


# estimates for one respondent (respondent 196)
ride.mc2$mcmc[ , grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE)] %>% summary()

# overall estimates ... again
ride.mc2$mcmc[ ,1:8] %>% summary()

# estimates for wood construction
ride.constWood <-
  ride.mc2$mcmc[ , grepl("b.constWood", colnames(ride.mc2$mcmc))] +
    ride.mc2$mcmc[ , "beta.constWood"] %>%
  summary()


ride.constWood$statistics[,1] %>%
  hist(main = "Preference for Wood vs. Steel",
       xlab = "Rating points",
       ylab = "Count of Respondents",
       xlim = c(-4,4))

# 60 mph 
ride.speed60 <-
  ride.mc2$mcmc[,grepl("b.speed60", colnames(ride.mc2$mcmc))] +
    ride.mc2$mcmc[,"beta.speed60"] %>%
      summary()

ride.speed60$statistics[,1] %>%
  hist(main = "Preference for 60mph vs. 40mph",
       xlab = "Rating points",
       ylab = "Count of Respondents",
       xlim = c(-4, 4))

ride.mc2$mcmc[,c("beta.constWood", "VCV.constWood.constWood",
                   "beta.speed60","VCV.speed60.speed60")] %>%
  summary()


#### Reflections on Model Comparison

# now that we have models from 2 models, we might compare the fixed effects
fix.hlm <- ride.hlm2 %>% fixef()
fix.hb  <- ride.mc2$mcmc[ , 1:8] %>% colMeans()

plot(fix.hlm, fix.hb)
abline(0,1)

# or compare random effects (in this case, for one respondent)
# in general, would want to compare full coefficients (fixed + random)
#
# but in this case, the fixed are nearly identical between the two,
# so we'll omit those for convenience
#
# LME random effects for ID #196
ranef(ride.hlm2)$resp.id[196, ]

# MCMC random effects for ID #196
colMeans(ride.mc2$mcmc[ , grepl(".196", colnames(ride.mc2$mcmc), 
                                fixed=TRUE)])

# compare them graphically: 
# .. plot the distribution of the MCMC draws of the random effects for ID 196
# .. and then add distribution for the LME random effects for ID 196
# .. doing this for only the first 4 of the 7 non-intercept parameters


par(mfrow=c(2,2))       # make a 2x2 plot surface
plot.xlim <- c(-3, 3)   # define limits for the x-axis

for (i in 2:5) {        # first four parameters only, for convenience
  # plot the MCMC density for random effect i
  mcmc.col <- which(grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE))[i]
  plot(density(ride.mc2$mcmc[ , mcmc.col]), xlab="", 
       ylim=c(0, 1.4), xlim=plot.xlim,
       main=paste("HB & lmer density:",
                  colnames(ride.mc2$mcmc)[mcmc.col] ))
  # add the HLM density for random effect i
  hlm2.est <- ranef(ride.hlm2)$resp.id[196, i]               # mean estimate
  hlm2.sd <-  sqrt(attr(ranef(ride.hlm2, condVar=TRUE)$resp.id, 
                        "postVar")[ , , 196][i, i])
  seq.pts <- seq(from=plot.xlim[1], to=plot.xlim[2], length.out=1000) # range
  # .. find density at x-axis points using dnorm() and add that to the plot
  points(seq.pts, dnorm(seq.pts, mean=hlm2.est, sd=hlm2.sd), 
         col="red", pch=20, cex=0.4)                      
  legend("topright", legend=c("red = lmer", "black = HB"), 
         text.col=c("red", "black"))
}


###
### BEGIN: NOT in the book
###
### Comparing the respondents' deviations between models more systematically
###

med.diff <- rep(NA, 8)     # median difference between models on each parameter
# WARNING: next part is sloow, takes 20-60 seconds
for (i in 1:8) {           # loop over the parameters
  hlm.diff <- rep(NA, 200) # difference between the models for each respondent (for parameter i)  
  
  for (j in 1:200) {       # loop over the respondents
    # find the MCMC mean for random effect of parameter i, respondent j
    mcmc.col <- which(grepl(paste0(".", j), colnames(ride.mc2$mcmc), fixed=TRUE))[i]
    mcmc.est <- mean(ride.mc2$mcmc[ , mcmc.col])
    # find the lme4 estimate of the random effect
    hlm2.est <- ranef(ride.hlm2)$resp.id[j, i]
    hlm.diff[j] <- mcmc.est - hlm2.est
  }
  print (i)
  print(summary(hlm.diff))     # diffs for parameter i
  med.diff[i] <- median(hlm.diff)  # save the median of those diffs
}
summary(med.diff)              # summary of the 8 median diffs

### END: not in book
