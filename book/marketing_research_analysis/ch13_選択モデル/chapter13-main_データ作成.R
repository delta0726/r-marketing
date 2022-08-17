# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 13 選択モデル
# Theme     : データ選択
# Created on: 2022/07/28
# Page      : P460 - P465
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - コンジョイントアンケートの作成


# ＜目次＞
# 0 準備
# 1 商品属性の決定
# 2 多変量正規分布のパーツの作成
# 3 アンケートデータの作成


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(MASS)


# 1 商品属性の決定 -------------------------------------------------------------------------

# 商品属性のリスト化
# --- 座席数/荷物スペース/エンジンタイプ/価格
attrib <-
  list(seat = c("6", "7", "8"),
       cargo = c("2ft", "3ft"),
       eng = c("gas", "hyb", "elec"),
       price = c("30", "35", "40"))


# 2 多変量正規分布のパーツの作成 --------------------------------------------------------------

# 係数名の作成
# --- ダミー変数のように1要素を除いて作成
coef.names <- NULL
a <- 1
for (a in seq_along(attrib)) {
  coef.names <- c(coef.names, str_c(names(attrib)[a], attrib[[a]][-1]))
}


# 部分効用値の決定
# --- 顧客が選択を行う際に属性の各水準に抱く潜在的な価値
mu <-
  c(-1, -1, 0.5, -1, -2, -1, -2) %>%
    set_names(coef.names)

# 共分散行列の決定
Sigma <-
  c(0.3, 1, 0.1, 0.3, 1, 0.2, 0.3) %>%
    diag() %>%
    set_colnames(coef.names) %>%
    set_rownames(coef.names) %>%
    inset("enghyb", "engelec", 0.3) %>%
    inset("engelec", "enghyb", 0.3)


# 3 アンケートデータの作成 ----------------------------------------------------------

# 乱数設定
set.seed(33040)

# respondent ids
resp.id <- 1:200
carpool <-
  c("yes", "no") %>%
    sample(size = length(resp.id), replace = TRUE, prob = c(0.3, 0.7))

coefs <-
  resp.id %>%
    length() %>%
    mvrnorm(mu = mu, Sigma = Sigma) %>%
    set_colnames(coef.names)

coefs[carpool == "yes", "seat8"] <- coefs[carpool == "yes", "seat8"] + 2
coefs[carpool == "yes", "seat7"] <- coefs[carpool == "yes", "seat7"] + 1.5

# Try it!: head(cbind(carpool, coefs))
# Try it!: by(coefs, carpool, colMeans)
nques <- 15
nalt <- 3

profiles <- attrib %>% expand.grid()
nrow(profiles)
head(profiles)
profiles.coded <- model.matrix(~seat + cargo + eng + price, data = profiles)[, -1]
head(profiles.coded)


cbc.df <- data.frame(NULL)

for (i in seq_along(resp.id)) {
  profiles.i <- sample(1:nrow(profiles), size = nques * nalt)
  utility <- profiles.coded[profiles.i,] %*% coefs[i,]
  wide.util <- matrix(data = utility, ncol = nalt, byrow = TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs, 1, function(x) sample(1:nalt, size = 1, prob = x))
  choice <- rep(choice, each = nalt) == rep(1:nalt, nques)
  conjoint.i <- data.frame(resp.id = rep(i, nques),
                           ques = rep(1:nques, each = nalt),
                           alt = rep(1:nalt, nques),
                           carpool = rep(carpool[i], nques),
                           profiles[profiles.i,],
                           choice = as.numeric(choice))
  cbc.df <- rbind(cbc.df, conjoint.i)
}


# データ確認
cbc.df %>% print()
cbc.df %>% glimpse()
