# ***********************************************************************************************
# Title   : Rによる実践的マーケティングリサーチと分析
# Chapter : 10 確認的因子分析と構造方程式モデル（メイン）
# Theme   : 構造方程式モデル
# Date    : 2022/04/08
# Page    : P348 - P354
# URL     : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞



# ＜目次＞
# 0 準備
# 1 EDAプロセス
# 2 反復勾配モデルの評価
# 3 代替モデルの作成
# 4 モデル比較


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lavaan)
library(semTools)
library(semPlot)
library(car)
library(psych)
library(ggcorrplot)
library(conflicted)

conflict_prefer("some", "car", quiet = TRUE)
conflict_prefer("logit", "car", quiet = TRUE)
conflict_prefer("some", "car", quiet = TRUE)

# データロード
satSimData <- read_csv("data/satSimData.csv")

# データ確認
satSimData %>% print()


# 1 EDAプロセス ------------------------------------------------------------------

# ＜ポイント＞
# - {psych}には有用なEDAプロセスの関数が多く含まれる
#   --- ここではdescribe()とalpha()を確認

# ＜参考＞
# 第6章 心理尺度および心理検査の作成と信頼性・妥当性
# https://cogpsy.educ.kyoto-u.ac.jp/personal/Kusumi/datasem15/tsuda2.pdf


# 基本統計量
satSimData %>% describe()

# 相関分析
satSimData %>%
  cor() %>%
  ggcorrplot(hc.order = TRUE, type = "lower", lab = TRUE)

# アルファ係数の確認
satSimData %>% psych::alpha()


# 2 反復勾配モデルの評価 -------------------------------------------------------------

# モデル構築
satModel <- " Quality =~ CSat + Value + q1 + q2 + q3  + 0*Cost
              Cost    =~ Value + Repeat + c1 + c2 + c3
              Value   =~ CSat + v1 + v2 + v3
              CSat    =~ Repeat + cs1 + cs2 + cs3
              Repeat  =~ r1 + r2 + r3 "

# 学習
sat.fit <- satModel %>% sem(data= satSimData, std.lv=TRUE)

# 確認
sat.fit %>% print()
sat.fit %>% summary(fit.measures = TRUE)
sat.fit %>% listviewer::reactjson(collapsed = TRUE)

# パス図の作成
sat.fit %>%
  semPaths(what = "est", fade = FALSE, residuals = FALSE,
           layout = "tree", structural = TRUE, nCharNodes = 7, edge.label.cex = 1)


# 3 代替モデルの作成 -----------------------------------------------------------------

# モデル定義
satAltModel <- " Quality =~ CSat  + q1 + q2 + q3 + 0*Cost
                 Cost    =~ Value + c1 + c2 + c3
                 Value   =~ CSat  + v1 + v2 + v3
                 CSat    =~ Repeat + cs1 + cs2 + cs3
                 Repeat  =~ r1 + r2 + r3 "

# 学習
satAlt.fit <- satAltModel %>% sem(data = satSimData, std.lv = TRUE)

# 確認
satAlt.fit %>% print()
satAlt.fit %>% summary(fit.measures = TRUE)

# パス図の作成
satAlt.fit %>%
  semPaths(what = "mod", fade = FALSE, residuals = FALSE, layout = "tree",
           structural = TRUE, nCharNodes = 7, fixedStyle=c("white",1))


# 4 モデル比較 -----------------------------------------------------------

# 比較
compareFit(sat.fit, satAlt.fit, nested=TRUE) %>% summary()
