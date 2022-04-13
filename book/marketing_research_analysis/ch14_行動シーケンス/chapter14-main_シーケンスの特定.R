# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 14 行動シーケンス
# Theme     : 3 シーケンスを特定する
# Created on: 2022/4/14
# Page      : P518 - P525
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - シーケンスデータを扱う際は何をシーケンスとみなすかを決める必要がある
#   --- EDAプロセス


# ＜目次＞
# 0 準備
# 1 order関数を使ったソート
# 2 データ加工
# 3 セッションデータの確認


# 0 準備 -------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(scales)
library(car)
library(conflicted)

conflict_prefer("filter", "dplyr", quiet = TRUE)


# データロード
epa.df <- read_csv("data/epa_df.csv")

# データ確認
epa.df %>% print()
epa.df %>% glimpse()


# 1 order関数を使ったソート ---------------------------------------------------

# ベクトル定義
x <- c(22, 46, 66, 11, 33, 55)

# 順序の抽出
x %>% order()

# ソート
x[order(x)]
x %>% sort()


# 2 データ加工 ---------------------------------------------------

# ＜ポイント＞
# - 単なるデータ加工なので省略
#  --- 集計後のデータをepa.ordered.csvとして保存


# put DF in order of host and timestamp
epa.ordered <- epa.df[order(epa.df$host, epa.df$datetime), ]

# get time differences between rows in minutes
epa.ordered$time.diff <-
  c(NA,
    as.numeric(
      epa.ordered$datetime[2:nrow(epa.ordered)] -
      epa.ordered$datetime[1:(nrow(epa.ordered)-1)],
      units="mins")
    )

# then determine new sessions, as being either:
# .. 1: host has changed since previous row
# .. 2: time difference exceeds session cutoff time

session.time              <- 15   # exceed (mins) ==> new session
epa.ordered$newsession    <- NA   # is this row a new session?
epa.ordered$newsession[1] <- TRUE # row 1 is always a new session

epa.ordered$newsession[2:nrow(epa.ordered)]  <-
  ifelse(epa.ordered$host[2:nrow(epa.ordered)] !=
           epa.ordered$host[1:(nrow(epa.ordered)-1)],   # hosts are different
         TRUE,                                          # so diff session
         epa.ordered$time.diff[2:nrow(epa.ordered)] >=
           session.time )                               # else base on time

# what do we have so far?
epa.ordered[1:20, c("host", "datetime", "newsession")]

epa.ordered$session <- cumsum(epa.ordered$newsession)
epa.ordered$time.diff[epa.ordered$newsession] <- NA  # time NA for new sess
epa.ordered[1:100, c(1, 7, 11:13)]


# 3 セッションデータの確認 -----------------------------------------------------------

# セッション統計
epa.ordered$newsession %>% sum()
epa.ordered$time.diff %>% is.na() %>% sum()
epa.ordered$session %>% max()

# リクエスト割合
nrow(epa.ordered) / sum(epa.ordered$newsession)

# what is the distribution of session lengths, in total requests?
session.length <- epa.ordered$session %>% rle() %>% .$lengths

# データ確認
session.length %>% table()
session.length %>% summary()

# プロット作成
session.length %>% table() %>% plot()
