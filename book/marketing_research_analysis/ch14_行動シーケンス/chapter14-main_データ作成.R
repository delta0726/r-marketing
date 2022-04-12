# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 14 行動シーケンス
# Theme     : データ作成
# Created on: 2022/4/13
# Page      : P506 - P514
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 データ確認


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(car)
library(conflicted)

conflict_prefer("some", "car", quiet = TRUE)


# データロード
epa.df.raw <-
  read.table("https://goo.gl/LPqmGb", sep = " ", header = FALSE, stringsAsFactors = FALSE) %>%
    set_names(c("host", "timestamp", "request", "status", "bytes"))

# データ確認
epa.df.raw %>% str()


# 1 データ加工 --------------------------------------------------------------

#
epa.df <-
  epa.df.raw %>%
    mutate(rawhost = host,
           host = factor(paste0("host", as.numeric(factor(rawhost)))),
           datetime = as.POSIXct(strptime(
                      paste0("1995:08:", substr(timestamp, 2, 12)),
                      format = "%Y:%m:%d:%H:%M:%S", tz = "America/New_York")),
           request = sub(" HTTP/1.0", "", request)) %>%
    mutate(reqtype = ifelse(grepl("POST", request), "POST",
                     ifelse(grepl("GET", request), "GET",
                     ifelse(grepl("HEAD", request), "HEAD", ""))),
           reqtype = factor(reqtype)) %>%
    mutate(pagetype = NA,
           pagetype = ifelse(grepl("\\.gif", request), "gif",
                      ifelse(grepl("\\.html", request), "html",
                      ifelse(grepl("\\.pdf", request), "pdf", ""))),
           pagetype = factor(pagetype)) %>%
    mutate(page = request,
           page = str_sub(page, "GET ", ""),
           page = str_sub(page, "HEAD ", ""),
           page = str_sub(page, "POST ", "")) %>%
    mutate(status = ordered(paste0("http", status)),
           bytes = as.numeric(bytes))


# 2 データ確認 -------------------------------------------------------------------------

# データ確認
epa.df %>% as_tibble()
epa.df %>% print()
epa.df %>% summary()
