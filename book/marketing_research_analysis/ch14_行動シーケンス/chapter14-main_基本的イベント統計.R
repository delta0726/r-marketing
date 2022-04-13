# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 14 行動シーケンス
# Theme     : 2 基本的イベントの統計
# Created on: 2022/4/14
# Page      : P514 - P518
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************


# ＜概要＞
# - 行動について分析する際にはイベント頻度をまず確認する
#   --- 単純なEDAプロセス


# ＜目次＞
# 0 準備
# 1 イベント件数のカウント
# 2 時間ごとのイベント数
# 3 エラー件数の確認
# 4 混雑具合の調査


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


# 1 イベント件数のカウント -------------------------------------------------------

# 上位ページの表示
# --- pageの文字数が多いためtibbleで表示
epa.df %>%
  group_by(page) %>%
  tally() %>%
  arrange(desc(n))

# 上位ページの表示
# --- HTML
epa.df %>%
  filter(pagetype == "html") %>%
  group_by(page) %>%
  tally() %>%
  arrange(desc(n))


# 2 時間ごとのイベント数 ----------------------------------------------------

# 密度プロット
# --- 時間をX軸とすることで時間に対する密度が表現できる
epa.df %>%
  ggplot(aes(x = datetime)) +
  geom_density()

# 密度プロット
# --- 装飾あり
epa.df %>%
  ggplot(aes(x = datetime, fill = I("lightblue"))) +
  geom_density(alpha = 0.5, bw = "SJ-dpi", adjust = 2.0) +
  scale_x_datetime(breaks = date_breaks("2 hours"),
                   date_labels = "%b %d %H:%M") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("HTTP Requests (proportion)") +
  xlab("Date / Time")


# 3 エラー件数の確認 ----------------------------------------------------------

# which pages had errors ?
err.page <-
  epa.df %>%
    filter(status >= "http400") %>%
    select(status, page)

# 確認
err.page %>% print()
err.page %>% arrange(desc(page))


# 4 混雑具合の調査 ---------------------------------------------------------------

# アクティブユーザー数
epa.df$host %>% unique() %>% length()

# データ集計
# --- ユーザーごとの頻度
host.tab <-
  epa.df$host %>%
    table() %>%
    tibble(host = names(.),
           freq = .) %>%
    arrange(desc(freq))

# プロット
# --- 上位ユーザーのアクセスが集中
host.tab %>%
  head(100) %>%
  ggplot(aes(x = reorder(host, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

# 上位データの確認
host.tab %>% head(10)
