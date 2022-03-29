# ***********************************************************************************************
# Title     : Rによる実践的マーケティングリサーチと分析
# Chapter   : 11 セグメンテーション：クラスタリングと分類（メイン）
# Objective : クラスタリング
# Created by: Owner
# Created on: 2021/02/22
# Page      : P371 - P422
# URL       : http://r-marketing.r-forge.r-project.org/Instructor/slides-index.html
# ***********************************************************************************************



# ＜目次＞
# 0 準備
# 1 関数定義
# 2 データ分割
# 3 ナイーブベイズ(学習)
# 4 ナイーブベイズ(モデル評価)
# 5 ランダムフォレスト(学習)
# 6 ランダムフォレスト(モデル評価)
# 7 ランダムフォレスト(変数重要度)
# 8 予測：潜在的な顧客の特定


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(yardstick)
library(cluster)
library(mclust)
library(e1071)
library(gplots)
library(RColorBrewer)
library(randomForest)
library(psych)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("map", "purrr")

# データ準備
# --- セグメンテーションデータ
seg.raw <-
  read_csv("data/seg_raw.csv") %>%
    mutate_if(is.character, as.factor)


# 1 関数定義 ----------------------------------------------------------------------------

# 関数定義
# --- 平均集計（数値データのみ）
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

# 関数定義
# --- ランダムフォレストのデンドログラム変換
to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
  if(dfrep[rownum,'status'] == -1){
    rval <- list()
    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE
  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)
    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
  }
  class(rval) <- "dendrogram"
  return(rval)
}

# 2 データ分割 ----------------------------------------------------------------------------

# データ分割
set.seed(04625)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw) * train.prop)
seg.df.train <- seg.raw %>% slice(train.cases)
seg.df.test  <- seg.raw %>% slice(-train.cases)

# データ確認
seg.df.train %>% dim()
seg.df.test %>% dim()
seg.df.train$Segment %>% table()


# 3 ナイーブベイズ(学習) ------------------------------------------------------------------------

# モデル構築
seg.nb <- naiveBayes(Segment ~ ., data = seg.df.train)

# 予測
seg.nb.class <- seg.nb %>% predict(seg.df.test)

# テーブル集計
# --- 割合ベース
seg.nb.class %>% table() %>% prop.table()

# プロット作成
seg.df.test[, -7] %>%
  clusplot(seg.nb.class, color = TRUE, shade = TRUE,
         labels = 4, lines = 0, main = "Naive Bayes classification, holdout data")


# 4 ナイーブベイズ(モデル評価) ------------------------------------------------------------------

# Accuracy
mean(seg.df.test$Segment == seg.nb.class)

# ランダムインデックス
seg.nb.class %>% adjustedRandIndex(seg.df.test$Segment)

# 混合行列
# --- マルチクラス
table(seg.nb.class, seg.df.test$Segment)

# カテゴリ集計
# --- Segment(実際の値)
# --- Segment(予測値)
seg.df.test %>% seg.summ(seg.nb.class)
seg.df.test %>% seg.summ(seg.df.test$Segment)

# クラス確率の推定
seg.nb %>% predict(seg.df.test, type = "raw") %>% as_tibble()


# 5 ランダムフォレスト(学習) ----------------------------------------------------------------

# モデル構築
set.seed(98040)
seg.rf <- randomForest(Segment ~ ., data = seg.df.train, ntree = 3000, importance = TRUE)
seg.rf %>% print()

# デンドログラム作成
# --- ツリー番号を指定
seg.rf %>%
  getTree( 987, labelVar = TRUE) %>%
  to.dendrogram() %>%
  plot(center = TRUE, edgePar = list(t.cex = 0.8, p.col = NA, p.lty = 0),
       main = "Example: Tree 987", yaxt = "n")

seg.rf %>%
  getTree( 1245, labelVar=TRUE) %>%
  to.dendrogram() %>%
  plot(center = TRUE, edgePar = list(t.cex = 0.8, p.col = NA, p.lty = 0),
       main = "Example: Tree 1245", yaxt = "n")

# 予測
seg.rf.class <- seg.rf %>% predict(seg.df.test)
seg.rf.class %>% length()

# クラスタープロット
seg.df.test %>%
  select(-Segment) %>%
  clusplot(seg.rf.class, color = TRUE, shade = TRUE,
           labels = 4, lines = 0,
           main = "Random Forest classification, holdout data")


# 参考 ***********************************************

# 予測
# --- 個別予測の分布も取得
seg.rf.class.all <- seg.rf %>% predict(seg.df.test, predict.all = TRUE)
seg.rf.class.all %>% glimpse()

# look at the distribution for the first 5 test data cases
seg.rf.class.all$individual[1:5, ] %>%
  apply( 1, function(x) prop.table(table(x)))


# 6 ランダムフォレスト(モデル評価) ------------------------------------------------------------

# Accuracy
mean(seg.df.test$Segment == seg.rf.class)

# 混合行列
table(seg.df.test$Segment, seg.rf.class)

# ランダムインデックス
seg.df.test$Segment %>%
  adjustedRandIndex(seg.rf.class)

# カテゴリ集計
# --- Segment(実際の値)
# --- Segment(予測値)
seg.df.test %>% seg.summ(seg.rf.class)
seg.df.test %>% seg.summ(seg.df.test$Segment)


# 7 ランダムフォレスト(変数重要度) ----------------------------------------------------------------

# 変数重要度
seg.rf %>% importance()

# 変数重要度プロット
seg.rf %>%
  varImpPlot(main = "Variable importance by segment")

# 変数重要度プロット
# --- ヒートマップ
importance(seg.rf)[ , 1:4] %>%
  t() %>%
  heatmap.2(col = brewer.pal(9, "Blues"), dend = "none",
            trace = "none", key = FALSE, margins = c(10, 10),
            main = "Variable importance by segment")


# 8 予測：潜在的な顧客の特定 ----------------------------------------------------------------

# ＜ポイント＞
# - subscribeを教師データとしてランダムフォレストの分類問題でアプローチ


# データ分割
set.seed(92118)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.df), nrow(seg.df) * train.prop)
sub.df.train <- seg.raw[train.cases, ]
sub.df.test  <- seg.raw[-train.cases, ]

# データ確認
# --- 不均衡ラベルであることを確認
sub.df.train$subscribe %>% table()
sub.df.train$subscribe %>% table() %>% prop.table()

# クラスタープロット
sub.df.train %>%
  select(-Segment) %>%
  clusplot(sub.df.train$subscribe, color = TRUE, shade = TRUE,
           labels = 4, lines = 0, main = "Subscriber clusters, training data")

# 学習
# --- クラス不均衡あり
# --- クラス不均衡に対応（サンプル数を指定）
set.seed(11954)
sub.rf <- sub.df.train %>% randomForest(subscribe ~ ., data = ., ntree = 3000)
sub.rf <- sub.df.train %>% randomForest(subscribe ~ ., data = ., ntree = 3000, sampsize = c(23, 23))

# 予測
sub.rf.sub <- sub.rf %>% predict(sub.df.test)

# 混合行列
sub.rf.sub %>% table(sub.df.test$subscribe)

# ランダムインデックス
sub.rf.sub %>% adjustedRandIndex(sub.df.test$subscribe)

sub.rf.sub %>%
  cbind(sub.df.test$subscribe) %>%
  cohen.kappa()
