# データセット読みこみ
 setwd("F:")
houjin <- readRDS("houjin-20200701.rds")
 
# ふりがなでグループ化，ランク集計

library(dplyr)
library(tidyr)
rank_kana <- houjin %>% drop_na(furigana) %>%
  group_by(furigana) %>% 
  summarize( count = n(), percent = count / nrow(.) * 100) %>% 
  arrange(desc(count)) 
