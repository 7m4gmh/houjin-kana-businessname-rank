library(dplyr)
library(openxlsx)
library(tidyverse)
library(grid)
library(ggplot2)

# 頻度と順位の抽出対象を指定
target = "ウメモト"

# データセット読みこみ
# 法人番号情報の全件をRDS形式に変換したもの
# 2020年7月1日現在 4,887,678件
houjin <- readRDS("houjin-20200701.rds")

# このうち，ふりがなの登録のある法人等について，ふりがなでランク集計
rank_kana <- houjin %>% drop_na(furigana) %>%
  group_by(furigana) %>% 
  summarize( count = n(), percent = count / nrow(.) * 100) %>% 
  arrange(desc(count)) %>%
  mutate(min_rank = min_rank(desc(count)), cume_dist = 100 * cume_dist(desc(count)))
# ここまでで  1,043,639種類の商号・名称


# ランクを確認。　「ウメモト」は 17,349位
target_rank = subset(rank_kana, furigana == target) %>%
  select(min_rank) %>% as.numeric()
target_count = subset(rank_kana, furigana == target) %>%
  select(count) %>% as.numeric()
target_label = paste(as.character(target_rank), "  --  ", target)

## 順位等一覧をファイルに保存 (CSVとXSL)
write_csv(rank_kana, "houjin_kana_rank.csv")
write.xlsx(rank_kana, "houjin_kana_rank.xlsx")

# 「ウメモト」の位置を示す図をプロット
ggplot(data=rank_kana) + 
  geom_point(mapping = aes (x = (min_rank), y = (count))) +
  annotate("segment",  
           x = target_rank + 50,  xend = target_rank, 
           y= target_count + 50, yend = target_count,
           arrow=arrow(), colour="blue"
                      ) +
  annotate("text", label = paste(target_rank, " ", target),
           x = target_rank, y = target_count+50)
ggsave("target_rank_normal.png")
ggplot(data=rank_kana) + 
  geom_point(mapping = aes (x = log10(min_rank), y = log10(count))) + 
  annotate("segment",  
           x = 4.5, #log(target_rank ), 
           y = log10(target_count ) + 0.5, 
           xend = log10(target_rank), 
           yend = log10(target_count),
           arrow=arrow(), colour="blue"
  ) +
  annotate("text", label = paste(target_rank, " ", target),
           x = log10(target_rank) + 0.5 , y = log10(target_count) )

#ggplot(data=rank_kana) + 
#  geom_point(mapping = aes (x = log10(min_rank), y = log10(count))) + 
#  geom_spoke(aes(angle = 1, radius = 0.5, x = log10(target_rank), 
#                 y = log10(target_count)), arrow = arrow()) +
#  geom_label(aes(x = log10(target_rank), y = log10(target_count), 
#                 label = paste(target_rank, " ", target)))

ggsave("target_rank_log10.png")

