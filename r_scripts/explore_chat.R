library(tidyverse)
library(tidytext)
library(gridGraphics)
library(png)

pogchamp <- readPNG("images/PogChamp.png")
poggrob  <- rasterGrob(pogchamp, interpolate=FALSE)
#test grob
# qplot(1:10, 1:10, geom="blank") + 
#   annotation_custom(poggrob, xmin=1, xmax=3, ymin=1, ymax=3)

chat_df <- read_csv("data/bts_chat_2017-03-05.csv",
                    col_types=cols(
                      date      = col_character(),
                      time      = col_character(),
                      user      = col_character(),
                      msg       = col_character(),
                      msg_type  = col_character(),
                      datetime  = col_datetime()
                    ))

chat_df %>% 
  filter(msg == "Mang0 gonna clutch KAPOW")

token_df <- chat_df %>% 
  unnest_tokens(token, msg) %>% 
  anti_join(stop_words, by=c("token"="word"))

bigram_df <- chat_df %>% 
  unnest_tokens(bigram, msg, token = "ngrams", n=2)

token_df %>%
  count(token, sort=TRUE)
# bigram_df %>% 
#   count(bigram, sort=TRUE)

#pogchamp per minute
pogchamp_per_min <- token_df %>% 
  mutate(is_pog = ifelse(token == "pogchamp",1,0)) %>% 
  group_by(date, time, datetime) %>% 
  summarise(pog_count=sum(is_pog)) %>% 
  ungroup() %>% 
  mutate(datetime = strftime(datetime, tz="EST")) %>% 
  mutate(minute_i = row_number())

pogchamp_labels <- pogchamp_per_min %>% 
  filter(str_sub(time, -2) == "00")

max_row <- pogchamp_per_min[which.max(pogchamp_per_min$pog_count),]
ggplot(pogchamp_per_min, aes(x=minute_i, y=pog_count)) + 
  geom_line() +
  labs(x="Time (EST)", y="Pog Count") +
  ggtitle("PogChamps in Chat", 
          subtitle = "Beyond the Summit Spring 2017 Finals Day") +
  annotate("text", 
           x = max_row$minute_i+10, 
           y = max_row$pog_count-4,
           size = 3,
           hjust = 0,
           label = "Hax$ reveals falcon box!") +
  annotation_custom(poggrob, 
                    xmin=max_row$minute_i-10,
                    xmax=max_row$minute_i+10,
                    ymin=max_row$pog_count+8,
                    ymax=max_row$pog_count-8) +
  scale_x_continuous(breaks=pogchamp_labels$minute_i,
                     labels=pogchamp_labels$time)
