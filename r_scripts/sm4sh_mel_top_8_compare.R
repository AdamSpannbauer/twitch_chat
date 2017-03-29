rm(list=ls())
library(tidyverse)
library(tidytext)
library(stringr)
library(fuzzyjoin)
source("r_scripts/helper_functions.R")
#vod: https://www.twitch.tv/videos/129767523
# process chat logs to csv
structure_csv_path <- "data/mvg_chat_2017-03-19.csv"
# irssi_to_csv("data/#mvg_league.log",
#              structure_csv_path,
#              date = as.Date("2017-03-19"))

# read in structured chat data
chat_raw_df <- read_csv(structure_csv_path, col_types = "cccccT")

#remove bot mods
chat_df <- chat_raw_df %>% 
  filter(user != "nightbot")

compare_nest_df <- tibble(event = c("melee","sm4sh"),
                           start_time_pre_5 = c("16:22", "19:26"),
                           stop_time_post_5 = c("19:53","22:42")) %>% 
  mutate(event_chat = map2(start_time_pre_5, stop_time_post_5,
                           ~chat_df %>% 
                             filter(time >= .x & time <= .y)))

nest_tokens <- compare_nest_df %>% 
  mutate(token_df = map(event_chat, 
                        ~.x %>% 
                          unnest_tokens(token, msg) %>% 
                          mutate(token = if_else(is.na(token),"",token)) %>% 
                          anti_join(stop_words, by=c("token"="word")) %>% 
                          mutate(token = str_replace_all(token, "[[:punct:]]", "")))) %>% 
  mutate(bigram_df = map(event_chat,
                         ~.x %>% 
                           unnest_tokens(bigram, msg, token = "ngrams", n=2) %>% 
                           mutate(bigram = if_else(is.na(bigram),"",bigram)) %>% 
                           mutate(bigram = str_replace_all(bigram, "[[:punct:]]", "")))) %>% 
  mutate(top_words = map(token_df,
                         ~.x %>% 
                           count(token, sort=T) %>% 
                           head(15))) %>%
  mutate(top_bigrams = map(bigram_df,
                           ~.x %>% 
                             count(bigram, sort=T) %>% 
                             head(15)))

curse_df <- nest_tokens %>% 
  select(event,token_df) %>% 
  unnest() %>% 
  mutate(token_stem = vec_hunspell_stem(token)) %>% 
  mutate(token_stem = if_else(token_stem %in% problem_words_df$problem_words,
                              "",
                              token_stem)) %>% 
  stringdist_left_join(one_word_fcc,
                       by=c("token_stem"="bad_token"),
                       max_dist=1) %>%
  # left_join(one_word_fcc, by=c("token_stem"="bad_token")) %>%
  mutate(curse_flag = if_else(is.na(curse_flag),0,1))

curse_df %>% 
  # filter(curse_flag == 1) %>% 
  group_by(event) %>% 
  summarise(curse_word_count = sum(curse_flag)) %>% 
  ungroup() %>% 
  arrange(desc(curse_word_count))





# curse_df_full <- chat_df %>% 
#   unnest_tokens(token, msg) %>% 
#   mutate(token = if_else(is.na(token),"",token)) %>% 
#   anti_join(stop_words, by=c("token"="word")) %>% 
#   mutate(token_stem = vec_hunspell_stem(token)) %>% 
#   mutate(token_stem = if_else(token_stem %in% problem_words_df$problem_words,
#                               "",
#                               token_stem)) %>% 
#   stringdist_left_join(one_word_fcc,
#                        by=c("token_stem"="bad_token"),
#                        max_dist=1) %>%
#   mutate(curse_flag = if_else(is.na(curse_flag),0,1))
# 
# curse_df_full %>% 
#   # filter(curse_flag == 1) %>% 
#   summarise(curse_word_count = sum(curse_flag)) %>% 
#   ungroup() %>% 
#   arrange(desc(curse_word_count))

# #review potential typo curse candidates
# d <- distinct(curse_df)
# split(d$token_stem, d$bad_token) %>% 
#   map(unique)
# 
# new_problem_words <- tibble(problem_words=c("shia","ship","hit","shirt","suit","shin",
#                             "izz","zuck","muck","puck","huck","tuck","iuck",
#                             "gag","faw","fal","fgg","fah","duke","dice","jum",
#                             "ctm","dick"))
# 
# problem_words_df %>% 
#   bind_rows(new_problem_words) %>% 
#   distinct() %>% 
#   write_csv("data/problem_words_fcc.csv")
