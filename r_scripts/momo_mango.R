library(tidyverse)
library(tidytext)
library(stringr)
library(fuzzyjoin)
source("r_scripts/helper_functions.R")

#chat log csv
chat_df <- read_csv("data/m2k_chat_2017-03-19.csv",
                    col_types="cccccT")

#stuff for curse words
problem_words_df <- read_csv("data/problem_words_fcc.csv",col_types = "c")
fcc_let_me_be <- read_csv("data/wirty_dords.csv", col_types = "c") %>% 
  filter(!str_detect(word," "))
one_word_fcc  <- fcc_let_me_be %>% 
  mutate(word = vec_hunspell_stem(word)) %>% 
  bind_rows(fcc_let_me_be) %>% 
  mutate(curse_flag = 1) %>% 
  select(bad_token=word, curse_flag) %>% 
  distinct() %>% 
  filter(!(bad_token %in% problem_words_df$problem_words))

#momo mango chat time
time <- chat_df %>% 
  filter(str_detect(msg, regex("^tricot"))) %>% 
  filter(str_detect(msg,"mango")) %>% 
  .[["time"]] %>% 
  min()

#set dataframe
mango_momo_df <- tibble(event= c("set start","end g1","end g2",
                                 "end g3","end g4","end g5"),
                        time = c("16:16", "16:19","16:22",
                                 "16:25","16:28","16:32")) %>% 
  mutate(time_1  = lag(time,1)) %>% 
  filter(!is.na(time_1)) %>% 
  mutate(game=1:5) %>% 
  mutate(winner = c("King Momo","King Momo","Mang0","Mang0","Mang0")) %>% 
  select(game, winner, time_start=time_1, time_stop=time) %>% 
  mutate(chat_data = map2(time_start, time_stop,
                          ~chat_df %>% 
                            filter(time >= .x) %>% 
                            filter(time <  .y))) %>% 
  unnest() %>% 
  mutate(msg = str_replace_all(msg, 
                               regex(paste0("\\b",stop_words$word,"\\b") %>% 
                                       paste(collapse="|")), 
                               ""))

#word df
token_df <- mango_momo_df %>% 
  unnest_tokens(token, msg) %>% 
  mutate(token=ifelse(token=="mango","mang0",token)) %>% 
  mutate(token=ifelse(token=="lol","lul",token)) %>% 
  anti_join(stop_words, by=c("token"="word"))

#top emotes
token_df %>% 
  count(token, sort=TRUE) %>% 
  View()

top_emotes <- c("pogchamp","kkona","peopleschamp","kreygasm")

emotes_by_min <- token_df %>% 
  mutate(is_pog=str_detect(token, "pogchamp"),
         is_lul=str_detect(token, "lul"),
         is_peo=str_detect(token, "peopleschamp"),
         is_krey=str_detect(token, "kreygasm")) %>% 
  mutate(token_stem = vec_hunspell_stem(token)) %>% 
  mutate(token_stem = if_else(token_stem %in% problem_words_df$problem_words,
                              "",
                              token_stem)) %>% 
  stringdist_left_join(one_word_fcc,
                       by=c("token_stem"="bad_token"),
                       max_dist=1) %>%
  # left_join(one_word_fcc, by=c("token_stem"="bad_token")) %>%
  mutate(curse_flag = if_else(is.na(curse_flag),0,1)) %>% 
  group_by(game,winner,time_start,time_stop,
           date,time,datetime) %>% 
  summarise(PogChamp     = sum(is_pog),
            lul          = sum(is_lul),
            PeoplesChamp = sum(is_peo),
            Kreygasm     = sum(is_krey),
            FCC_Curse    = sum(curse_flag)) %>% 
  ungroup() %>% 
  mutate(min_i=row_number()) %>% 
  gather(emote, emote_count, 
         PogChamp, lul, 
         PeoplesChamp, Kreygasm, FCC_Curse) %>% 
  group_by(min_i) %>% 
  mutate(game=as.factor(game))

game_lines <- emotes_by_min %>% 
  group_by(game, winner,
           time_start, time_stop) %>% 
  summarise(i_start = min(min_i),
            i_stop  = max(min_i)) %>% 
  ungroup()

plot_df <- emotes_by_min %>% 
  filter(emote %in% c("PogChamp","lul"))

ggplot(plot_df, aes(x=min_i, y=emote_count, color=emote)) +
  geom_line() + 
  geom_vline(xintercept=game_lines$i_stop) +
  labs(x="Minute in Set",
       y="Count per Minute",
       color="Word") + 
  ggtitle("Mango v King Momo",
          "vertical lines at end of games")

chat_df %>% 
  filter(msg=="the commentary hasn't been that bad lol")

