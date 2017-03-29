library(tidyverse)
library(tidytext)
library(stringr)
library(hunspell)
library(fuzzyjoin)
library(gridGraphics)
library(png)
library(plotly)

######################
# HELPER FUNCTIONS
######################
##vectorised hunspell stemmer----------------------------------------------------
my_hunspell_stem <- function(token) {
  stem_token <- hunspell::hunspell_stem(token)[[1]]
  if (length(stem_token) == 0) return(token) else return(stem_token[1])
}
vec_hunspell_stem <- Vectorize(my_hunspell_stem, "token")
##function to censor bad words in print------------------------------------------
censor <- function(sailor_vocab) {
  stringr::str_replace_all(tolower(sailor_vocab), 
                           stringr::regex("a|e|i|o|u"),
                           "*")
}
##function extract mode------------------------------------------
censored_word_mode <- function(curse_vec) {
  if(length(curse_vec) == 0) return(NA_character_)
  curse_vec %>% 
    table() %>% 
    .[which(.==max(.))] %>% 
    names() %>% 
    censor() %>% 
    paste(collapse="/")
}

##read in fcc banned words and clean---------------------------------------------
# (keep single word instances for simplicity)
# remove words that were causing problems in fuzzy join
problem_words_df <- read_csv("data/problem_words_fcc.csv",col_types = "c")

fcc_let_me_be <- read_csv("data/wirty_dords.csv", col_types = "c") %>% 
  filter(!str_detect(word," "))
one_word_fcc  <- fcc_let_me_be %>% 
  mutate(word = vec_hunspell_stem(word)) %>% 
  bind_rows(fcc_let_me_be) %>% 
  mutate(curse_flag = 1) %>% 
  select(bad_token=word, curse_flag) %>% 
  distinct() %>% 
  filter(!(bad_token %in% problem_words_df$problem_words)) #were causing problems

chat_df <- read_csv("data/bts_chat_2017-03-05.csv", 
                    col_types= c("cccccT"))

chat_tokens <- chat_df %>% 
  unnest_tokens(token, msg) %>% 
  mutate(token = if_else(is.na(token),"",token)) %>% 
  anti_join(stop_words, by=c("token"="word")) %>% 
  mutate(token_stem = vec_hunspell_stem(token)) %>% 
  mutate(token_stem = if_else(token_stem %in% problem_words_df$problem_words,
                              "",
                              token_stem)) %>% 
  stringdist_left_join(one_word_fcc,
                       by=c("token_stem"="bad_token"),
                       max_dist=1) %>%
  # left_join(one_word_fcc, by=c("token_stem"="bad_token")) %>%
  mutate(curse_flag = if_else(is.na(curse_flag),0,1))

#examine results of fuzzyjoin matches
split(distinct_degen$token_stem, distinct_degen$bad_token)

fcc_by_min <- chat_tokens %>% 
  group_by(date, time, datetime) %>% 
  summarise(curse_count = sum(curse_flag),
            curse_words = list(bad_token[!is.na(bad_token)])) %>%
  ungroup() %>% 
  mutate(top_curse = map_chr(curse_words,censored_word_mode)) %>% 
  mutate(datetime = strftime(datetime, tz="EST")) %>% 
  mutate(minute_i = row_number())

time_labels <- fcc_by_min %>% 
  filter(str_sub(time, -2) == "00") %>% 
  select(minute_i, time)

max_row   <- fcc_by_min[which.max(fcc_by_min$curse_count),]
rage  <- readPNG("images/SwiftRage.png")
ragegrob  <- rasterGrob(rage, interpolate=FALSE)

ggplot(fcc_by_min, aes(x=minute_i, y=curse_count)) + 
  scale_x_continuous(breaks=time_labels$minute_i,
                     labels=time_labels$time) +
  geom_line(color='#6441A4') +
  labs(x="Time (EST)", y="FCC Banned Word Count") +
  ggtitle("FCC Banned Words in Chat per Minute", 
          subtitle = "Beyond the Summit Spring 2017 Finals Day") +
  annotate("text", 
           x = max_row$minute_i-15, 
           y = max_row$curse_count-3,
           size = 5,
           hjust = 1,
           label = "Leffen rested by Hbox\ntwice to lose G5 LF") +
  annotation_custom(ragegrob, 
                    xmin=max_row$minute_i-15,
                    xmax=max_row$minute_i+15,
                    ymin=max_row$curse_count+10,
                    ymax=max_row$curse_count-10)

max_row   <- fcc_by_min[which.max(fcc_by_min$curse_count),]
plot_ly(data=fcc_by_min %>% 
          filter(minute_i>113), 
        x=~minute_i, 
        y=~curse_count,
        type = "scatter",
        mode = "lines",
        line = list(color = '#6441A4')) %>% 
  layout(title="<b>FCC Banned Words in Chat per Minute</b><br><sup>Beyond the Summit Spring 2017 Finals Day</sup>",
         xaxis=list(title="Time (EST)",
                    tickmode="array",
                    tickvals=time_labels$minute_i,
                    ticktext=time_labels$time),
         yaxis=list(title="Banned Words per Minute"),
         margin=list(t=50),
         annotations=list(x = max_row$minute_i-15,
                          y = max_row$curse_count,
                          text = "Leffen rested by Hbox<br>twice to lose G5 LF",
                          xref = "x",
                          yref = "y",
                          showarrow = TRUE,
                          arrowhead = 1,
                          ax = -40,
                          ay = -30),
         images=list(list(source =  "https://static-cdn.jtvnw.net/emoticons/v1/34/2.0",
                          xref = "x",
                          yref = "y",
                          x = 580,
                          y = 61.5,
                          sizex = 30,
                          sizey = 10,
                          sizing = "stretch",
                          layer = "above")))

# chat_df %>% 
#   filter(msg=="Who do you want. To win! Juan or william? Whisper me is you want to win!") %>% 
#   select(time)

degen_frame <- chat_tokens %>%
  filter(curse_flag == 1) %>% 
  mutate(bad_token = censor(bad_token)) %>%
  count(bad_token, sort=T)

distinct_degen <- degen_frame %>%
  select(bad_token, token_stem) %>%
  distinct()

degen_frame %>%
  mutate(bad_token = censor(bad_token)) %>%
  count(bad_token, sort=T)


