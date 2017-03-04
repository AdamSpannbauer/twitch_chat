library(tidyverse)
library(stringr)

#read data
log_lines <- read_lines("data/#beyondthesummit.log")

# rm lines with log open/close data
meta_lines     <- str_detect(log_lines, regex('^--- Log|^\\d\\d:\\d\\d -!- '))
log_lines_trim <- log_lines[!meta_lines]

# 2 cases to clean
# cases <- c("15:31  * primzed PRESS Kreygasm FOR PLUPBALLZ PogChamp",
#            "15:56 < iconicivan> vish ResidentSleeper")

#use regex to capture time, user type (as * or <), username, msg text
cleaned_lines_df <- str_match(cases, regex('(^\\d\\d:\\d\\d)\\s+(\\*|\\<)\\s+(\\w+)(.+)')) %>% 
  as.data.frame() %>% 
  set_names(c("raw","time","user_type","user","msg")) %>% 
  #drop raw input column
  select(-raw) %>% 
  # rm > at beginning of messages
  mutate(msg = str_replace_all(msg, regex("^\\>"), ""))

#trim whitespace all cols
cleaned_lines_df[] <- map(cleaned_lines_df, trimws)

#save clean chat
write_csv(cleaned_lines_df, "data/chat_log.csv")
