library(tidyverse)
library(stringr)

irssi_to_csv("data/mew2king_2017-03-19.log", 
             "data/m2k_chat_2017-03-19.csv", 
             as.Date("2017-03-19"))

irssi_to_csv <- function(log_file_in, csv_out, date=Sys.Date(), verbose=TRUE) {
  #read data
  if(verbose) cat("reading data...\n")
  log_lines <- readr::read_lines(log_file_in)
  
  # rm lines with log open/close data
  if(verbose) cat("trimming lines...\n")
  meta_lines     <- stringr::str_detect(log_lines, regex('^--- Log|^\\d\\d:\\d\\d -!- '))
  log_lines_trim <- log_lines[!meta_lines]
  
  # 2 cases to clean
  # cases <- c("15:31  * primzed PRESS Kreygasm FOR PLUPBALLZ PogChamp",
  #            "15:56 < iconicivan> vish ResidentSleeper")
  
  if(verbose) cat("extracting message data & converting to df...\n")
  #use regex to capture time, user type (as * or <), username, msg text
  cleaned_lines_df <- str_match(log_lines_trim, regex('(^\\d\\d:\\d\\d)\\s+(\\*|\\<)\\s+(\\w+)(.+)')) %>% 
    dplyr::as_data_frame() %>% 
    purrr::set_names(c("raw","time","msg_type","user","msg")) %>% 
    dplyr::mutate(date = date) %>% 
    #drop raw input column
    dplyr::select(date, time, user, msg, msg_type) %>% 
    # rm > at beginning of messages
    dplyr::mutate(msg = str_replace_all(msg, regex("^\\>"), ""))
  
  #trim whitespace all cols
  cleaned_lines_df[] <- purrr::map(cleaned_lines_df, trimws)
  cleaned_lines_df$datetime <- strptime(paste(cleaned_lines_df$date, 
                                              cleaned_lines_df$time), 
                                        format="%Y-%m-%d %H:%M")
  # cleaned_lines_df
  #save clean chat
  if(verbose) cat("saving df to csv...\n")
  readr::write_csv(cleaned_lines_df, csv_out)
}
