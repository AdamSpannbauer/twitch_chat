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

#these arent functions but have ended up reusing so moved to central place
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
