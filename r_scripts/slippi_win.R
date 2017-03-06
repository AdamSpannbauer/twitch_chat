library(tidyverse)
library(forcats)
library(jsonlite)
library(xgboost)
library(glmnet)

######################
# read slippi stats from smash.gg api (commented out to prevent api abuse)
######################
# base_url <- "https://api.smash.gg/slippi/getBySet/"
# 
# id_path  <- "data/slippi_data"
# id_files <- list.files(id_path, full.names = TRUE)
# 
# set_meta_df <- map(id_files, read_csv) %>% 
#   bind_rows() %>% 
#   select(id     = `Set ID`,
#          p1     = `Player 1`,
#          p2     = `Player 2`,
#          winner = Winner) %>% 
#   mutate(url = paste0(base_url, id))
# 
# set_df <- set_meta_df %>% 
#   mutate(summary_data = map(url, function(.x) {
#     cat(.x, "\n")
#     json_list <- read_lines(.x) %>% 
#       fromJSON(simplifyVector = TRUE) %>% 
#       .[['summary']] %>% 
#       map(function(.x) {
#         .x$prefix <- NULL
#         .x
#       }) 
#     do.call(bind_rows, json_list)
#   }))
# 
# set_df_unnest <- set_df %>% 
#   unnest() %>% 
#   mutate(won = if_else(gamerTag == winner, 1, 0))
# 
# model_data_df <- set_df_unnest %>% 
#   select(-id, -p1, -p2, -winner, -url)
# 
# write_csv(model_data_df, "data/slippi_data/model_df.csv")
#--------------------------------------------------------------------------------

######################
# xgboost for var importance
######################
mod_df <- read_csv("data/slippi_data/model_df.csv")
x <- mod_df %>% 
  select(-won, -gamerTag) %>% 
  as.matrix()
y <- mod_df %>% 
  select(won) %>% 
  as.matrix()

n_rounds  <- 2000
eta       <- .03
xg_params <- list(booster="gbtree",
                  max_depth=5,
                  eta=eta,
                  subsample=1,
                  colsample_bytree=1)

set.seed(42)
xgb_mod <- xgboost(x, 
                   y,
                   params  = xg_params,
                   nrounds = n_rounds,
                   verbose = 1,
                   print_every_n = 500,
                   missing = NA)

importance_df <- xgb.importance(colnames(x), model = xgb_mod) %>%
  arrange(desc(Gain))
ggplot(importance_df, aes(x=fct_reorder(Feature, Gain), y=Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Importance in Predicting Set Win", 
       subtitle="Based on Slippi Data from Smash the Summit Spring 2017",
       x=NULL)
#--------------------------------------------------------------------------------

######################
# lasso logistic regression for effect direction
######################
x <- mod_df %>% 
  select(-won, -gamerTag) %>% 
  as.matrix()
y <- mod_df %>% 
  select(won) %>% 
  as.matrix()

glm_mod <- cv.glmnet(x, 
                     y=as.factor(y), 
                     alpha=1, 
                     family="binomial")

min_lambda <- glm_mod$lambda.min
coef_mat <- glm_mod %>%
  coef() %>% 
  as.matrix()

glm_res_df <- tibble(stat  = rownames(coef_mat),
                 coeff = coef_mat[,1]) %>% 
  mutate(direction = ifelse(coeff < 0, "Negative Effect on Win", NA)) %>% 
  mutate(direction = ifelse(coeff > 0, "Positive Effect on Win", direction)) %>% 
  mutate(direction = ifelse(coeff == 0, "dropped", direction))

ggplot(glm_res_df %>% 
         filter(stat != '(Intercept)') %>% 
         filter(direction != "dropped"), 
       aes(x=fct_reorder(stat, abs(coeff)), y=coeff,
           fill=direction)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Effect on Predicting Set Win", 
       subtitle="Based on Slippi Data from Smash the Summit Spring 2017",
       x=NULL,
       y="Lasso Logistic Regression Coefficient") +
  theme(legend.position="none")
#--------------------------------------------------------------------------------

######################
# combine results and plot
######################
importance_df_direction <- importance_df %>% 
  inner_join(glm_res_df, by=c("Feature"="stat")) %>% 
  rename(Effect = direction) %>% 
  filter(Effect != "dropped")

ggplot(importance_df_direction, aes(x=fct_reorder(Feature, Gain), y=Gain, fill=Effect)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Importance in Predicting Set Win", 
       subtitle="Based on Slippi Data from Smash the Summit Spring 2017",
       x=NULL) +
  theme(legend.title=element_blank())
#--------------------------------------------------------------------------------