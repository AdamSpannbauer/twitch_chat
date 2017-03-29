library(tidyverse)
library(httr)
library(uuid)

##########################
# SUPER SECRET KEYS
##########################
key_1 <- "ac0ead5d1b4c442b85baf77ae956d88e"
key_2 <- "9245e64788e24186837d17e2c75995f7"


##########################
# GET JSON WEB TOKEN FOR RECOGNITION REQUEST
##########################
jwt_endpoint <- "https://api.cognitive.microsoft.com/sts/v1.0/issueToken"

header_list <- list(
  `Ocp-Apim-Subscription-Key` = key_1
)

h <- do.call(add_headers, header_list)

jwt_response <- POST(jwt_endpoint,h)
access_token <- content(jwt_response)


##########################
# CALL FOR SPEACH TO TEXT API
##########################
speech_endpoint_base <- "https://speech.platform.bing.com/recognize"

#gen params
os          <- Sys.info()[['sysname']]
request_id  <- UUIDgenerate()
instance_id <- if(Sys.info()[['nodename']] == "CND6284H9M-A") {
  "aab6c804-0fde-11e7-b597-fbf721086e80"
}

params <- list(
  scenarios  = "websearch",
  appid      = "D4D52672-91D7-4C74-8AD8-42B1D98141A5",
  locale     = "en-US",
  device.os  = os,
  version    = "3.0",
  format     = "xml",
  requestid  = request_id,
  instanceid = instance_id
)

param_str <- paste0(names(params), "=", params) %>% 
  paste(collapse="&")

#full url
speech_endpoint <- paste0(speech_endpoint_base,
                          "?",
                          param_str)

header_list <- list(
  Authorization  = paste(access_token, collapse="")#,
  # `Content-Type` = 
)

h <- do.call(add_headers, header_list)

response <- POST(speech_endpoint,h)

content(response)
