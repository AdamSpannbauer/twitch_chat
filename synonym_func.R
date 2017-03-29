library(magrittr)

get_syns_df <- function(url) {
  selector <- 
    "div.filters > div.relevancy-block > div.relevancy-list > ul > li > a"
  syn_nodes <- xml2::read_html(url) %>% 
    rvest::html_nodes(selector)
  syns <- rvest::html_attr(syn_nodes, "href") %>% 
    stringr::str_split("/") %>% 
    purrr::map_chr(~tail(.x,1) %>% 
                     URLdecode())
  rels <- rvest::html_attr(syn_nodes, "data-category") %>% 
    stringr::str_extract_all("relevant-\\d") %>% 
    purrr::map_chr(1) %>% 
    stringr::str_extract_all("\\d+$") %>% 
    purrr::map_chr(1) %>% 
    as.numeric()
  dplyr::tibble(synonym=syns, relevance=rels)
}

get_syns_df("http://www.thesaurus.com/browse/frisk")