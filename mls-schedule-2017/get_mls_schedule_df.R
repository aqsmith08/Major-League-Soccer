library(rvest)
library(stringr)

get_mls_schedule_df <- function(mls.url = NULL) {
  html <- read_html(mls.url)
  html.list <- html_nodes(html, ".odd .button")
  
  game.url <- NULL
  for (i in 1:length(html.list)) {
    url.html <- html.list[i][[1]]
    game.url.str <- strsplit(as.character(url.html), '"')[[1]][2]
    game.url <- append(game.url, game.url.str)
  }
  
  df <- data.frame(game.url, stringsAsFactors = FALSE)
  return(df)
}

# Example
# mls.2017.schedule <- get_mls_schedule_df("http://www.mlssoccer.com/schedule?month=all&year=2017&club=select&club_options=9&op=Update&form_build_id=form-KKIKB_T_PkjrBZU_p68sNOdmhTnCFLJzQY1XkhVaj1M&form_id=mp7_schedule_hub_search_filters_form")
