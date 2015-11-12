library(rvest)
library(dplyr)
library(stringr)

url <- 'http://matchcenter.mlssoccer.com/matchcenter/2015-11-08-new-york-red-bulls-vs-dc-united/boxscore'
url2 <- 'http://matchcenter.mlssoccer.com/matchcenter/2015-11-08-vancouver-whitecaps-fc-vs-portland-timbers/boxscore'
url3 <- 'http://matchcenter.mlssoccer.com/matchcenter/2015-11-08-columbus-crew-sc-vs-montreal-impact/boxscore'

GetAttendance <- function(url) {
  # Scrapes attendance from URL and converts to a number
  #
  # Args:
  #   url: string.
  #
  # Returns:
  #   Number of attendance from url.
  #
  game_data <- html(url)
  attendance <- game_data %>%
    html_node(".match-info div:nth-child(3)") %>%
    html_text() %>%
    as.character()
  num <- as.numeric(str_sub(attendance, start=12))
  return(num)
}

GetAttendance(url)
GetAttendance(url2)
GetAttendance(url3)

# Sometimes there's an error if child(2) is the attendance
.match-info div:nth-child(2) span