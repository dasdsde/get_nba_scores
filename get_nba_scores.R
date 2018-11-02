############################################################################
# This Script simply gets the NBA Scores from the last day,
# including the western and eastern conference standings,
# and sends them via email to different email addresses (or via Cisco Spark)

# Unfortunately, the API endpoints are not so well documented, but there 
# is a good overview here:
# https://github.com/seemethere/nba_py/wiki/stats.nba.com-Endpoint-Documentation
###########################################################################

# packages ----------------------------------------------------------------

library(tidyverse)
library(httr)
library(lubridate)
library(glue)
library(jsonlite)
library(mailR)
library(htmlTable)

# get data ----------------------------------------------------------------

today <- Sys.Date() - 1 # I run this script from Germany in the morning, therefore set the date to yesterday
day <- day(today) # extract day
if (nchar(day) == 1) { # for days 1 to 9 add a leading 0
  day <- paste0(c("0", day), collapse = "")
}

month <- month(today) # extract month
if (nchar(month) == 1) { # for month 1 to 9 add a leading 0
  month <- paste0(c("0", month), collapse = "")
}
year <- year(today) # extract year

# get GameIDs -------------------------------------------------------------

url1 <- glue("http://stats.nba.com/stats/scoreboard/?GameDate={month}/{day}/{year}&LeagueID=00&DayOffset=0") # create URL
nba <- readLines(url1) # get data
json <- fromJSON(txt = nba) # parse to list
gameid <- json$resultSets$rowSet[[4]][,1] #extract GameIDs

# create confernce standings and parse to html ----------------------------

west <- json$resultSets$rowSet[[6]][,c(6,8,9,10)] %>% 
  as.data.frame()
east <- json$resultSets$rowSet[[5]][,c(6,8,9,10)] %>% 
  as.data.frame()

names(west) <- c("Team", "Wins", "Losses", "%")
names(east) <- c("Team", "Wins", "Losses", "%")

west_mail <- htmlTable(west)
east_mail <- htmlTable(east)

# get final results from GameIDs ------------------------------------------

results <- list()
for (i in 1:length(gameid)) {
  url1 <- glue("http://stats.nba.com/stats/boxscoresummaryv2/?GameId={gameid[i]}&StartPeriod=0&EndPeriod=0&RangeType=0&StartRange=0&EndRange=0")
  nba <- readLines(url1)
  json <- fromJSON(txt = nba)
  results[[i]] <- json$resultSets$rowSet[[6]]
}

# paste results togehter --------------------------------------------------

for_print <- matrix(NA, nrow = length(gameid), ncol = 6)
for (i in 1:length(gameid)) {
  for_print[i,1] <- results[[i]][2,6] # away team city
  for_print[i,2] <- results[[i]][2,7] # away team name
  for_print[i,3] <- results[[i]][2,23] # away team final score
  for_print[i,4] <- results[[i]][1,23] # home team final score
  for_print[i,5] <- results[[i]][1,6] # home team city
  for_print[i,6] <- results[[i]][1,7] # home team name
}

# prepare for printing ----------------------------------------------------

for_print <- for_print %>% 
  as.data.frame() %>% 
  mutate(final = paste(V1, V2, V3, "-", V4, V5, V6))

for_print <- paste0(for_print$final, collapse = "<br><br>")
for_print <- paste0("<p><b>Results: </b></p>", for_print, "<p><b>Eastern Conference: </b></p>", east_mail, 
                    "<p><b>Western Conference: </b></p>", west_mail, collapse = "<br>")

# send via spark ----------------------------------------------------------

# request <- list(
#   roomId = "xxx",
#   text = paste0("NBA Scores for ", today, ":<br><br>", for_print)
# )
# 
# POST("https://api.ciscospark.com/v1/messages",
#        content_type('application/json'), 
#        add_headers(Authorization = "Bearer XXX"),
#        body = request,  
#        encode = "json"
#   )

# send via email ----------------------------------------------------------

sender <- ""  # Replace with a valid address
recipients <- c()  # Replace with one or more valid addresses
email <- mailR::send.mail(from = sender,
                   to = recipients,
                   subject = paste0("NBA Scores for ", today),
                   body = for_print,
                   smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "xxx", passwd = "xxx", ssl = TRUE),
                   authenticate = TRUE,
                   send = FALSE,
                   html = TRUE)

email$send()