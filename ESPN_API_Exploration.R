library(tidyverse)

base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = "2020"
mid = "/segments/0/leagues/"
leagueID = "89417258"
tail = "?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mSettings&view=mTeam&view=modular&view=mNav&view=mMatchupScore&view=players_wl&view=mTransactions2&view=mStatus"
url = paste0(base,year,mid,leagueID,tail)


ESPNGet <- httr::GET(url = url)
ESPNGet$status_code

ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)

listviewer::jsonedit(ESPNFromJSON)


ESPNFromJSON %>% listviewer::jsonedit()
ESPNFromJSON$players %>% as_tibble() %>% names

## this is a keeper of all players
ESPNFromJSON$players$player %>% as_tibble() 
ESPNFromJSON$players$player %>% as_tibble() %>% select(stats) 


ESPNFromJSON



