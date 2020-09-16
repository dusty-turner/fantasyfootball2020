library(tidyverse)

base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = "2020"
mid = "/segments/0/leagues/"
leagueID = "89417258"
tail = str_c("?view=mDraftDetail",
             "&view=mLiveScoring&view=mMatchupScore",
             "&view=mPendingTransactions",
             "&view=mPositionalRatings",
             "&view=mSettings",
             "&view=mTeam",
             "&view=modular",
             "&view=mNav",
             "&view=mMatchupScore",
             "&view=players_wl",
             "&view=mTransactions2",
             "&view=mStatus",
             "&view=kona_player_info",
             "&view=kona_league_communication",
             "&view=mPositionalRankingsStats"
)
tail = str_c("?view=kona_player_info"
)
url = paste0(base,year,mid,leagueID,tail,"&limit=1000")


ESPNGet <- httr::GET(url = url)
# ESPNGet <- httr::GET(url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/89417258?view=mPositionalRatings&view=mRoster&view=mSettings&view=mTeam&view=modular&view=mNav")
# ESPNGet <- httr::GET(url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/89417258?view=modular&view=mNav&view=mMatchupScore&view=mScoreboard&view=mStatus&view=mSettings&view=mTeam&view=mPendingTransactions")
# ESPNGet <- httr::GET(url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/89417258?view=modular&view=mNav&view=mMatchupScore&view=mScoreboard&view=mStatus&view=mSettings&view=mTeam&view=mPendingTransactions?scoringPeriodId=1")
# ESPNGet <- httr::GET(url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/89417258?view=mPositionalRatings&view=mRoster&view=mSettings&view=mTeam&view=modular&view=mNav")
# ESPNGet <- httr::GET(url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/players?scoringPeriodId=1&view=players_wl")
# ESPNGet <- httr::GET(url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/89417258?view=modular&view=mNav&view=mMatchupScore&view=mScoreboard&view=mStatus&view=mTeam&view=mPendingTransactions&rosterForTeamId=1")
# ESPNGet <- httr::GET(url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/89417258?rosterForTeamId=1&view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mRoster&view=mSettings&view=mTeam&view=modular&view=mNav&scoringPeriodId=2")
ESPNGet <- httr::GET(url = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/89417258?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mRoster&view=mSettings&view=mTeam&view=modular&view=mNav&scoringPeriodId=2")
ESPNGet$status_code

ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)


ESPNFromJSON %>% listviewer::jsonedit()


ESPNFromJSON$teams$roster$entries[[1]]$playerPoolEntry$player$fullName
ESPNFromJSON$teams$roster$entries[[1]]$playerPoolEntry$player$id

ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$fullName[player_number]

## one players stats
player_extract <- function(team_number = 1, player_number = 1){
  player_week <-
tibble(
team = str_c(ESPNFromJSON$teams$location[team_number]," ",ESPNFromJSON$teams$nickname[team_number]),
fullName = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$fullName[player_number],
appliedTotal = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$appliedTotal,
seasonId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$seasonId,
scoringPeriodId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$scoringPeriodId,
statsplitTypeId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$statSplitTypeId,
externalId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$externalId,
lineupSlot_id = ESPNFromJSON$teams$roster$entries[[team_number]]$lineupSlotId[player_number]
) %>% 
  filter(seasonId==2020) %>% 
  filter(scoringPeriodId != 0)
  return(player_week)
}


player_extract(team_number = 1,player_number = 1)

player_slot <- rep(1:15,12)
team_number <- rep(1:12,15) %>% sort()
purrr::map2_dfr(player_slot,team_number, .f = ~player_extract(team_number = .y,player_number = .x)) %>% 
  filter(scoringPeriodId == 1) %>% 
  filter(lineupSlot_id !=20)



tibble(
  fullname = ESPNFromJSON$players$player$fullName,
  id = ESPNFromJSON$players$player$id,
  onTeamId = ESPNFromJSON$players$onTeamId
  ) %>% 
  mutate(slot_id = as.double(seq(1,nrow(.)))) %>% 
  left_join(
    ESPNFromJSON$players$player$stats %>%  
    bind_rows(.id = "slot_id") %>% 
    as_tibble() %>% 
    select(appliedTotal, externalId, scoringPeriodId, slot_id) %>%
    mutate(slot_id = as.numeric(slot_id)) %>% 
    filter(scoringPeriodId != 0) %>% 
    filter(str_length(string = externalId)>4) %>% 
    mutate(Type = if_else(str_length(externalId)>6,"Actual","Projected")), 
    by = "slot_id"
)  
