library(lubridate)
library(tidyverse)
library(ggrepel)

get_api_data <- function(leagueID = "89417258"){

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
"&view=mPositionalRankingsStats"
)
url = paste0(base,year,mid,leagueID,tail)


ESPNGet <- httr::GET(url = url)
ESPNGet$status_code

ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)
return(ESPNFromJSON)
}

data_for_processing <- get_api_data(leagueID = "89417258")


listviewer::jsonedit(data_for_processing)


ESPNFromJSON %>% listviewer::jsonedit()
ESPNFromJSON$players %>% as_tibble() %>% names

## team info
team_info <-
ESPNFromJSON$teams %>% 
  select(abbrev,currentProjectedRank,divisionId,draftDayProjectedRank,id,location,nickname,playoffSeed,points,pointsAdjusted,pointsDelta,rankCalculatedFinal,waiverRank) %>% 
  bind_cols(ESPNFromJSON$teams$transactionCounter %>% select(acquisitions))

## game info
game_df <-
ESPNFromJSON$schedule$away %>% mutate(location = "Away") %>% bind_cols(ESPNFromJSON$schedule %>% select(id,matchupPeriodId,winner)) %>% 
  bind_rows(ESPNFromJSON$schedule$home %>% mutate(location = "Away") %>% bind_cols(ESPNFromJSON$schedule %>% select(id,matchupPeriodId,winner)))  %>% 
  as_tibble() %>% 
  rename(game_id = id)

game_and_team_info <-
game_df %>% 
  left_join(team_info, by = c("teamId"="id")) 


## this is a keeper of all players
ESPNFromJSON$players$player %>% as_tibble() 
ESPNFromJSON$players$player %>% as_tibble() %>% select(stats) %>% unnest(cols = stats)


week_of <- 7

standings <-
game_and_team_info %>% 
  filter(matchupPeriodId <= 7) %>% 
  arrange(game_id) %>% 
  mutate(points = runif(n = nrow(.),min = 60,max = 100)) %>% 
  select(points, game_id, teamId, matchupPeriodId,location.y, nickname) %>% 
  group_by(game_id) %>% 
  mutate(winner = points==max(points)) %>% 
  group_by(matchupPeriodId) %>% 
  mutate(week_wins = rank(points)-1, week_losses = rank(-points)-1) %>% 
  group_by(teamId, location.y ,nickname) %>% 
  summarise(wins = sum(winner), losses = week_of - wins, week_wins = sum(week_wins), week_losses = sum(week_losses)) %>% 
  mutate(win_perc = wins/(wins+losses), week_win_perc = week_wins/(week_wins+week_losses))

luck_help_df = tibble(win_perc = c(0.1,0.1,.9,.9), week_win_perc = c(0,1,1,0), labs = c("Bad","Lucky","Good","Unlucky"))

standings %>% 
  mutate(luck = (-win_perc + week_win_perc)/sqrt(2)) %>% 
  ggplot(aes(x=win_perc,y=week_win_perc, color = luck)) +
  geom_point(size = 3) +
  geom_abline(aes(intercept = 0,slope = 1)) +
  xlim(0,1) + ylim(0,1) +
  scale_color_continuous(low = "red", high = "green",limits=c(-.2, .2)) +
  geom_label_repel(aes(label = str_c(location.y, " ", nickname)), color = "Black",max.iter = 10000) +
  geom_label(data = luck_help_df,mapping = aes(x=win_perc,y=week_win_perc,label = labs), color = "Black") +
  labs(title = "How Lucky Is Your Team",subtitle = "Are You Winning As Much As You Should?",
       x = "Traditional Win Percentage", y = "Every Game Every Week Win Percentage", color = "Luck") +
  theme(legend.position = "none")
  

