library(tidyverse)

base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = "2020"
mid = "/segments/0/leagues/"
leagueID = "89417258"
tail = str_c("?view=mDraftDetail",
             "&view=mLiveScoring",
             "&view=mMatchupScore,",
             "&view=mPendingTransactions",
             "&view=mPositionalRatings",
             "&view=mRoster",
             "&view=mSettings",
             "&view=mTeam",
             "&view=modular",
             "&view=mNav",
             "&view=mMatchupScore",
             "&scoringPeriodId="
             )



per_id <- 1
url = paste0(base,year,mid,leagueID,tail,per_id)

ESPNGet <- httr::GET(url = url)
ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)

ESPNFromJSON %>% listviewer::jsonedit()


## one players stats
player_extract <- function(team_number = 1, player_number = 1){
  player_week <-
    tibble(
      team = str_c(ESPNFromJSON$teams$location[team_number]," ",ESPNFromJSON$teams$nickname[team_number]),
      teamId = ESPNFromJSON$teams$id[team_number],
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

schedule <-
  tibble(
    home = ESPNFromJSON$schedule$away$teamId,
    away = ESPNFromJSON$schedule$home$teamId,
    scoringPeriodId = ESPNFromJSON$schedule$matchupPeriodId,
    gameId = ESPNFromJSON$schedule$id
  ) %>% 
  pivot_longer(cols = c(home,away), values_to = "teamId")
# ESPNFromJSON$schedule$away$totalPoints


roster_size <- 15
number_of_teams <- 12

player_slot <- rep(1:roster_size,number_of_teams)
team_number <- rep(1:number_of_teams,roster_size) %>% sort()

team_list <- purrr::map2_dfr(player_slot,team_number, .f = ~player_extract(team_number = .y,player_number = .x)) %>% 
  left_join(schedule) %>% 
  mutate(points_type = if_else(str_length(externalId) > 6, "actual", "projected")) %>% 
  relocate(team:appliedTotal, points_type)

schedule_prep <-
team_list %>% 
  filter(lineupSlot_id != 20) %>%  # remove bench players
  filter(scoringPeriodId <= per_id) %>% 
  group_by(team, scoringPeriodId, points_type) %>% 
  summarise(points = sum(appliedTotal), gameId = gameId[1]) %>% 
  arrange(scoringPeriodId,gameId) %>%
  filter(points_type == "actual") %>% 
  group_by(scoringPeriodId,gameId) 

standings <-
schedule_prep %>% 
  mutate(Win_week = points==max(points)) %>% 
  ungroup() %>% 
  group_by(team) %>% summarise(Wins = sum(Win_week)) %>%
  mutate(Losses = per_id - Wins) %>% 
  group_by(team) %>% 
  mutate(win_perc = Wins / (Wins + Losses))

week_win_standings <-
schedule_prep %>% 
  group_by(scoringPeriodId) %>% 
  mutate(week_wins = rank(points)) %>% 
  mutate(week_losses = max(week_wins)-week_wins) %>% 
  group_by(team) %>% 
  summarise(week_wins = sum(week_wins), week_losses = sum(week_losses)) %>% 
  mutate(week_win_perc = week_wins / (week_wins + week_losses))

total_standings <-
  standings %>% 
  left_join(week_win_standings) %>% 
  relocate(contains("perc"), .after = last_col()) %>% 
  ungroup() %>% 
  mutate(luck = (-win_perc + week_win_perc)/sqrt(2)) 
  mutate(luck = abs(luck)) %>% 
  mutate(luck = max(luck) - luck)

luck_help_df = tibble(win_perc = c(0.3,0.3,.7,.7), week_win_perc = c(0,1,1,0), labs = c("Bad","Unlucky","Good","Lucky"))

plot_luck_chart <- function(total_standings = total_standings){
  total_standings %>% 
    arrange(-luck) %>% 
    ggplot(aes(x=win_perc,y=week_win_perc, color = luck)) +
    geom_point(size = 3) +
    geom_abline(aes(intercept = 0,slope = 1)) +
    xlim(0,1) + ylim(0,1) +
    scale_color_gradient2(low = "green",mid = "grey" ,high = "red",midpoint = 0,limits=c(range(total_standings$luck))) +
    # scale_color_continuous(low = "green", high = "red",limits=c(range(total_standings$luck))) +
    geom_label_repel(aes(label = str_c(team)), color = "Black",max.iter = 10000) +
    geom_label(data = luck_help_df,mapping = aes(x=win_perc,y=week_win_perc,label = labs), color = "Black", label.size = 1) +
    labs(title = "How Lucky Is Your Team?",subtitle = "Are You Winning As Much As You Should?",
         x = "Traditional Win Percentage", y = "Every Game Every Week Win Percentage", color = "Luck") +
    theme(legend.position = "none")
  
}

plot_luck_chart(total_standings = total_standings)

