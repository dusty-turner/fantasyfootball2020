
ESPNFromJSON <- function(leagueID, per_id){

base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = "2020"
mid = "/segments/0/leagues/"
# leagueID = "847888" # jim
# leagueID = "35354777" # caa
# leagueID = "89417258" # dusty 
# leagueID = "206814" # twitter guy
# per_id <- 1
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

# set_league <- function(league){leagueID <- league}
# set_per_id <- function(period){per_id <- period}
# per_id=1

url = paste0(base,year,mid,leagueID,tail,per_id)

ESPNGet <- httr::GET(url = url)
ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)
return(ESPNFromJSON)
}
