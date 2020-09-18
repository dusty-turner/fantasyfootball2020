library(tidyverse)

league_ids <- c(847888,35354777,89417258,206814)
week_number <- 1

run_reports <- function(league_id, week) {
  # league_id=89417258
  # week=1
  rmarkdown::render("ff2020.Rmd",params=list(
    per_id=week, 
    leagueID=league_id))
  file.rename(from="ff2020.html", to = paste0("ffdashboard",league_id,"_",week,".html"))
  file.copy(from=paste0(getwd(),"/","ffdashboard",league_id,"_",week,".html"),
            to=paste0(getwd(),"/ff2020_reports/","ffdashboard",league_id,"_",week,".html" ))
  file.remove(paste0(getwd(),"/","ffdashboard",league_id,"_",week,".html"))
}

map2(league_ids,rep(week_number,length(league_ids)),run_reports)
