library(tidyverse)

leagueIDs <- c(847888,35354777,89417258,206814)
week_number <- 1

run_reports <- function(leagueID = 847888, week = 1) {
  # leagueID=89417258
  # week=1
  # unlink("ff2020_reports",recursive = T,force = T)
  
  if(!dir.exists("ff2020_reports")){
    dir.create("ff2020_reports")
  } 
  
  rmarkdown::render("ff2020.Rmd",params=list(
    per_id=week, 
    leagueID=leagueID))
  file.rename(from="ff2020.html", to = paste0("ffdashboard",leagueID,"_",week,".html"))
  file.copy(from=str_c("ffdashboard",leagueID,"_",week,".html"),
            to=str_c("ff2020_reports/ffdashboard",leagueID,"_",week,".html" ))
  # file.copy(from=paste0(getwd(),"/","ffdashboard",leagueID,"_",week,".html"),
  #           to=paste0(getwd(),"/ff2020_reports/","ffdashboard",leagueID,"_",week,".html" ))
  file.remove(paste0(getwd(),"/","ffdashboard",leagueID,"_",week,".html"))
  unlink(x = "ff2020_cache*",recursive = T, force = T)
  # unlink(x = "ff2020_files*",recursive = T, force = T)
}

# map2(leagueIDs,rep(week_number,length(leagueIDs)),run_reports)

map2(leagueIDs,rep(week_number,length(leagueIDs)),~run_reports(leagueID = .x,week = .y))
