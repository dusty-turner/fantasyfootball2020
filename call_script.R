library(tidyverse)

leagueID <- list(847888,35354777)
# leagueID <- list(847888,35354777,89417258,206814)
per_id <- 1

run_reports <- function(leagueID, per_id = 1) {
  # leagueID=89417258
  # per_id=1
  # unlink("ff2020_reports",recursive = T,force = T)
  
  if(!dir.exists("ff2020_reports")){
    dir.create("ff2020_reports")
  } 
  
  rmarkdown::render("ff2020.Rmd",params=list(
    per_id=per_id, 
    leagueID=leagueID))

  file.rename(from="ff2020.html", to = paste0("ffdashboard",leagueID,"_",per_id,".html"))

  file.copy(from=str_c("ffdashboard",leagueID,"_",per_id,".html"),
            to=str_c("ff2020_reports/ffdashboard",leagueID,"_",per_id,".html" ))

  file.remove(paste0(getwd(),"/","ffdashboard",leagueID,"_",per_id,".html"))

  unlink(x = "ff2020_cache*",recursive = T, force = T)

}

# map2(leagueIDs,rep(per_id_number,length(leagueIDs)),run_reports)
leagueID %>% 
purrr::walk(.f = ~run_reports(leagueID = .x,per_id = 1))

# purrr::map(leagueID, ~run_reports(leagueID = .x,per_id = 1))
# 
# run_reports(leagueID = leagueID[1],per_id = 1)

