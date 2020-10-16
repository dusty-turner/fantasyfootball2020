library(tidyverse)


# leagueID <- list(89417258)
# names <- list("OA")
leagueID <- list(847888,35354777,89417258,206814)
names <- list("jim","headshed","OA","Twitter_Guy")
per_id <- 5

run_reports <- function(leagueID, per_id = per_id, names) {
  # leagueID=89417258
  # per_id=1
  # unlink("ff2020_reports",recursive = T,force = T)
  
  if(!dir.exists("03_ff2020_reports")){
    dir.create("03_ff2020_reports")
  } 
  
  rmarkdown::render("ff2020.Rmd",params=list(
    per_id=per_id, 
    leagueID=leagueID))

  file.rename(from="ff2020.html", to = paste0("ffdashboard_",names,"_",per_id,".html"))

  file.copy(from=str_c("ffdashboard_",names,"_",per_id,".html"),
            to=str_c("03_ff2020_reports/ffdashboard_",names,"_",per_id,".html" ),overwrite = T)

  file.remove(paste0(getwd(),"/","ffdashboard_",names,"_",per_id,".html"))

  unlink(x = "ff2020_cache*",recursive = T, force = T)
  
  if(Sys.info()[[6]]=="turne" & names %in% c("headshed","OA","jim")){
    file.copy(from=str_c("03_ff2020_reports/ffdashboard_",names,"_",per_id,".html"),
              to=str_c("../blog/static/ff2020/ffdashboard_",names,"_",per_id,".html"),
              overwrite = TRUE
    )
    setwd("../blog")
    blogdown::serve_site()
    blogdown::stop_server()
    setwd("../fantasyfootball2020/")
    shell("C:/Users/turne/Desktop/Dustys_Files/R_Work/fantasyfootball2020/09_personal/shell1.sh")
    # shell(str_c(getwd(),"/09_personal/shell1.sh"))
    

  }

}


# map2(leagueIDs,rep(per_id_number,length(leagueIDs)),run_reports)
leagueID %>% 
purrr::walk2(names,.f = ~run_reports(leagueID = .x,names = .y,per_id = per_id))

