#Conference Simulations
needed_packages <- c('tidyverse', 'rvest', 'gt', 'gtExtras', 'withr')
installed_packages <- needed_packages %in% rownames(installed.packages())
invisible(lapply(needed_packages, library, character.only = TRUE))

#Login to CBBDATA Api
# cbbdata::cbd_login()
# cbd_kenpom_authorization("password")

#Function to get teams and conferences
get_teams <- function(x) {
  cbd_kenpom_ratings(year = 2024) %>%
    filter(conf == x) %>% 
    select(conf, team) #%>% 
    #simplify()
  
}

#vector of p5 conferences
power_five <- 
  conferences %>%
  slice(1:5) %>%
  select(conf) %>%
  simplify()
  
confs <- lapply(power_five, get_teams)%>% #lapply to loop over vector of conferences
  bind_rows(,.id = "column_label") %>% #turn into df
  select(conf, team)

#test simulation function
kansas_sim <- try(cbd_torvik_season_simulation("Duke", 2023))

texas_prediction <- cbd_torvik_season_prediction("Texas", year = 2024)

texas_prediction <- texas_prediction %>%
  mutate(win_loss = as.numeric(did_win))
  
predict_wl <- function(team){
  i <- cbd_torvik_season_prediction(team, year = 2024)%>%
    mutate(win_loss = as.numeric(did_win)) %>%
    wins <- sum(win_loss)
  
  sprintf("The projected win total for %s is %.f", team,wins)  
}


blerg <- "Texas"
arg <- 24
sprintf("This team is %s and their number is %.f", blerg, arg)


practice <- predict_wl("Texas")
