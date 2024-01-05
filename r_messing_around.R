#SETTING UP R AND CBBDATA#

library(tidyverse)
library(devtools)
library(cbbdata)
library(plotly)
library(patchwork)

#devtools::install_github("andreweatherman/cbbdata")
# to register
#cbbdata::cbd_create_account(username = 'chaseweaver', email = 'dcweaver@utexas.edu', password = '', confirm_password = '')
# persistent log-in
cbbdata::cbd_login()
###############################################################################

#messing around in cbbdata

#kenpom authorization
cbbdata::cbd_kenpom_authorization(password = "insert pass")

colnames(cbd_torvik_team_factors(year = 2024))

#EXAMPLES FROM GITHUB TO HELP

cbbdata::cbd_torvik_team_factors(year = 2024) %>%
  dplyr::slice(1:20) %>%
  dplyr::select(team, barthag, adj_o, adj_d) %>%
  #dplyr::mutate()
  cbbdata::cbd_gt_logos(team, team) %>% #Load in team logos
  gt::gt() %>%
  cbbdata::gt_theme_athletic() %>% #format in table style of the athletic
  gt::fmt_markdown(team) %>% # MUST USE THIS FUNCTION TO DISPLAY LOGOS NEXT TO TEAM
  gt::cols_align(columns = team, 'left') #Lines up the logos so they look pretty


#TRACK TEAMS PERFORMANCE OVER TIME (FOUR FACTORS)
  
#df of Texas' 2024 season
texas_2024 <- data.frame(cbbdata::cbd_torvik_game_factors(year = 2024, team = "Texas")) 
#Add new column for margin of victory
texas_2024 <- texas_2024 %>%
  dplyr::mutate(
    margin_of_victory = pts_scored - pts_allowed 
  )
#create plotly variable
p <- 
  #scale_color_gradient(low location#scale_color_gradient(low = "blue", high = "red") #color gradient for margin of victory
  #geom_point(data = df, aes(text = team, opp, size = result), color = "blue", alpha = 0.5) +
  #scale_color_gl(values = c("darkred", "darkgreen"))
  
ggplotly(p)

####################################################
#cbd_kenpom_ratings(year = 2024, team = "Texas")

#make box plot of games for each month so far 
texas_2024 <- texas_2024 %>%
  mutate(month = case_when((team == "Texas" & date == "2023-12") ~ "December",
                          (team == "Texas" & date == "2023-11") ~ "November"))

q <- texas_2024 %>% 
  ggplot(aes())
  
  
#NCAA Tourney history for Texas 
#cbbdata::cbd_torvik_ncaa_results(min_year = 2015, max_year = 2023, type = "team") %>%
  #dplyr::filter(team == "Texas")

df <- cbbdata::cbd_torvik_game_box() %>%
  dplyr::select(date, year, result, team, opp) %>%
  dplyr::filter(year == 2024, team == "Texas")

#Create side by side plots for Texas home vs. away (2023 data since 2024 is incomplete)
texas_2023 <- data.frame(cbbdata::cbd_torvik_game_factors(year = 2023, team = "Texas"))
texas_2023 <- texas_2023 %>% 
  dplyr::mutate(margin_of_victory = pts_scored - pts_allowed)

#boxplots for home/away/neutral site vs. margin of victory
texas_2023 %>%
  ggplot(aes(x = loc, y = margin_of_victory, fill = result)) +
  geom_boxplot() +
  #coord_flip() + #turn box plots horizontal 
  stat_summary(fun.y = mean, geom = "point", shape = 5 , size = 4) + #add in diamond to represent mean margin of v for each location type
  ggtitle("Margin of Victory vs. Location")
  
