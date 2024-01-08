library(nbastatR)
library(tidyverse)
library(here)

#Try and work with NBAstatR

#Make table of stats (basic + advanced) for 2024 hornets
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2) # fixes issue calling

# standings_2024 <- standings(seasons = 2024, season_types = c("Regular Season"))
hornets_players <- c("Lamelo Ball", "Gordon Hayward", "Terry Rozier", "Mark Williams", "Miles Bridges", "Ish Smith", "Nick Smith Jr.", "Nick Richards", "P.J. Washington", "J.T. Thor", "Frank Ntilikina", "Cody Martin", "Bryce McGowens", "Brandon Miller")

hornets_id <- nba_teams_ids(teams = "Hornets")
df <- nba_teams()

hornets <- nba_players()

hornets<- hornets %>% 
  filter(isActive == "TRUE") %>%
  select(namePlayer,idPlayer)
hornets <- hornets %>%
  filter(namePlayer %in% hornets_players)


ids <- hornets$idPlayer

players_tables(
  player_ids = ids,
  seasons = 2024)
