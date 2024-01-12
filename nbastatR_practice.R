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

# players_tables(
#   players = hornets_players,
#   seasons = 2024, 
#   tables = c("game logs"),
#   measures = c("Base"), 
#   mode = c("PerGame"))

#make function to 

player_stats <- function(x){
  bref_players_stats(seasons = x) %>%
    unnest(cols = c(dataTable))
  }
this_season <- player_stats(c(2024))



avg_3pa <- function(x) {
  bref_teams_stats(seasons = x) %>%
    unnest(cols = c(dataTable)) %>%
    # group_by(yearSeason) %>%
    mutate(mean_3pa = mean(fg3aPerGameTeam) )
}

three_attempts <- avg_3pa(c(1980, 1985, 1990, 1995, 2000, 2005,
                            2010, 2015))
data.frame(three_attempts)

tbles <- c("per_game", "advanced")

player_stats <- function(x) {
  bref_players_stats(seasons = x, tables = tbles) %>%
    unnest(cols = c(dataTable)) %>%
    group_by(yearSeason)
}

test <- player_stats(c(2024))

bref_players_stats(seasons = 2024, tables = tbles)
