#CBBPLOTR INITIAL PRACTICE

# install.packages("cbbplotR")
# devtools::install_github("andreweatherman/cbbplotR") 
# library(devtools)
# if (!require("pak")) install.packages("pak")
# pak::pak("andreweatherman/cbbplotR")
library(cbbdata)
library(tidyverse)
library(gt)
library(cbbplotR)
library(here)
#######################################################################################
#load in data
all_conf <- cbd_torvik_conf_factors(year = 2024) %>%
  slice(1:10)

facet_data <- cbbdata::cbd_torvik_ratings_archive(year = 2024) %>% 
  summarize(avg_rating = mean(barthag), .by = c(conf, date)) %>%
  filter(conf %in% c('ACC', 'B10', 'B12'))


#########################################################################
#Create big 12 adjusted efficiency plot
b12_team_data <- cbd_torvik_ratings(year = 2024, conf = "B12")

b12_team_data %>%
  ggplot(aes(adj_o, adj_d, team = team)) + #must pass team = team in aes for cbbplotR to match team logos
  geom_cbb_teams(width = 0.1) + #geom with logos
  geom_mean_lines(aes(x0 = adj_o, y0 = adj_d), color = "black") +
  # geom_label(label = b12_team_data$team) +
  # geom_text(label = b12_team_data$team, check_overlap = TRUE, hjust = "bottom") + #add team labels to each point
  theme_minimal() +
  annotate("text",x = 117, y = 98.5,label = "Good Off. Bad Def.", alpha = 0.5) + #annotate adds custom text to graph
  annotate("text",x = 117, y = 89,label = "Good Off. Good Def.", alpha = 0.5) +
  annotate("text",x = 107, y = 89,label = "Bad Off. Bad Def.", alpha = 0.5) +
  annotate("text",x = 107.5, y = 98.4 ,label = "Bad Off. Good Def.", alpha = 0.5) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold")) +
  labs(title = "Adjusted Offensive + Defensive Efficiency in the Big12",
       x = "Adjusted Offense",
       y = "Adjusted Defense")


ggsave(filename = here("GitHub/practicing_cbb/images","b12_adjusted_efficiency.png" ))

#####################################################################################
#Create adjusted efficiency plot comparing conferences

all_conf %>%
  ggplot(aes(adj_o, adj_d, conference = conf)) +
  geom_cbb_conferences(width = 0.12) +
  geom_mean_lines(aes(x0 = adj_o, y0 = adj_d), color = "black") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold")) +
  labs(title = "Adjusted Offensive + Defensive Efficiency across Conf",
       x = "Adjusted Offense",
       y = "Adjusted Defense")

ggsave(filename = here("GitHub/practicing_cbb/images","conf_adjusted_efficiency.png" ))

#####################################################################################
#Use player headshots in plot

set.seed(25)
player_ids <- get_espn_players("Texas")
random_players <- tibble(
  val1 = rnorm(nrow(player_ids)),
  val2 = rnorm(nrow(player_ids)),
  id = player_ids$id
)


random_players <- random_players %>%
  left_join(player_ids$displayName)


random_players %>%
  ggplot(aes(val1,val2)) +
  geom_cbb_headshots(aes(player_id = id, width = 0.1)) +
  geom_label(label = player_ids$displayName, vjust = 2, alpha = 0.1)+
  theme_minimal()


######################################################################
#look at home court advantage in b12 in 2023

b12_2023 <- 
  cbd_torvik_game_factors(year = "2023", conf = "B12") #%>%

home_away <- b12_2023 %>%
  select(team, result, opp_conf,loc, type, adj_o, adj_d, game_score,opp_conf)

wanted_teams <- c("Kansas", "Texas", "Okalhoma", "Baylor")

home_away %>%
  filter(team == "Kansas" | team == "Texas" | team == "Baylor" & type == "conf") %>%
  ggplot(aes(adj_o, adj_d, team = team, color = loc)) +
  geom_point() +
  geom_mean_lines(aes(x0 = adj_o, y0 = adj_d))+
  geom_smooth() +
  facet_wrap(~team) +
  theme_minimal() +
  theme(strip.text.x = element_cbb_teams(size = 1))
  # geom_cbb_teams(width = 0.1)

help(match)
