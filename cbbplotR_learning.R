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

b12_team_data <- cbd_torvik_ratings(year = 2024, conf = "B12")

all_conf <- cbd_torvik_conf_factors(year = 2024) %>%
  slice(1:10)

facet_data <- cbbdata::cbd_torvik_ratings_archive(year = 2024) %>% 
  summarize(avg_rating = mean(barthag), .by = c(conf, date)) %>%
  filter(conf %in% c('ACC', 'B10', 'B12'))


#########################################################################
#Create big 12 adjusted efficiency plot
b12_team_data %>%
  ggplot(aes(adj_o, adj_d, team = team)) + #must pass team = team in aes for cbbplotR to match team logos
  geom_cbb_teams(width = 0.1) + #geom with logos
  geom_mean_lines(aes(x0 = adj_o, y0 = adj_d), color = "black") +
  # geom_label(label = b12_team_data$team) +
  # geom_text(label = b12_team_data$team, check_overlap = TRUE, hjust = "bottom") + #add team labels to each point
  theme_minimal() +
  annotate("text",x = 117, y = 98.5,label = "Good Offense, Bad Defense", alpha = 0.5) + #annotate adds custom text to graph
  annotate("text",x = 117, y = 86,label = "Good Offense, Good Defense", alpha = 0.5) +
  annotate("text",x = 107, y = 86,label = "Bad Offense, Bad Defense", alpha = 0.5) +
  annotate("text",x = 107.5, y = 98.2 ,label = "Bad Offense, Good Defense", alpha = 0.5) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold")) +
  labs(title = "Adjusted Offensive + Defensive Efficiency in the Big12",
       x = "Adjusted Offense",
       y = "Adjusted Defense")


ggsave(filename = here("/images","b12_adjusted_efficiency.png" ))
library(here)
