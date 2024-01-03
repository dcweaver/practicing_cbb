needed_packages <- c('tidyverse', 'rvest', 'gt', 'gtExtras', 'withr')
installed_packages <- needed_packages %in% rownames(installed.packages())
invisible(lapply(needed_packages, library, character.only = TRUE))


#USE THIS ON WINDOWS
withr::local_options(HTTPUserAgent='Buckets & Bytes')

schedule <- read_csv("https://barttorvik.com/2024_master_sked.csv", col_names = FALSE) %>%
  setNames(c('game_id', 'game_date', 'game_type', 'neutral', 'away', 'home')) %>%
  select(neutral, home, away)



rankings <- read_html('https://barttorvik.com/trankpre.php') %>%
  html_table() %>% #vrest function to turn html table into df
  purrr::pluck(1) %>% # "pluck" what column you want
  janitor::clean_names() %>% #cleans up column name syntax
  mutate(across(c(adj_oe, adj_de), ~ as.numeric(.x))) %>% #convert columns to numeric data type
  select(team, adj_oe, adj_de)

ap_top_25 <- tibble(
  rank = 1:25,
  team = c('Kansas', 'Duke', 'Purdue', 'Michigan St.', 'Marquette', 'Connecticut', 'Houston', 'Creighton', 'Tennessee',
           'Florida Atlantic', 'Gonzaga', 'Arizona', 'Miami FL', 'Arkansas', 'Texas A&M', 'Kentucky', 'San Diego St.', 'Texas',
           'North Carolina', 'Baylor', 'USC', 'Villanova', "Saint Mary's", 'Alabama', 'Illinois')
)

ap_top_25 <- left_join(ap_top_25, rankings, by = 'team')

#CREATE R FUNCTION TO ADD IN EXTRA INFO FOR TOP 25
parse_wp <- function(team) {
  win_percentages <- schedule %>%
    filter(home == team | away == team) %>%#filter = picking rows to use
    
    mutate(
      #add game location
      game_location = case_when(
        home == team & neutral == 0 ~ 'home',
        away == team & neutral == 0 ~ 'away',
        .default = 'neutral'),
      
      team = team,
      opponent = if_else(team == home, away, home),
      
      avg_ap_adj_oe = mean(ap_top_25$adj_oe),
      avg_ap_adj_de = mean(ap_top_25$adj_de)
      ) %>%
    
    left_join(rankings %>% select(team, opp_adj_oe = adj_oe, opp_adj_de = adj_de), join_by('opponent' == 'team')) %>%
    left_join(ap_top_25 %>% select(-rank), by = 'team') %>%
    
    adjust_efficiency() %>%
    
    mutate(
      team_pythag = (adj_oe^11.5) / (adj_oe^11.5 + adj_de^11.5),
      avg_ap_pythag = (avg_ap_adj_oe^11.5) / (avg_ap_adj_oe^11.5 + avg_ap_adj_de^11.5),
      opp_pythag = (opp_adj_oe^11.5) / (opp_adj_oe^11.5 + opp_adj_de^11.5),
      team_wp = (team_pythag - team_pythag * opp_pythag) / (team_pythag + opp_pythag - 2 * team_pythag * opp_pythag),
      ap_wp = (avg_ap_pythag - avg_ap_pythag * opp_pythag) / (avg_ap_pythag + opp_pythag - 2 * avg_ap_pythag * opp_pythag)) %>% 
    
    select(team, team_wp, ap_wp)
}


game_preds <- map_dfr(ap_top_25$team, \(school) parse_wp(school))


#add team logos and make it pretty
teams <- read_csv('https://gist.github.com/andreweatherman/cd2a258b7a75dc75cd86940e29f28af9/raw/12a2cd5f919a681ab4711c4b5082385bacd78c2e/teams.csv') %>%
  select(common_team, logo)

game_preds_summarized <- game_preds_summarized %>%
  left_join(teams, join_by('team' == 'common_team')) %>%
  mutate(ap_rank = row_number(), .before = team) %>%
  mutate(team_logo = glue::glue("<img src='{logo}' style='height: 25px; width: auto; vertical-align: middle;'> {team}"))


game_preds_summarized %>%
  gt(id='ap') %>% #gt() creates empty table able to be stylized. id is arbitrary
  gt_theme_excel() %>% #choose from different templates (excel, athletic, 538, espn, etc)
  fmt_markdown(team_logo) %>% #lines up logos with rows
  cols_hide(c(team, logo)) %>% #hide extraneous columns
  cols_move(team_logo, after = ap_rank) %>% #order columns correctly
  cols_align(columns = c(team_wins, ap_wins, diff, ap_rank), align = 'center') %>% #center numbers/text in viz
  cols_align(columns = team_logo, align = 'left') %>% #format team logo and name
  cols_label(
    ap_rank = 'Rank',
    team_logo = 'Team',
    team_wins = 'TM Wins',
    ap_wins = 'AP Wins',
    diff = 'Diff.') %>%
  fmt_number(columns = c(team_wins, ap_wins, diff),decimals = 2) %>% #round numbers to two decimals where appropriate
  gt_hulk_col_numeric(diff) %>% #apply green/purple color palette to fill diff column based on difference
  tab_header(
    title = md("How Teams Stack Up to Avg. AP Top 25 Expectation"),
    subtitle = md("Venue-adjusted wins vs. expected wins by the avg top 25 team")) %>%
  tab_source_note(source_note = md('Data by Barttorvik<br>Analysis + Viz. by @andreweatherman(copied by chase)')) %>%
  # FORMATTING ALL THE TABLE STUFF ALL THE CODING IS BASICALLY DONE
  # bold diff col. text
  tab_style(
    style = cell_text(weight = 'bold'),
    location = cells_body(columns = diff)
  ) %>%
  # control padding on header
  tab_options(
    heading.padding = 3,
    table.font.size = 15
  ) %>%
  # title font
  tab_style(
    locations = cells_title('title'),
    style = cell_text(
      font = google_font('Fira Sans'),
      weight = 600,
      size = px(19)
    )
  ) %>%
  # subtitle font
  tab_style(
    locations = cells_title('subtitle'),
    style = cell_text(
      font = google_font('Fira Sans'),
      weight = 400,
      size = px(14)
    )
  ) %>%
  # source note font
  tab_style(
    locations = cells_source_notes(),
    style = cell_text(
      font = google_font('Fira Sans'),
      weight = 400,
      size = px(12)
    )
  ) %>%
  # columns labels
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = cell_text(
      font = google_font('Fira Sans'),
      weight = 600,
      size = px(14),
      transform = 'uppercase',
      align = 'center'
    )
  ) %>%
  # table font
  opt_table_font(
    font = google_font('Fira Sans'),
    weight = 450
  ) %>%
  opt_css(
    css = "
     #ap .gt_heading {
      padding-bottom: 0px;
      padding-top: 6px
     }
     #ap .gt_subtitle {
      padding-top: 2px;
      padding-bottom: 6px;
     }
    "
  ) %>%
  #SAVE TABLE AS PNG
  gtsave_extra('ap_25_expected.png')
