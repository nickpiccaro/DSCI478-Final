
# Poisson and beta test accuracy
df = read.csv("test_data.csv")
hockey_data = read.csv("~/DSCI478-Final/hockeydata.csv") %>%
  na.omit() %>%
  mutate(Visitor = toupper(Visitor),
         Home = toupper(Home))


set.seed(1)
# df is the test data set created in 'combine_test_games'
for(i in 1:nrow(df)) {
  print(paste("Game", i, "of", nrow(df)))
  
  
  # Filter and find average goals scored and allowed by away team
  away_team1 = hockey_data %>%
    filter(Visitor == df$Visitor[i]) %>%
    summarise(avg_gf_visit = mean(away_goals, na.rm = T),
              avg_ga_visit = mean(home_goals, na.rm = T)) 
  
  # Filter and find average goals scored and allowed by home team
  home_team1 = hockey_data %>%
    filter(Home == df$Home[i]) %>%
    summarise(avg_gf_home = mean(home_goals, na.rm = T),
              avg_ga_home = mean(away_goals, na.rm = T)) 
  
  
  # Get league average home/away goals score
  avg_gf_home_league <- mean(hockey_data$home_goals, na.rm = T)
  avg_gf_away_league <- mean(hockey_data$away_goals, na.rm = T)
  
  # Compare home and away team offense to league average
  home_attack <- home_team1$avg_gf_home/avg_gf_home_league
  away_attack <- away_team1$avg_gf_visit/avg_gf_away_league
  
  # Compare home and away team defense to league average
  home_defense <- home_team1$avg_ga_home/avg_gf_away_league
  away_defense <- away_team1$avg_ga_visit/avg_gf_home_league
  
  # Find expected goals
  home_exp_gf <- round(home_attack * away_defense * avg_gf_home_league, 2)
  away_exp_gf <- round(away_attack * home_defense * avg_gf_away_league, 2)
  
  # Get sampling distribution
  home_xG_pois = rpois(250000, home_exp_gf)
  away_xG_pois = rpois(250000, away_exp_gf)
  
  away_xG_beta = rbeta(250000, away_exp_gf, home_exp_gf)
  home_xG_beta = rbeta(250000, home_exp_gf, away_exp_gf)
  
  # Get winning percentage by distribution
  home_in_reg <- mean(home_xG_pois > away_xG_pois)
  away_in_reg <- mean(home_xG_pois < away_xG_pois)
  tie_in_reg <- round((1-home_in_reg-away_in_reg)/2, 2)
  
  home_beta <- mean(home_xG_beta)
  away_beta <- mean(away_xG_beta)


  
  df$pois_home_win_pct[i] = home_in_reg
  df$pois_away_win_pct[i] = away_in_reg
  
  df$beta_home_win_pct[i] = home_beta
  df$beta_away_win_pct[i] = away_beta
  

  
}

# Poisson accuracy on new data
df %>%
  mutate(home_pred_win = case_when(
    pois_home_win_pct > pois_away_win_pct ~ 1,
    TRUE ~ 0)) %>%
  summarise(accuracy = mean(home_win == home_pred_win))

# Beta accuracy on new data
df %>%
  mutate(home_pred_win = case_when(
    beta_home_win_pct > beta_away_win_pct ~ 1,
    TRUE ~ 0)) %>%
  summarise(accuracy = mean(home_win == home_pred_win))
