library(tidyverse)
# Bootstrapping
hockey_data <- read.csv("~/DSCI478-Final/hockeydata.csv") %>%
  na.omit() %>%
  mutate(Visitor = toupper(Visitor),
         Home = toupper(Home))



bootstrap = function(home_team, away_team, n = 10000){
  home_team1 = hockey_data %>%
    filter(Home == home_team)
  
  away_team1 = hockey_data %>%
    filter(Visitor == away_team)
  
  home_GF = sample(home_team1$home_goals, n, replace = T)
  home_GA = sample(home_team1$away_goals, n, replace = T)
  
  away_GF = sample(away_team1$away_goals, n, replace = T)
  away_GA = sample(away_team1$home_goals, n, replace = T)
  
  home_xG = rowMeans(cbind(home_GF, away_GA))
  away_xG = rowMeans(cbind(away_GF, home_GA))
  
  home_xG_mean = mean(home_xG)
  away_xG_mean = mean(away_xG)
  
  df1 = data.frame(Avalanche = home_xG, Oilers = away_xG) %>%
    pivot_longer(everything(),values_to = "xG", names_to = "team")
  
  home_win_pct = mean(home_xG > away_xG)
  away_win_pct = mean(home_xG < away_xG)
  tie_pct = mean(home_xG == away_xG)
  
  cat(home_team, " xG: ", home_xG_mean, "\n")
  cat(away_team, " xG: ", away_xG_mean, "\n")
  cat(home_team, " Win %: ", home_win_pct, "\n")
  cat(away_team, " Win %: ", away_win_pct, "\n")
  cat("Tie %: ", tie_pct)
  
  ggplot(df1) +
    geom_density(aes(x = xG, fill = team), position = "identity", adjust = 5, alpha = 0.5) +
    labs(x = "Expected Goals (xG)",
         fill = "Team", y = "Density",
         title = "Colorado Avalanche vs. Edmonton Oilers") +
    scale_fill_manual(values = c("Avalanche" = "darkred", "Oilers" = "orange")) +
    theme_minimal()
  
}
# 460 430
bootstrap("COLORADO AVALANCHE", "EDMONTON OILERS", 1000000)


# df is the test data set created in 'combine_test_games'
n = 100000
for(i in 1:nrow(df)) {
  print(paste("Game", i, "of", nrow(df)))
  home_team1 = hockey_data %>%
    filter(Home == df$Home[i])
  
  away_team1 = hockey_data %>%
    filter(Visitor == df$Visitor[i])
  
  home_GF = sample(home_team1$home_goals, n, replace = T)
  home_GA = sample(home_team1$away_goals, n, replace = T)
  
  away_GF = sample(away_team1$away_goals, n, replace = T)
  away_GA = sample(away_team1$home_goals, n, replace = T)
  
  home_xG = rowMeans(cbind(home_GF, away_GA))
  away_xG = rowMeans(cbind(away_GF, home_GA))
  
  home_xG_mean = mean(home_xG)
  away_xG_mean = mean(away_xG)
  
  
  home_win_pct = mean(home_xG > away_xG)
  away_win_pct = mean(home_xG < away_xG)
  tie_pct = mean(home_xG == away_xG)
  
  df$home_win_pct[i] = home_win_pct
  df$away_win_pct[i] = away_win_pct
  df$tie_pct[i] = tie_pct
 
}

# Bootstrapping accuracy on new data
df %>%
  mutate(home_pred_win = case_when(
    home_win_pct > away_win_pct ~ 1,
    TRUE ~ 0)) %>%
  summarise(accuracy = mean(home_win == home_pred_win))
