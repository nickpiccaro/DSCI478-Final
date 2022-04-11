library(tidyverse)
# Bootstrapping
hockey_data <- read.csv("~/DSCI478-Final/hockeydata.csv") %>%
  na.omit()

bootstrap = function(home_team, away_team, n){
  home_team1 = hockey_data %>%
    filter(Home == home_team)
  
  away_team1 = hockey_data %>%
    filter(Visitor == away_team)
  
  home_GF = sample(home_team1$home_goals, n, replace = T)
  home_GA = sample(home_team1$away_goals, n, replace = T)
  
  away_GF = sample(away_team1$home_goals, n, replace = T)
  away_GA = sample(away_team1$away_goals, n, replace = T)
  
  home_xG = rowMeans(cbind(home_GF, away_GA))
  away_xG = rowMeans(cbind(away_GF, home_GA))
  
  home_xG_mean = mean(home_xG)
  away_xG_mean = mean(away_xG)
  
  home_win_pct = mean(home_xG > away_xG)
  away_win_pct = mean(home_xG < away_xG)
  tie_pct = mean(home_xG == away_xG)
  
  cat(home_team, " xG: ", home_xG_mean, "\n")
  cat(away_team, " xG: ", away_xG_mean, "\n")
  cat(home_team, " Win %: ", home_win_pct, "\n")
  cat(away_team, " Win %: ", away_win_pct, "\n")
  cat("Tie %: ", tie_pct)
  
  
}

bootstrap("Colorado Avalanche", "Florida Panthers", 10000)
bootstrap("Florida Panthers","Colorado Avalanche", 10000)
bootstrap("Dallas Stars", "Winnipeg Jets", 10000)


