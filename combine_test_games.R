library(tidyverse)

test_games = read.delim("test_games.txt", sep = ",") %>%
  na.omit() %>%
  rename(home_goals = G.1, 
         away_goals = G) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date > "2022-4-3") %>%
  mutate(home_win = case_when(
    (home_goals > away_goals) ~ "1",
    TRUE ~ "0"),
    Visitor = toupper(Visitor),
    Home = toupper(Home)) %>%
  select(Date, Visitor, Home, home_win)

df = test_games %>%
  inner_join(all_team_stats, by = c("Visitor" = "Team")) %>%
  inner_join(all_team_stats, by = c("Home" = "Team")) %>%
  inner_join(elo, by = c("Visitor" = "Team")) %>%
  inner_join(elo, by = c("Home" = "Team"))

names(df) = gsub("\\.x","_visitor", names(df))
names(df) = gsub("\\.y","_home", names(df))
names(df) = gsub('\\.',"_pct", names(df))


write.csv(df, "test_data.csv")
