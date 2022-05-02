library(tidyverse)

elo = read.csv("elo.csv") %>%
  select(Team, Elo)

hockey_data <- read.csv("~/DSCI478-Final/hockeydata.csv") %>%
  na.omit() %>%
  mutate(Visitor = toupper(Visitor),
         Home = toupper(Home))

basic_stats = read.delim("basic_team_stats.txt", sep = ",") %>%
  rename(Team = X)%>%
  filter(Team != "League Average") %>%
  mutate(Team = gsub("[*].*$", "", Team),
         Team = toupper(Team)) %>%
  select(-Rk)

analytical_stats = read.delim("analytical_stats.txt", sep = ",") %>%
  rename(Team = X) %>%
  mutate(Team = gsub("[*].*$", "", Team),
         Team = toupper(Team)) %>%
  select(-S., -SV., -Rk)

all_team_stats = basic_stats %>%
  inner_join(analytical_stats, by = "Team")

df = hockey_data %>%
  inner_join(all_team_stats, by = c("Visitor" = "Team")) %>%
  inner_join(all_team_stats, by = c("Home" = "Team")) %>%
  inner_join(elo, by = c("Visitor" = "Team")) %>%
  inner_join(elo, by = c("Home" = "Team")) %>%
  select(-X)

names(df) = gsub("\\.x","_visitor", names(df))
names(df) = gsub("\\.y","_home", names(df))
names(df) = gsub('\\.',"_pct", names(df))

df = df %>%
  mutate(home_win = case_when(
    (home_goals > away_goals) ~ "1",
    TRUE ~ "0"))


