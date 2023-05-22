#Libraries used:
library(ggplot2)
library(engsoccerdata)
library(understatr)
library(dplyr)
library(lubridate)
library(ggforce)
library(ggrepel)

#Code to create Dataset:
  
  team_data <- get_league_teams_stats("EPL", year =2020)
team_chelsea <- team_data %>%
  filter(team_name == "Chelsea") %>%
  mutate(week = row_number())
team_chelsea <- as.data.frame(team_chelsea)

team_chelsea$date <- ymd(team_chelsea$date)
team_chelsea <- team_chelsea %>%
  mutate(manager = ifelse(date < "2021-01-26","Lampard","Tuchel"))
team_chelsea

player_chelsea <- get_team_players_stats(team_name = "Chelsea", year = 2020) %>% 
  mutate(avg_xG_per_min = xG / (time/90) ) %>% 
  mutate(avg_xA_per_min = xA / (time/90) ) %>%
  filter(goals != 0 & assists != 0) %>% 
  filter(position != "GK" & time >= 500) %>% 
  filter(position != 'D')
player_chelsea <- as.data.frame(player_chelsea)
player_chelsea

avg_metrics <- team_chelsea %>%
  group_by(manager) %>%
  summarize(avg_xG = mean(xG),
            avg_xGA = mean(xGA),
            avg_npxG = mean(npxG),
            avg_npxGA = mean(npxGA),
            avg_deep = mean(deep),
            avg_deep_allowed = mean(deep_allowed),
            win_rate = mean(result == "w"),
            draw_rate = mean(result == "d"),
            loss_rate = mean(result == "l"))

avg_metrics_melt <- avg_metrics %>%
  gather(key = "metric", value = "value", -manager)


##Code to create Manager Comparison Barplot:
  
  dev.new(3,2)
ggplot(avg_metrics_melt, aes(x = manager, y = value, fill = manager)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Manager", y = "Value", title = "Comparison of Managers' Tactics and Results")

##Code to create Player Comparison Point plot:
  dev.new(3,4)
  ggplot(player_chelsea, aes(x=avg_xA_per_min,y=avg_xG_per_min)) +
  geom_point(stat ="identity",color='#777777') +
  geom_text_repel(aes(label=player_name),color='#777777')+
  labs(title='Chelsea players average xG/XA per 90 minutes 2020/2021',
       x='average xA',
       y='average xG')
