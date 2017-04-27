library(plyr)
library(dplyr)



### Read data
age = read.csv("age.csv", stringsAsFactors = F)
key = read.csv("key.csv", stringsAsFactors = F)
results = read.csv("results.csv", stringsAsFactors = F)
table = read.csv("table.csv", stringsAsFactors = F)
wages = read.csv("wages.csv", stringsAsFactors = F)



### Find all goals conceded and scored by team
home.scored = ddply(results, .(home_name), summarise, home_scored = sum(home_score))
home.conceded = ddply(results, .(home_name), summarise, home_conceded = sum(away_score))
away.scored = ddply(results, .(away_name), summarise, away_scored = sum(away_score))
away.conceded = ddply(results, .(away_name), summarise, away_conceded = sum(home_score))
scored = home.scored %>% left_join(away.scored, by=c("home_name" = "away_name"))
scored = data.frame(Club = scored$home_name, total_scored = scored$home_scored + scored$away_scored)
conceded = home.conceded %>% left_join(away.conceded, by = c("home_name" = "away_name"))
conceded = data.frame(Club = conceded$home_name, total_conceded = conceded$home_conceded + conceded$away_conceded)



### Create unified dataset for analysis
epl = key %>% left_join(age) %>% left_join(table) %>% left_join(wages) %>% left_join(scored) %>% left_join(conceded)