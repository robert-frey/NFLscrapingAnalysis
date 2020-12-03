#install.packages("nflfastR")
library(nflfastR)
#install.packages("rvest")
library(rvest)
#install.packages("xml2")
library(xml2)
#install.packages("tidyverse", dependencies = T)
library(tidyverse)
#install.packages("furrr")
library(furrr)

#### Scrape Game Schedules
game_ids = nflfastR::fast_scraper_schedules(2020)

#Read in csv file
codes = readr::read_csv("pfr_team_codes.csv")

#Join Game IDs and get correct game_ids for Pro Football Reference to scrape games and rosters
game_ids = game_ids %>% left_join(codes,by="home_team") %>% mutate(old_game_id = paste0(stringr::str_sub(old_game_id,1,8),"0"),
                                                                   pfr_site = paste0("https://www.pro-football-reference.com/boxscores/",old_game_id,pfr_code,".htm"),
                                                                   pfr_roster = paste0("https://www.pro-football-reference.com/teams/",pfr_code,"/2020-snap-counts.htm")) %>%
  dplyr::filter(is.na(home_result)==F)

#create df of rosters
roster_teams = game_ids %>% select(away_team,home_team,pfr_roster,pfr_rost_code) %>% distinct(pfr_roster, .keep_all = TRUE)
#url = "https://www.pro-football-reference.com/boxscores/202011160chi.htm"


#### Get offensive boxscores for each game ####
pfr_offense_boxscore <- function(url,type) {
game_box = url %>% xml2::read_html() %>% rvest::html_nodes("#player_offense") %>% rvest::html_table(trim = T) %>% data.frame(stringsAsFactors = F) %>%
  purrr::set_names(.,c("Player","Team","Completions","Passing_Attempts","Passing_Yards",
                       "Passing_TDs","Passing_Ints","Sacks","Sack_Yards_Lost","Passing_Long",
                       "Passing_Rate","Rushing_Attempts","Rushing_Yards","Rushing_TDs",
                       "Rushing_Long","Targets","Receptions","Receiving_Yards","Receiving_TDs",
                       "Receiving_Long","Fumbles","Fumbles_Lost")) %>%
  dplyr::filter(!(Player %in% c("", "Player"))) %>% 
  dplyr::mutate_at(vars(3:22), as.numeric)

passing_box = game_box %>% dplyr::select(Player,Team,3:11,21,22) %>% dplyr::filter(Passing_Attempts > 0)
rushing_box = game_box %>% dplyr::select(Player,Team,12:15,21,22) %>% dplyr::filter(Rushing_Attempts > 0)
receiving_box = game_box %>% dplyr::select(Player,Team,16:22) %>% dplyr::filter(Targets > 0)

#assign("passing_box_score", passing_box, envir = .GlobalEnv)
#assign("rushing_box_score", rushing_box, envir = .GlobalEnv)
#assign("receiving_box_score", receiving_box, envir = .GlobalEnv)

if (type == "Passing") {
  return(passing_box)
} else if (type == "Rushing") {
  return(rushing_box)
} else if (type == "Receiving") {
  return(receiving_box)
}

}



####rushing data####
rushing = 1:nrow(game_ids) %>% furrr::future_map(function(x) pfr_offense_boxscore(game_ids$pfr_site[x],type="Rushing") %>% dplyr::mutate(Away_Team = game_ids$away_team[x],
                                                                                                                                     Home_Team = game_ids$home_team[x],
                                                                                                                                     Week = game_ids$week[x]), .progress = T)




rushing = bind_rows(rushing)

####passing data####
passing = 1:nrow(game_ids) %>% furrr::future_map(function(x) pfr_offense_boxscore(game_ids$pfr_site[x],type="Passing") %>% dplyr::mutate(Away_Team = game_ids$away_team[x],
                                                                                                                                         Home_Team = game_ids$home_team[x],
                                                                                                                                         Week = game_ids$week[x]), .progress = T)


passing = bind_rows(passing)

####receiving data####
receiving = 1:nrow(game_ids) %>% furrr::future_map(function(x) pfr_offense_boxscore(game_ids$pfr_site[x],type="Receiving") %>% dplyr::mutate(Away_Team = game_ids$away_team[x],
                                                                                                                                         Home_Team = game_ids$home_team[x],
                                                                                                                                         Week = game_ids$week[x]), .progress = T)


receiving = bind_rows(receiving)


#### acquire roster data ####
#get_pfr_roster <- function(url) {
 # roster = url %>% xml2::read_html() %>% rvest::html_nodes("#snap_counts") %>% rvest::html_table(trim = T) %>% data.frame(stringsAsFactors = F) %>%
  #  purrr::set_names(.,c("Player","Pos","Offensive_Snaps","Offensive_Snaps_Pct","Defensive_Snaps",
   #                      "Defensive_Snaps_Pct","Special_Teams_Snaps","Special_Teams_Snaps_Pct","Extra_Column")) %>%
  #  dplyr::select(-"Extra_Column") %>%
  #  dplyr::filter(Pos %in% c("QB","RB","FB","WR","TE","K"))
  
#  return(roster)
#}


#get rosters of each team #
#rosters = 1:nrow(roster_teams) %>% furrr::future_map(function(x) get_pfr_roster(roster_teams$pfr_roster[x]) %>%
 #                                                    dplyr::mutate(Team = roster_teams$pfr_rost_code[x]))

#rosters = bind_rows(rosters)

#rosters = rosters %>% select(Player,Team,Pos)


###Nevermind nflfastR has a fast scraper roster function###
rosters = fast_scraper_roster(2020) %>% select(full_name,team,position) %>% set_names("Player","Team","Pos")


#### RB Data ####
rb_data_rush = left_join(rushing,rosters,by=c("Player","Team")) %>% mutate(Team = str_replace_all(Team,c("(NOR)" = "NO",
                                                                                                   "(SFO)" = "SF",
                                                                                                   "(GNB)" = "GB",
                                                                                                   "(TAM)" = "TB",
                                                                                                   "(LAR)" = "LA",
                                                                                                   "(KAN)" = "KC",
                                                                                                   "(LVR)" = "LV",
                                                                                                   "(NWE)" = "NE")),
                                                                     Off_Team = ifelse(Team==Home_Team,Home_Team,Away_Team),
                                                                     Def_Team = ifelse(Team==Home_Team,Away_Team,Home_Team)) %>%
  dplyr::filter(Pos == "RB")

rb_data_pass = left_join(receiving,rosters,by=c("Player","Team")) %>% mutate(Team = str_replace_all(Team,c("(NOR)" = "NO",
                                                                                                         "(SFO)" = "SF",
                                                                                                         "(GNB)" = "GB",
                                                                                                         "(TAM)" = "TB",
                                                                                                         "(LAR)" = "LA",
                                                                                                         "(KAN)" = "KC",
                                                                                                         "(LVR)" = "LV",
                                                                                                         "(NWE)" = "NE")),
                                                                           Off_Team = ifelse(Team==Home_Team,Home_Team,Away_Team),
                                                                           Def_Team = ifelse(Team==Home_Team,Away_Team,Home_Team)) %>%
  dplyr::filter(Pos == "RB")


rushing_data = left_join(rb_data_rush,rb_data_pass,by=c("Player","Team","Fumbles","Fumbles_Lost","Away_Team","Home_Team",
                                                        "Week","Pos","Off_Team","Def_Team"))

rushing_data = rushing_data %>% replace_na(list(Targets=0,Receptions=0,Receiving_Yards=0,Receiving_TDs=0))

team_data = rushing_data %>% filter(Pos == "RB") %>% group_by(Def_Team,Week) %>%
  summarise(
            Att = sum(Rushing_Attempts),
            Rush_Yards = sum(Rushing_Yards),
            Rush_TDs = sum(Rushing_TDs),
            Targets = sum(Targets),
            Rec = sum(Receptions),
            Rec_Yards = sum(Receiving_Yards),
            Rec_TDs = sum(Receiving_TDs),
            Fumbles_Lost = sum(Fumbles_Lost)) %>% ungroup() %>%
  #Feel free to change the multiplier for Receptions, since it varies by PPR and non-PPR leagues
  mutate(FP = ((.1*Rush_Yards)+(6*Rush_TDs)+(-2*Fumbles_Lost)) + ((.5*Rec)+(.1*Rec_Yards)+(6*Rec_TDs))) %>%
  group_by(Def_Team) %>%
  dplyr::summarise(Games = n(),
                   FP = sum(FP),
                   Att = sum(Att),
                   Rush_Yards = sum(Rush_Yards),
                   Rush_TDs = sum(Rush_TDs),
                   Targets = sum(Targets),
                   Rec = sum(Rec),
                   Rec_Yards = sum(Rec_Yards),
                   Rec_TDs = sum(Rec_TDs)) %>% ungroup() %>%
  dplyr::mutate(Targ_G = round(Targets/Games),
                Rec_G = round(Rec/Games,0),
                Att_G = round(Att/Games),
                Rush_Yards_G = round(Rush_Yards/Games),
                Rec_Yards_G = round(Rec_Yards/Games),
                Rush_TDs_G = round(Rush_TDs/Games,1),
                Rec_TDs_G = round(Rec_TDs/Games,1),
                FPA_G = round(FP/Games,1))

#### WR Data ####
receiving_data = left_join(receiving,rosters,by=c("Player","Team")) %>% mutate(Team = str_replace_all(Team,c("(NOR)" = "NO",
                                                                                                             "(SFO)" = "SF",
                                                                                                             "(GNB)" = "GB",
                                                                                                             "(TAM)" = "TB",
                                                                                                             "(LAR)" = "LA",
                                                                                                             "(KAN)" = "KC",
                                                                                                             "(LVR)" = "LV",
                                                                                                             "(NWE)" = "NE")),
                                                                               Off_Team = ifelse(Team==Home_Team,Home_Team,Away_Team),
                                                                               Def_Team = ifelse(Team==Home_Team,Away_Team,Home_Team)) %>%
  dplyr::filter(Pos == "WR")

team_wr_data = receiving_data %>% filter(Pos == "WR") %>% group_by(Def_Team,Week) %>%
  summarise(
    Targets = sum(Targets),
    Rec = sum(Receptions),
    Rec_Yards = sum(Receiving_Yards),
    Rec_TDs = sum(Receiving_TDs),
    Fumbles_Lost = sum(Fumbles_Lost)) %>% ungroup() %>%
  #Feel free to change the multiplier for Receptions, since it varies by PPR and non-PPR leagues
  mutate(FP = (-2*Fumbles_Lost) + ((.5*Rec)+(.1*Rec_Yards)+(6*Rec_TDs))) %>%
  group_by(Def_Team) %>%
  dplyr::summarise(Games = n(),
                   FP = sum(FP),
                   Targets = sum(Targets),
                   Rec = sum(Rec),
                   Rec_Yards = sum(Rec_Yards),
                   Rec_TDs = sum(Rec_TDs)) %>% ungroup() %>%
  dplyr::mutate(Targ_G = round(Targets/Games),
                Rec_G = round(Rec/Games,0),
                Rec_Yards_G = round(Rec_Yards/Games,0),
                Rec_TDs_G = round(Rec_TDs/Games,1),
                FPA_G = round(FP/Games,1))


#### TE Data ####

#ugly way of getting correct number of games, since some games, there was no targets or attempts to a TE
te_merge = select(team_wr_data,Def_Team,Games)

te_data = left_join(receiving,rosters,by=c("Player","Team")) %>% mutate(Team = str_replace_all(Team,c("(NOR)" = "NO",
                                                                                                             "(SFO)" = "SF",
                                                                                                             "(GNB)" = "GB",
                                                                                                             "(TAM)" = "TB",
                                                                                                             "(LAR)" = "LA",
                                                                                                             "(KAN)" = "KC",
                                                                                                             "(LVR)" = "LV",
                                                                                                             "(NWE)" = "NE")),
                                                                               Off_Team = ifelse(Team==Home_Team,Home_Team,Away_Team),
                                                                               Def_Team = ifelse(Team==Home_Team,Away_Team,Home_Team)) %>%
  dplyr::filter(Pos == "TE")



team_te_data = te_data %>% filter(Pos == "TE") %>% group_by(Def_Team,Week) %>%
  summarise(
    Targets = sum(Targets),
    Rec = sum(Receptions),
    Rec_Yards = sum(Receiving_Yards),
    Rec_TDs = sum(Receiving_TDs),
    Fumbles_Lost = sum(Fumbles_Lost)) %>% ungroup() %>%
  mutate(FP = (-2*Fumbles_Lost) + ((.5*Rec)+(.1*Rec_Yards)+(6*Rec_TDs))) %>%
  group_by(Def_Team) %>%
  dplyr::summarise(
                   FP = sum(FP),
                   Targets = sum(Targets),
                   Rec = sum(Rec),
                   Rec_Yards = sum(Rec_Yards),
                   Rec_TDs = sum(Rec_TDs)) %>% ungroup() %>%
  dplyr::left_join(te_merge,by="Def_Team") %>%
  dplyr::mutate(Targ_G = round(Targets/Games),
                FPA_G = round(FP/Games,1),
                Rec_G = round(Rec/Games,0)) %>%
  dplyr::select(Def_Team,Games, everything())