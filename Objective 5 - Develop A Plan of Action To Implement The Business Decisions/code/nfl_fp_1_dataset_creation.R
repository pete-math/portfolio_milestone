library(tidyverse)
library(janitor)
library(formattable)
#install.packages("sqldf")
library(sqldf)
library(plyr)
library(arules)
library(arulesViz)


################## to-do: will work out a better config set-up
# set the working directory
## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

cwd

#cwd <- "FINALCODE/final_project"
#setwd(cwd)

source(paste0("FINALCODE/final_files/config.R"))


#####Step 1: IMPORT FLAT FILES



#IMPORT GAMES AND FILTER DOWN TO SEASON 2018
game_armchair <- read_csv("FINALCODE/final_files/data/armchair_analysis_nfl_00-18/Game.csv")
game_armchair_18_df <- game_armchair %>% filter(seas==2018) %>%
  mutate(
    Day.of.Week=day,
    Home=h,
    Humidity=humd,
    NFL.Week=wk,
    OverUnder=ou,
    Season=seas,
    Spread=sprv,
    Stadium=stad,
    StadiumSurface=surf,
    Temperature=temp,
    Vistor=v,
    Weather=cond,
    WindDirection=wdir,
    WindSpeed=wspd,
    Year=seas
  )%>% select(
              gid,
              Day.of.Week	,
              Home	,
              Humidity	,
              NFL.Week	,
              OverUnder	,
              Season	,
              Spread	,
              Stadium	,
              StadiumSurface	,
              Temperature	,
              Vistor	,
              Weather	,
              WindDirection	,
              WindSpeed	,
              Year)


head(game_armchair_18_df)
sqldf("select * from game_armchair_18_df where Season=2018") %>% formattable()

# CREATE A DF WITH JUST THE GAME ID AND WEEK - NEED THIS LATER


#IMPORT OFFENSE
player_injury_armchair <- read_csv("FINALCODE/final_files/data/armchair_analysis_nfl_00-18/INJURY.csv")
glimpse(player_injury_armchair)

# check data
sqldf("select * from player_injury_armchair limit 5") %>% formattable()
sqldf("select distinct gstat from player_injury_armchair") %>% formattable()


team_off_armchair <- read_csv("FINALCODE/final_files/data/armchair_analysis_nfl_00-18/OFFENSE.csv")
glimpse(team_off_armchair)
team_off_armchair_df <- team_off_armchair %>% filter(year==2018) %>%
  mutate (
    Touchdowns = tdp+tdr+tdrec,
    PassAttemptsPerGame = pa,
    PTDs = tdp,
    DepthChartPos = dcp,
    Game_ID = gid,
    GameNumber = gid,
    NFLcom_Points = fp,
    Player_ID = player,
    SeasonsPlayed = seas,
    Snaps = snp,
    SnapCount = snp,
    TeamFP = fp,
    Unique_ID = uid,
    Yards = ry + rety+ py,
    RRYPG = ry + rety,
    RTDs = tdr + tdrec,
    Turnovers = fuml + ints,
    TouchesTargetsPerGame = ra + trg,
    NFL_Season = seas	,
  ) %>% 
  select(
    gid,
    Team=team,
    Touchdowns,
    Yards,
    PassAttemptsPerGame,
    PTDs,
    SnapCount,
    DepthChartPos,
    Game_ID,
    GameNumber,
    NFLcom_Points,
    Player_ID,
    SeasonsPlayed,
    Snaps,
    TeamFP,
    Unique_ID,
    RRYPG,
    RTDs,
    Turnovers,
    TouchesTargetsPerGame,
    NFL_Season 
  )

sqldf("select distinct DepthChartPos from team_off_armchair_df  ") %>% formattable()


#IMPORT PLAYERS
player_armchair <- read_csv("FINALCODE/final_files/data/armchair_analysis_nfl_00-18/PLAYER.csv")
glimpse(player_armchair)
sqldf("select * from player_armchair limit 5") %>% formattable()


player_armchair_df <- player_armchair %>%
mutate(
  PlayerName	=	paste(fname, lname),
  Position	=	pos1,
  Player_ID = player
) %>% select (
  Player_ID,
  PlayerName,
  Position
  )

sqldf("select * from player_armchair_df where PlayerName like '%mar%' limit 10 ") %>% formattable()

#IMPORT INJURIES
player_injury_armchair <- read_csv("FINALCODE/final_files/data/armchair_analysis_nfl_00-18/INJURY.csv")
glimpse(player_injury_armchair)

sqldf("select * from player_injury_armchair limit 5") %>% formattable()

game_injury_status <- sqldf("select distinct i.gid, i.player, gstat,
  case when gstat in ('Out','Doubtful','Inactive','Suspend','IR','PUP') then 1
 when gstat in ('Questionable','Probable','Available','Full','Limited','DNP') then 2
  when gstat is null then 4
  else 4
  end as injury,  f.team
    from player_injury_armchair i
     join team_off_armchair f on   i.player = f.player and i.gid = f.gid and f.Team = i.Team
      order by  1 ") 
game_injury <- sqldf("select g.gid,g.player,g.injury,g.team 
                     from game_injury_status g
                     order by 1 ")


sqldf("select * from game_injury 
                     where player IN ('NF-0250','MW-0500','TC-2050','JA-0450','DC-1050') 
                     and gid = 4791
                     order by 1 ")

# import coaching tiers
coach_tiers <- read_csv("FINALCODE/final_files/data/coach_tiers.csv")
glimpse(coach_tiers)
sqldf("select * from coach_tiers limit 5") %>% formattable()

# import coaching tiers
defensive_index <- read_csv("FINALCODE/final_files/data/defensive_index.csv")
glimpse(defensive_index)
sqldf("select * from defensive_index limit 5") %>% formattable()


defensive_index <- sqldf("select distinct gid, i.*
     from game_armchair_18_df d
      left join defensive_index i on i.OpposingDefense in (d.home,Vistor)
      ") %>% formattable()


defensive_index %>% formattable()

#join team_off_armchair t
#join game_armchair_18 g on t.gid = g.gid
#join player_armchair p on t.player=p.player

glimpse(game_data)

# import market share data
market_share_index <- read_csv("FINALCODE/final_files/data/fantasy_market_share.csv")
glimpse(market_share_index)
sqldf("select * from market_share_index limit 5") %>% formattable()


game_data <-  join(game_armchair_18_df,team_off_armchair_df, by=c("gid"))
game_data <-  join(game_data,player_armchair_df, by=c("Player_ID"="Player_ID"))
game_data <-  left_join(game_data,coach_tiers, by=c("Team" = "team"))
game_data <- left_join(game_data,game_injury, by=c("gid" = "gid","Player_ID" = "player", "Team"="team"))
game_data <- left_join(game_data,market_share_index, by=c("gid" = "Game_ID","Player_ID" = "Player_ID"))

game_data$injury[is.na(game_data$injury)] <- 3

opposing_defense <- sqldf("select i.*,Player_ID,Team
     from game_data g
      join defensive_index i on  i.gid=g.gid and g.Team <> i.OpposingDefense
      ") %>% formattable()


game_data <- left_join(game_data,opposing_defense, by=c("gid" = "gid","Player_ID" = "Player_ID"))

game_data$Away <- game_data$Vistor
game_data$PlayInjuryStatus <- game_data$injury
game_data$Coach.Tier <- game_data$coach_tier
 

sqldf("select * from game_data where gid = 4791  ") %>% formattable()


###################### 7 ADD INJURY AND RESET HOME ##################################



game_data$PlayerName %>% formattable()

game_team_data_sum <- game_data %>% 
  group_by(Game_ID,Team.x) %>% 
  dplyr::summarize(
    TeamFP = sum(TeamFP)
  ) %>% ungroup(.)  %>% 
  select(
    Team = Team.x,
    Game_ID,
    TeamFP
  )  %>% 
  formattable()
game_data <- left_join(game_data,game_team_data_sum, by=c("Game_ID" = "Game_ID", "Team.x"="Team"))


game_data_avg <- game_data %>% 
  group_by(Team.x,Player_ID) %>% 
  #select(uid,gid, seas,team,player,pa,pc,py,ints,tdp,ra,sra,ry,tdr,trg,rec,recy,tdrec,ret,rety,tdret,fuml,
   # peny,conv,snp,fp,fp2,fp3,posd,jnum,dcp) %>%
  dplyr::summarize(
    Average_PassAttemptsPerGame = mean(PassAttemptsPerGame),
    Average_PTDs = mean(PTDs),
    Average_PYPG = mean(Yards),
    Average_RRYPG = mean(RRYPG),
    Average_RTDs = mean(RTDs),
    Average_SnapCount = mean(SnapCount),
    Average_TouchesTargetsPerGame =  mean(TouchesTargetsPerGame),
    Average_Turnovers = mean(Turnovers),
   ) %>% ungroup(.)  %>% 
  select(
    Team = Team.x,
    Player_ID,
    Average_RRYPG,
    Average_PYPG,
    Average_RTDs,
    Average_PTDs,
    Average_PassAttemptsPerGame,
    Average_TouchesTargetsPerGame,
    Average_Turnovers,
    Average_SnapCount,
   )  %>% 
  formattable()

team_off_aa_18  %>%
  formattable()


game_data <- left_join(game_data,game_data_avg, by=c("Player_ID" = "Player_ID", "Team.x"="Team"))
sqldf("select * from game_data where gid=4791") %>% formattable()

game_data <- game_data %>%
  mutate(GameNumber = NFL.Week) %>%
    select(
      Game_ID	,
      Player_ID	,
      PlayerName	,
      Position	,
      Team = Team.x	,
      NFLcom_Points	,
      PlayInjuryStatus	,
      Average_RRYPG	,
      Average_PYPG	,
      Average_RTDs	,
      Average_PTDs	,
      Average_PassAttemptsPerGame	,
      Average_TouchesTargetsPerGame	,
      SeasonsPlayed	,
      DepthChartPos	,
      Coach.Tier	,
      OpposingDefense	,
      DefensiveIndex	,
      OpposingDefenseTier	,
      Average_Turnovers	,
      TeamFP = TeamFP.y	,
      Average_SnapCount	,
      Snaps	,
      GameNumber,
      Spread	,
      OverUnder	,
      StadiumSurface	,
      Weather	,
      WindDirection	,
      Humidity	,
      Temperature	,
      Stadium	,
      WindSpeed	,
      Home	,
      Vistor	,
      Year	,
      NFL_Season	,
      Season	,
      NFL.Week	= NFL.Week	,
      Day.of.Week	,
      MarketShareofOffense	,
      FantasyPointsTier	,
      HomevsAway
    )

sqldf("select * from game_data where Game_ID=4791") %>% formattable()

########### 6
########## SIMPLE EDA and Plots ##########################
library(DataExplorer)
plot_str(game_data)
plot_missing(game_data)
plot_histogram(game_data)
freq(fp_player_ds)
plot_num(game_data)
plot_correlation(game_data, type = 'continuous','Team')

library(VIM)
aggr(game_data, numbers = TRUE, prop = c(TRUE, FALSE))







#aa1 <- read.csv("FINALCODE/final_files/data/ArmchairAnalysis2018.csv")


#sqldf("select * from aa1  where Game_ID = 4791 order by 2") %>%   formattable()
write_csv(game_data,"FINALCODE/final_files/game_data.csv")

#Inspect data
#glimpse(aa1) %>% formattable()