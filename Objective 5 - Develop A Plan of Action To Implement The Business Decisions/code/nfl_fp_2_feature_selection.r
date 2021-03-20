#Load relevant packages

library(arules)
library(arulesViz)
library(dplyr)
library(tidyverse)
library(formattable)
#Read in Data Set

#aa1 <- read.csv("Classes/IST707/FINALCODE/ArmchairAnalysis2018.csv")
aa1 <- read.csv("FINALCODE/final_files/game_data.csv")

head(aa1) %>% formmattable()
#Inspect data
glimpse(aa1) %>% formattable()

str(aa1)

#Remove ID Field

aa <- aa1[,-1]

#Convert discretize & convert to factor continuous or int variables

# as.factor
aa$Game_ID  <- as.factor(aa$Game_ID)
aa$PlayInjuryStatus <- as.factor(aa$PlayInjuryStatus)
aa$SeasonsPlayed <- as.factor(aa$SeasonsPlayed)
aa$DepthChartPos <- as.factor(aa$DepthChartPos)
aa$Coach.Tier <- as.factor(aa$Coach.Tier)
aa$OpposingDefenseTier <- as.factor(aa$OpposingDefenseTier)
aa$GameNumber <- as.factor(aa$GameNumber)
aa$Year <- as.factor(aa$Year)
aa$NFL_Season <- as.factor(aa$NFL_Season)
aa$Season <- as.factor(aa$Season)
aa$NFL.Week <- as.factor(aa$NFL.Week)
aa$Day.of.Week <- as.factor(aa$Day.of.Week)
aa$FantasyPointsTier <- as.factor(aa$FantasyPointsTier)

# discretized
aa$NFLcom_Points <- discretize(aa$NFLcom_Points)
aa$Average_RRYPG <- discretize(aa$Average_RRYPG)
aa$Average_RTDs <- discretize(aa$Average_RTDs)
aa$Average_TouchesTargetsPerGame <- discretize(aa$Average_TouchesTargetsPerGame) 
aa$DefensiveIndex <- discretize(aa$DefensiveIndex)
aa$TeamFP <- discretize(aa$TeamFP)
aa$Average_SnapCount <- discretize(aa$Average_SnapCount)
aa$Snaps <- discretize(aa$Snaps)
aa$Spread <- discretize(aa$Spread)
aa$OverUnder <- discretize(aa$OverUnder)
aa$Humidity <- discretize(aa$Humidity)
aa$MarketShareofOffense <- discretize(aa$MarketShareofOffense)

aa$Average_PYPG <- as.factor(aa$Average_PYPG) #some breaks are not unique
aa$Average_PTDs <- as.factor(aa$Average_PTDs) #some breaks are not unique
aa$Average_PassAttemptsPerGame <- as.factor(aa$Average_PassAttemptsPerGame) #some breaks are not unique
aa$Average_Turnovers <- as.factor(aa$Average_Turnovers) #some breaks are not unique


#Create high fantasy points tier

aaHigh <- aa[aa$FantasyPointsTier == "3",]

#Convert data set to transactions

aatransactions <- as(aa, "transactions")
aaHightransactions <- as(aaHigh, "transactions")

#Inspect the data sets

summary(aatransactions)

#Create Item Frequency Plots to visually explore the data

itemFrequencyPlot(aatransactions, support=.5)
itemFrequencyPlot(aatransactions, support=.6)
itemFrequencyPlot(aatransactions, support=.7)
itemFrequencyPlot(aatransactions, support=.8)
itemFrequencyPlot(aatransactions, support=.9)

itemFrequencyPlot(aaHightransactions, support=.5)
itemFrequencyPlot(aaHightransactions, support=.6)
itemFrequencyPlot(aaHightransactions, support=.7)
itemFrequencyPlot(aaHightransactions, support=.8)
itemFrequencyPlot(aaHightransactions, support=.9)


#Mine the association rules using the apriori algorithm

aaRules <- apriori(aatransactions, parameter = list(support=0.3, confidence=.3))
aahighRules <- apriori(aaHightransactions, parameter = list(support=0.3, confidence=.3))

# Reusing variable names here.. rerunning code based on what configuration I want to look at

#aaRules <- apriori(aatransactions, parameter = list(support=0.05, confidence=.9,minlen=2),
#                   appearance = list(default="lhs", rhs="FantasyTier=3"))

aahighRules <- apriori(aaHightransactions, parameter = list(support=0.05, confidence=.7,minlen=2),
)


#Inspect mined rules

inspect(aaRules)
inspect(aaHighRules)

#Plot rules to visually inspect rules by support/confidence/lift

plot(aaRules)
plot(aaHighRules)

#Sort and inspect rules by confidence and lift

aaSortedConf <- sort (aaRules, by="confidence", decreasing=TRUE)
sort (aaHighRules, by="confidence", decreasing=FALSE)

sort (aaRules, by="lift", decreasing=TRUE)
sort (aaHighRules, by="lift", decreasing=FALSE)


#Inspect and Vizualize Rules

aaRulesTop10 <- aaSortedConf[0:15,]
plot(aaRulesTop10 , method = "two-key plot")
plot(aaRulesTop10 , method = "paracoord")
plot(aaRulesTop10 , method = "graph")



#BORUTA 
library(Boruta)
library(caret)
library(randomForest)

traindata <- read.csv("FINALCODE/final_files/game_data.csv")%>% clean_names()

#loook at the data
str(traindata)

#convert NAs
traindata[is.na(traindata)] <- 0

#categorical vars
convert <- c('Game_ID','Player_ID','PlayerName','Position','Team','PlayInjuryStatus','SeasonsPlayed','DepthChartPos','Coach Tier','OpposingDefense','OpposingDefenseTier','TeamFP','StadiumSurface','Weather','WindDirection','Humidity','Temperature','Stadium','WindSpeed','Home','Vistor','Year','NFL_Season','NFL Week','Day of Week','FantasyPointsTier','HomevsAway')
class(convert)

names(traindata)

training <- sqldf("select nf_lcom_points, player_id,position,team,average_rrypg,average_pypg,average_rt_ds,
                  average_pt_ds,average_pass_attempts_per_game,average_touches_targets_per_game,seasons_played,
                  depth_chart_pos,coach_tier,defensive_index,opposing_defense_tier,average_turnovers,team_fp,
                  average_snap_count,snaps,game_number,over_under,stadium,nfl_season,
                  nfl_week,market_shareof_offense,fantasy_points_tier 
                  from traindata where nfl_week < 15") 

testing <- sqldf("select  nf_lcom_points, player_id, position,team,average_rrypg,average_pypg,average_rt_ds,
                  average_pt_ds,average_pass_attempts_per_game,average_touches_targets_per_game,seasons_played,
                  depth_chart_pos,coach_tier,defensive_index,opposing_defense_tier,average_turnovers,team_fp,
                  average_snap_count,snaps,game_number,over_under,stadium,nfl_season,
                  nfl_week,market_shareof_offense,fantasy_points_tier 
                  from traindata where nfl_week in (15,16,17)") 


######################
testing[,-1] %>%
  formattable()

set.seed(3125)
library(doMC)
registerDoMC(cores = 3)
tc <- trainControl(method = "cv", number = 4, verboseIter = F, allowParallel = T)
modRF1 <- train(nf_lcom_points ~. , data= training, method = "rf", trControl = tc)
RFpredict1 <-predict(modRF1,newdata = testing[,-1])
summary(RFpredict1)
pre_RF_DF <- data.frame(RFpredict1)
head(pre_RF_DF)
colnames(pre_RF_DF) <- 'prediction'
pre_RF_DF$prediction <- as.factor(pre_RF_DF$prediction)

confusionMatrix(pre_RF_DF$prediction, testing$nf_lcom_points)

Results <- data.frame(pre_RF_DF$prediction, testing)
head(Results)
Results %>%
  formattable()

##########################

# process in parallel
library(doParallel) 
#cl <- makeCluster(detectCores(), type='PSOCK')
#registerDoParallel(cl)
#tc <- trainControl(method = "cv", number = 4, verboseIter = F, allowParallel = T)
# turn parallel processing off and run sequentially again:
#registerDoSEQ()
################### 
#model 
training <- traindata[complete.cases(training), ]
testing <- esting[complete.cases(testing), ]


set.seed(123)
boruta.train <- Boruta(nf_lcom_points~., data = traindata, doTrace = 2)
print(boruta.train)

#interpret the results
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#Blue boxplots correspond to minimal, average and maximum Z score of a 
#shadow attribute. Red, yellow and green boxplots represent Z scores of 
#rejected, tentative and confirmed attributes respectively.
#Now is the time to take decision on tentative attributes.  
#The tentative attributes will be classified as confirmed or 
#rejected by comparing the median Z score of the attributes with the 
#median Z score of the best shadow attribute. Let’s do it.

#final results
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

#
library(formattable)
boruta.df <- attStats(final.boruta)
boruta.df

print(boruta.df) %>% formattable()


