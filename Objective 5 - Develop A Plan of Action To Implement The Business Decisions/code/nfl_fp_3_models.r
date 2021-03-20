library(e1071)
library(caret)
library(ggplot2)
library(randomForest)
library(formattable)
library(tidyverse)
library(sqldf)
library(janitor)
library(rpart.plot)



############################################################################################################

####### STORE PATH ############
setwd("IST707/FINALCODE/")

####### load data ############
filename=("FINALCODE/final_files/game_data.csv")
NFL_Data <- read.csv(filename, header =TRUE, stringsAsFactors =TRUE)
summary(NFL_Data)
glimpse(NFL_Data) %>% formattable()

head(NFL_Data,10) %>% formattable()

####### QL ############
################################################################################
################################################################################
####### NFL_Data ############

NFL_Data[,-1]= sapply(NFL_Data[,-1], as.numeric)
NFL_Data$FantasyPointsTier = factor(NFL_Data$FantasyPointsTier  )
str(NFL_Data)

#add column to make quering easier
NFL_Data$NFL_Week <- NFL_Data$NFL.Week

NFL_Data_Train  <-
  sqldf("select * from NFL_Data where NFL_Week<=14")

NFL_Data_Test <-
  sqldf("select * from NFL_Data where NFL_Week >=15")

#NFL_Data_Train <- FantasyPointsTier [,-16]
FantasyPointsTier <- NFL_Data_Train[,-16]
NFL_Data_Test<- NFL_Data_Test[,-16]
NFL_Data_KNN<- NFL_Data[,-16]

table(FantasyPointsTier $FantasyPointsTier)
str(NFL_Data_Train )
str(NFL_Data_Test)




#######################
##### viz        ######
#######################
#bar plot Fantasy Tier
plot1 <- ggplot(NFL_Data,aes(x = factor(FantasyPointsTier), fill = FantasyPointsTier))+
  geom_bar()+
  xlab("Tiers")+
  ylab("Count")+
  ggtitle("Bar Plot Fantasy Tier: Main dataset")
plot1

#bar plot of Fantasy Tier Train
plot2 <- ggplot(NFL_Data_Train,aes(x = factor(FantasyPointsTier), fill = FantasyPointsTier))+
  geom_bar()+
  xlab("Tiers")+
  ylab("Count")+
  ggtitle("Bar Plot of Fantasy Tier: Train dataset")
plot2

#bar plot of Fantasy Tier Test
plot3 <- ggplot(NFL_Data_Test,aes(x = factor(FantasyPointsTier), fill = FantasyPointsTier))+
  geom_bar()+
  xlab("Tiers")+
  ylab("Count")+
  ggtitle("Bar Plot of Fantasy Tier: Test dataset")
plot3



#####MODELs######
################################################################################
################################################################################
################################################################################
################################################################################
####### Clustering ############ 
##############################

##Distance matrices

distE <- dist(NFL_Data, method= "euclidean") # Euclidean distance matrix
distE

#################
#Hierachical HAC#
#################
clustE<- hclust(distE,method="ward.D2")
plot(clustE, cex=0.5, hang=-1, main = "ALL Dendogram" )#plot dendrogram
rect.hclust(clustE, k=3)                               #show number of clusters on dendrogram
cutree(clustE, k=3)                                    # method to get the label classification out of the dendrogram

cut_avg = cutree(clustE, k=3)
clustE_CL <- mutate(NFL_Data, cluster = cut_avg)

#cluster's vs ? not good..........
table(clustE_CL$cluster)
table(NFL_Data[1])



#####
#### MISSING NFL_Data$Average_PTDs_Tile_Num (NEED TO CREATE THIS)
library(ggplot2) 
ggplot(NFL_Data, aes(NFL_Data$FantasyPointsTier, NFL_Data$Average_PTDs_Tile_Num)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

res.km <- eclust(NFL_Data[,-1], "kmeans", nstart = 3)
fviz_gap_stat(res.km$gap_stat)
fviz_silhouette(res.km)


######### MAY HAVE NA'S REMOVING THEM TO RUN THE MODELS
NFL_Data <- NFL_Data[complete.cases(NFL_Data), ]

##########
##K-Means#
##########

kmeansFIT <- kmeans(NFL_Data,centers= 3)
kmeansFIT

#combine pts_total with cluster
kmeans_pred_df <- data.frame(kmeansFIT$cluster, NFL_Data[1])
kmeans_pred_df %>% formattable()
#check total number of values in each cluster
sum(kmeans_pred_df$kmeansFIT.cluster == "1")
sum(kmeans_pred_df$kmeansFIT.cluster == "2")
sum(kmeans_pred_df$kmeansFIT.cluster == "3")

kmeansFIT$withinss#SSE

##### COULDNT RUN THIS 
fviz_cluster( kmeansFIT$cluster, NFL_Data[,-1]) 
##### END RUN THIS 


kmDF <- data.frame(NFL_Data$FantasyPointsTier, kmeansFIT$cluster)
summary(kmeans_pred_df)
table(kmDF)

################################################################################
################################################################################
################################################################################
##### decision tree ######
##########################

#added to run completed records only
NFL_Data_Test <- NFL_Data_Test[complete.cases(NFL_Data_Test), ]


str(NFL_Data_Test)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
dtree_fit <- train(FantasyPointsTier ~., data = NFL_Data_Test, method = "rpart",
                   trControl = trctrl,
                   tuneLength = 10)
dtree_fit$finalModel
summary(dtree_fit)


#######################
##### viz Model  ######
#######################
dtree_pred <- predict(dtree_fit,newdata = NFL_Data_Test)
confusionMatrix(dtree_pred,NFL_Data_Test[,1])
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

################################################################################
################################################################################
################################################################################
################################################################################
#### CURRENTLY NOT WORKING - NEED TO DETERMINE WHY
##### RANDOM FOREST caret with e1071 ######
###########################################

tc <- trainControl(method = "cv",  number = 5 , verboseIter = F)
modRF1 <- train(FantasyPointsTier  ~. , data= FantasyPointsTier , method = "rf", trControl = tc)
RFpredict1 <-predict(modRF1,newdata = NFL_Data_Test[,-1])
summary(RFpredict1)
pre_RF_DF <- data.frame(RFpredict1)
head(pre_RF_DF)
colnames(pre_RF_DF) <- 'prediction'
pre_RF_DF$prediction <- as.factor(pre_RF_DF$prediction)

confusionMatrix(pre_RF_DF$prediction, NFL_Data_Test$FantasyPointsTier)

Results <- data.frame(pre_RF_DF$prediction, NFL_Data_Test)
head(Results)
Results %>%
  formattable()

#######################
##### viz Model  ######
#######################
fit_RF <- randomForest( FantasyPointsTier~ . , data = NFL_Data_Train)
print(fit_RF)

#################
##### viz  ######
#################
#fit plot thing
varImpPlot(fit_RF, main="Fantasy_VIP")


####################################
##### histogram random Forest ######
####################################
hist(treesize(fit_RF), col = "blue", main =  'Fantasy Football RF Histogram')

##################################
##### Error plot for labels ######
##################################
plot(fit_RF, col = 1:10, main =  'Fantasy Football Error Plot')
legend("topright", colnames(fit_RF$err.rate),col=1:10,cex=0.7,fill=1:10)


################################################################################
################################################################################
####### NaiveBayes ############
###############################

digit_train_Bayes <- naiveBayes(FantasyPointsTier ~.,data=FantasyPointsTier , na.action=na.pass)
digit_predict <- predict(digit_train_Bayes, NFL_Data_Test[,-1])
print(digit_predict)

#####
#VIZ#
#####
plot(digit_predict, col = "blue", main="Naive Bayes", xlab="Fantasy Points Tier", ylab="Counts")


confusionMatrix(digit_predict, NFL_Data_Test$FantasyPointsTier)

Draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}


#Draw_confusion_matrix(Big_DICK_DAVE) 

################################################################################
################################################################################
################################################################################
####### KNN fit ############
############################
NFL_Data_KNN <- NFL_Data_KNN[complete.cases(NFL_Data_KNN), ]


trctl <- trainControl(method = "cv", number = 10)
knnFit <- train(FantasyPointsTier ~ ., data = NFL_Data_KNN, method = "knn", trControl = trctl, preProcess = c("center","scale"), tuneLength = 10)
print(knnFit)

#####
#VIZ#
#####
plot(knnFit)
Big_Dan<-confusionMatrix(predict(knnFit,newdata=NFL_Data_Test),NFL_Data_Test$FantasyPointsTier)
Big_Dan


################################################################################
################################################################################
#######  SVM  ############ NOT WORKING
##########################
trctl <- trainControl(method = "cv", number = 10)
tgrid <- expand.grid(cost = c(0.01, 0.1, 0.25, 0.5, 1, 2))

set.seed(123)
caret_svm1 <- train(FantasyPointsTier ~ ., data = FantasyPointsTier , method = "svmLinear2", 
                    trControl = trctl, tuneGrid = tgrid)


cm_caret_svm1 <- confusionMatrix(predict(caret_svm1, newdata = NFL_Data_Test), 
                                 reference = NFL_Data_Test$FantasyPointsTier)
cm_caret_svm1

#####
#VIZ#
#####
plot(varImp(caret_svm1), top = 10, main ='Variables of Importance')


################################################################################




