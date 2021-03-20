library(tm)
library(stringr)
library(wordcloud)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
library(tidyverse)
library(tidytext)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(nnet)
library(RColorBrewer)

set.seed(1234)
theme_set(theme_minimal())




setwd("/Users/petermathews/Downloads/SU Masters /Classes/IST707/week5/")

##, load in the documents (the corpus)
corpus <- VCorpus(DirSource("Corpus"))
ndocs<-length(corpus)


minTermFreq <- ndocs * .3
minTermFreq
maxTermFreq <- ndocs * .8
maxTermFreq

# view corpous



# preprocess/clean the training corpus
corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lowercase
corpus <- tm_map(corpus, removeNumbers) # remove digits
corpus <- tm_map(corpus, removePunctuation) # remove punctuation
corpus <- tm_map(corpus, stripWhitespace) # strip extra whitespace
corpus <- tm_map(corpus, removeWords, stopwords('english')) # remove stopwords

tdm <- DocumentTermMatrix(corpus)

stopWords <- c("will","one","two","may","less", "well","might","without",
               "small","single", "several","however","must","number","
               part","james","madison","jame")

# create term document matrix (tdm)
tdm <- DocumentTermMatrix(corpus,
                          control = list(
                             wordLengths=c(4, 12), ## get rid of words of len 3 or smaller or longer than 15
                            removePunctuation = TRUE,
                            removeNumbers = TRUE,
                            tolower=TRUE,
                            stemming = TRUE,
                            stopwords=stopWords,
                            remove_separators = TRUE,
                            bounds = list(global = c(minTermFreq, maxTermFreq))
                          ))

(tdm <- removeSparseTerms(tdm, sparse = 0.99))

# inspecting the tdm
dim(tdm) # 993 documents, 9243 terms

colnames(tdm)[200:210]

as.matrix(tdm)[1:50,1:65] # inspect a portion of the tdm


########################## BEGIN METHOD 1 ############################# 
tdm_df <- tidy(as.matrix(tdm))





tdm_df <- tdm_df %>% separate(.rownames, c("author"), extra='drop') 
tdm_df$.rownames <- NULL
table(tdm_df$author)

tdm_df <- tdm_df %>% group_by(author) %>% sample_n(size = 11)

glimpse(tdm_df)

weightedtdm <- weightTfIdf(tdm)
weightedtdm_df <- tidy(as.matrix(weightedtdm))
weightedtdm_df <- weightedtdm_df %>% separate(.rownames, c("author"), extra='drop') 
weightedtdm_df <- weightedtdm_df %>% group_by(author) %>% sample_n(size = 11)
weightedtdm_df$.rownames <- NULL



# split  into train and test sets
tdmTrain <- tdm_df[which(tdm_df$author != 'dispt'),]
tdmTrain$author <- as.factor(as.character(tdmTrain$author ))

weightedTDMtrain <- weightedtdm_df[which(weightedtdm_df$author != 'dispt'),]
weightedTDMtrain$author <- as.factor(as.character(weightedTDMtrain$author ))
#levels(droplevels(weightedTDMtrain$author ))



table(tdmTrain$author)




tdmTest <- tdm_df[which(tdm_df$author == 'dispt'),]
#tdmTest$author <- as.factor(as.character(tdmTest$author ))
table(tdmTest$author)

weightedTDMtest <- weightedtdm_df[which(weightedtdm_df$author == 'dispt'),]
#weightedTDMtest$author <- as.factor(as.character(weightedTDMtest$author ))
table(weightedTDMtest$author)


# remove objects that are no longer needed to conserve memory
remove(tdm,weightedtdm)

##################################################################
# TRAIN/PREDICT USING WEIGHTED MODEL
##################################################################

glimpse(data.frame(weightedTDMtrain)) 
set.seed(1245)
fit = rpart(
  author ~ .,
  data =  data.frame(weightedTDMtrain),
  method = "class",
  parms = list(split = 'information'), 
  maxdepth = 1, 
  minsplit = 2, 
  minbucket = 1
  )

summary(fit)

fit$variable.importance
## visualize default tree
rpart.plot(fit,roundint=FALSE,nn=TRUE)

prp(fit,roundint=FALSE)

pred <- predict(fit, newdata=weightedTDMtest, type = 'class')
predict(fit, newdata=weightedTDMtest, type = 'prob')
plot(pred)
summary(pred)



#PLOTS
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
########################## END METHOD 1 ############################# 
# TRAIN/PREDICT
##################################################################

glimpse(data.frame(tdmTrain)) 
set.seed(1245)
fitTdm = rpart(
  author ~ .,
  data =  data.frame(tdmTrain),
  method = "class"
)

summary(fitTdm)

fitTdm$variable.importance
## visualize default tree
rpart.plot(fitTdm,roundint=FALSE,nn=TRUE)
prp(fitTdm,roundint=FALSE)

pred2 <- predict(fitTdm, newdata=tdmTest, type = 'class')
predict(fit, newdata=tdmTest, type = 'prob')
plot(pred2)
summary(pred2)



#PLOTS
rpart.plot(fitTdm,roundint=FALSE,nn=TRUE)
printcp(fitTdm) # display the results
plotcp(fitTdm) # visualize cross-validation results
summary(fitTdm) # detailed summary of splits


########################

########################### Information Gain with Entropy ----------------------------
library(CORElearn)

# attrEval evaluates the quality of the attributes 
## (dependent variables) with the selected heuristic method
##install.packages("CORElearn")
##https://www.researchgate.net/publication/265202420_Package_%27CORElearn%27#pf5
#https://www.mitre.org/sites/default/files/pdf/harris_biases.pdf
#https://www.rdocumentation.org/packages/CORElearn/versions/1.53.1/topics/attrEval
Method.CORElearn <- CORElearn::attrEval(weightedTDMtrain$author ~ ., data=weightedTDMtrain,  estimator = "InfGain")
(Method.CORElearn)
Method.CORElearn2 <- CORElearn::attrEval(weightedTDMtrain$author ~ ., data=weightedTDMtrain,  estimator = "Gini")
(Method.CORElearn2)
Method.CORElearn3 <- CORElearn::attrEval(weightedTDMtrain$author ~ ., data=weightedTDMtrain,  estimator = "GainRatio")
(Method.CORElearn3)

###########################

# set resampling scheme: 10-fold cross-validation, 3 times
ctrl <- trainControl(method="repeatedcv", number = 10, repeats = 3,classProbs=TRUE)

# fit a decision tree using the weighted (td-idf) term document matrix
# tuning parameter: cp
glimpse(weightedTDMtrain)
# predict on test data
set.seed(1080)
tree.tfidf  <- train(author ~. , data = data.frame(weightedTDMtrain), method = "rpart",trControl = ctrl )
tree.tfidf
summary(tree.tfidf)
plot(tree.tfidf)


# What is the standard deviation?
cat(paste("\nCross validation standard deviation:",  
          sd(tree.tfidf$resample$Accuracy), "\n", sep = " "))

# Pull out the the trained model using the best parameters on
# all the data! Mighty!
rpart.best <- tree.tfidf$finalModel


#Plots
prp(rpart.best, type = 0, extra = 1, under = TRUE)
printcp( rpart.best)

#predict
predict(object=tree.tfidf, newdata=data.frame(weightedTDMtest), type = 'prob',xlev=tree.tfidf$xlevels[["author"]])

### by class
predW <- predict(tree.tfidf, data.frame(weightedTDMtest), type = 'raw')
summary(predW)


# fit a decision tree using the unweighted TDM
# tuning parameter: cp
set.seed(1080)
tree <- train(author ~ . , data = data.frame(tdmTrain), method = "rpart", trControl = ctrl) 

# predict on test data
tree.predict <- predict(tree,newdata = data.frame(tdmTest))

#
predict(tree, data.frame(tdmTest), type = 'prob')

### by class
pred2 <- predict(tree, data.frame(tdmTest), type = 'raw')
summary(pred2)

rpart2.best <- tree$finalModel
prp(rpart2.best, type = 0, extra = 1, under = TRUE)
printcp( rpart2.best)

# --------------- Weighted (tfi-idf) TDM ---------------------#
# output the decision tree fit using the weighted (tf-idf) TDM
tree.tfidf

plot(tree.tfidf) # accuracy vs. complexity parameter values

# output the decision tree fit using the unweighted TDM
tree

plot(tree)  # accuracy vs. complexity parameter values



# print info about parameters, etc. used in the model with highest accuracy
tree.tfidf$results # error rate and values of tuning parameter

tree.tfidf$bestTune # final tuning parameter

tree.tfidf$metric # metric used to select optimal model

tree.tfidf$times # a list of execution times




















