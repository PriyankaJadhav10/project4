library(rpart)
library(rpart.plot)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

match_data <- read.csv("C:/Users/Administrator/Desktop/ipl/match_data.csv")
matches_data_extended <- read.csv("C:/Users/Administrator/Desktop/ipl/match_data_extended.csv")
mp<-read.csv("C:/Users/Administrator/Desktop/ipl/match_pre.csv")

set.seed(1)
train.index<-sample(c(1:dim(matches_data_extended)[1],dim(matches_data_extended)[1]*0.6))
train.df<-matches_data_extended[train.index, ]
valid.df<-matches_data_extended[-train.index, ]


fit <- rpart(Result~., data = train.df, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, train.df, type = 'class')
table_mat <- table(train.df$Result, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

pmatch <- rpart(winner~team1+team2+toss_winner+toss_decision, data = train, method = 'class')
rpart.plot(pmatch, extra = 106)
predict_unseen <-predict(pmatch, train, type = 'class')
table_mat <- table(train$winner, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


must_convert<-sapply(M,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(M[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
out<-cbind(M[,!must_convert],M2)        # complete data.frame with all variables


pre_match <- matches%>% select(team1,team2,toss_winner,winner) %>%
  distinct()
match_pre <- pre_match %>% mutate_if(is.factor, as.numeric)

match_df<-twListToDF(match_pre)
write.csv(match_pre,file = 'C:/Users/gaurav/Desktop/match_pre.csv',row.names=F)



set.seed(1)
train.index<-sample(c(1:dim(mp)[1],dim(mp)[1]*0.6))
train.dff<-mp[train.index, ]
valid.dff<-mp[-train.index, ]


fitt <- rpart(winner~., data = train.dff, method = 'class')
rpart.plot(fitt, extra = 106)
predict_un <-predict(fitt, train.dff, type = 'class')
tab_matt <- table(train.dff$winner, predict_un)
tab_matt
accuracy_t <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_t))  

cor(mp$winner, mp$toss_winner)  
scatter.smooth(x=mp$winner, y=mp$toss_winner, main="win ~ toss")

player_of_matchCorpus <- Corpus(VectorSource(matches$player_of_match))
wordcloud(player_of_matchCorpus, min.freq = 100, max.words = 200, scale=c(3,1), colors=brewer.pal(8,"Dark2"), random.order = FALSE)

