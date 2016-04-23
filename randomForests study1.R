#Random forest test on Break up data
library(randomForest)
library(tree)
library(ggplot2)
library(dplyr)    #for tables
library(ggvis)   #nice graphs, also has interactive capacity

###########################By Raters
df <- read.csv("BS1.csv", header=TRUE)
dfMen <- df[ which(df$Sex == 'M'),]
dfWomen <- df[ which(df$Sex == 'F'),]

dfMen <- dfMen[,(-c(1,4,5,6,7,8))]
dfWomen <- dfWomen[,(-c(1,4,5,6,7,8))]

colnames(dfMen)[16] <- "Responsible"
colnames(dfWomen)[16] <- "Responsible"
colnames(dfWomen)[12] <- "Sociable"
colnames(dfMen)[12] <- "Sociable"


m1 <- df %>%
      group_by(Sex, Overall)
  
table1 <- df %>%
          group_by(Sex) %>%
          summarise(Overall = mean(round(Overall),digits=2), Face=round(mean(Face),digits=2))


df %>% 
  ggvis(~Face, ~Overall) %>% 
  layer_points(fill = ~factor(Sex)) 


set.seed(32)

#For complete sample (men and women) by raters
rf <- randomForest(Overall~., data=df, ntree=500, mtry = 5, importance=TRUE)
rf

varImpPlot(rf, main="Variable Importance for men and Women")
varUsed(rf)
names(df)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), 
                                Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), 
                                   y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Predictor Importance-Men and Women\n") +
  theme(plot.title=element_text(size=18))
p


#for men only judged by raters
dfMen1 <- dfMen[,c(5:8,10:19)]

set.seed(2001)
rfMen <- randomForest(Overall~., data=dfMen1, ntree=500, mtry = 3, importance=TRUE)
rfMen

varImpPlot(rfMen, main="Variable Importance for men")
varUsed(rfMen)
names(dfMen1)

impM <- importance(rfMen, type=1)
featureImportanceM <- data.frame(Feature=row.names(impM), 
                                Importance=impM[,1])


png(filename="men_by_raters.png")

pM <- ggplot(featureImportanceM, aes(x=reorder(Feature, Importance), 
                                     y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(a) Predictors of Men's Mate Value by Raters") +
  theme(plot.title=element_text(size=16.5))
pM

dev.off()



#For women only judged by raters
dfWomen1 <- dfWomen[,c(5:8,10:19)]

set.seed(821)
rfWomen <- randomForest(Overall~., data=dfWomen1, importance=TRUE, ntree=500, mtry=5)
rfWomen

varImpPlot(rfWomen, main="Variable Importance for Women")
varUsed(rfWomen)
names(dfWomen1)

impW <- importance(rfWomen, type=1)
featureImportanceW <- data.frame(Feature=row.names(impW), 
                                Importance=impW[,1])

png(filename="women_by_raters.png")

pW <- ggplot(featureImportanceW, aes(x=reorder(Feature, Importance), 
                                   y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(b) Predictors of Women's Mate Value by Raters") +
  theme(plot.title=element_text(size=16.5)) 
  
pW

dev.off()


#####################By Self
df <- read.csv("BreakupStudy1.csv", header=TRUE)
dfMen <- df[ which(df$I_1 == '1'),]
dfWomen <- df[ which(df$I_1 == '0'),]

#for men
dfM <- dfMen[,c(69:86)]
colnames(dfM) <- c("Ambitious", "Face","Body","Kids","Sex","Faithful",
                   "Finances","Generous","Humor","Healthy", "Independent","Intelligent","Kind",
                   "Loyal","Responsible","Sociable","Stable","Overall")

set.seed(2001)
rfMen <- randomForest(Overall~., data=dfM, ntree=500, importance=TRUE,mtry=4, na.action=na.omit)
rfMen

varImpPlot(rfMen, main="Variable Importance for men")
varUsed(rfMen)
names(dfMen1)

impM <- importance(rfMen, type=1)
featureImportanceM <- data.frame(Feature=row.names(impM), 
                                 Importance=impM[,1])


png(filename="men_by_self.png")

pM <- ggplot(featureImportanceM, aes(x=reorder(Feature, Importance), 
                                     y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(c) Predictors of Men's Mate Value by Self") +
  theme(plot.title=element_text(size=16.5))
pM

dev.off()





#for women
dfW <- dfWomen[,c(69:86)]
colnames(dfW) <- c("Ambitious", "Face","Body","Kids","Sex","Faithful",
                   "Finances","Generous","Humor","Healthy", "Independent","Intelligent","Kind",
                   "Loyal","Responsible","Sociable","Stable","Overall")

set.seed(413)
rfWomen <- randomForest(Overall~., data=dfW, ntree=500, importance=TRUE,mtry=3, na.action=na.omit)
rfWomen

varImpPlot(rfWomen, main="Variable Importance for Women")
varUsed(rfWomen)
names(dfW)

impW <- importance(rfWomen, type=1)
featureImportanceW <- data.frame(Feature=row.names(impW), 
                                 Importance=impW[,1])


png(filename="women_by_self.png")

pW <- ggplot(featureImportanceW, aes(x=reorder(Feature, Importance), 
                                     y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(d) Predictors of Women's Mate Value by Self") +
  theme(plot.title=element_text(size=16.5))
pW

dev.off()




########################By Partner
df <- read.csv("BreakupStudy1.csv", header=TRUE)
dfMen <- df[ which(df$I_1 == '1'),]
dfWomen <- df[ which(df$I_1 == '0'),]

#for men
dfM <- dfMen[,c(89:106)]
colnames(dfM) <- c("Ambitious", "Face","Body","Kids","Sex","Faithful",
                   "Finances","Generous","Humor","Healthy", "Independent","Intelligent","Kind",
                   "Loyal","Responsible","Sociable","Stable","Overall")

set.seed(2001)
rfMen <- randomForest(Overall~., data=dfM, ntree=500, importance=TRUE,mtry=2, na.action=na.omit)
rfMen

varImpPlot(rfMen, main="Variable Importance for men")
varUsed(rfMen)
names(dfMen1)

impM <- importance(rfMen, type=1)
featureImportanceM <- data.frame(Feature=row.names(impM), 
                                 Importance=impM[,1])


png(filename="men_by_partner.png")

pM <- ggplot(featureImportanceM, aes(x=reorder(Feature, Importance), 
                                     y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(e) Predictors of Men's Mate Value by Partner") +
  theme(plot.title=element_text(size=16.5))
pM

dev.off()






#For women
dfW <- dfWomen[,c(89:106)]
colnames(dfW) <- c("Ambitious", "Face","Body","Kids","Sex","Faithful",
                   "Finances","Generous","Humor","Healthy", "Independent","Intelligent","Kind",
                   "Loyal","Responsible","Sociable","Stable","Overall")

set.seed(413)
rfWomen <- randomForest(Overall~., data=dfW, ntree=500, importance=TRUE,mtry=3, na.action=na.omit)
rfWomen

#dfW1 <- dfW[complete.cases(dfW),]
#tuneRF(dfW1[,1:17], dfW1$Overall, mtryStart=2, ntreeTry=500, stepFactor=2, improve=0.05,
#       trace=TRUE, plot=TRUE, doBest=FALSE)


varImpPlot(rfWomen, main="Variable Importance for Women")
varUsed(rfWomen)
names(dfW)

impW <- importance(rfWomen, type=1)
featureImportanceW <- data.frame(Feature=row.names(impW), 
                                 Importance=impW[,1])


png(filename="women_by_partner.png")

pW <- ggplot(featureImportanceW, aes(x=reorder(Feature, Importance), 
                                     y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(f) Predictors of Women's Mate Value by Partner") +
  theme(plot.title=element_text(size=16.5))
pW

dev.off()

mean(dfW$Responsible, na.rm = TRUE)


library(tables)
dfW <- na.omit(dfW)
dfW2 <- melt(dfW, id.vars="Overall")
tabular( (variable + 1) ~ (n=1) + Format(digits=3)*(value)*(mean + sd), data=dfW2 )





####################for figure 1

Perceiver <- c("Self","Partner","Self","Partner")
Sex <- c("Male","Male","Female","Female")
Mean_Rating <- c(5.84,5.17,5.26,5.95)
e <- cbind.data.frame(Perceiver,Sex,Mean_Rating)

png(filename="stability_interaction1.png")

ggplot(e, aes(x=Perceiver, y=Mean_Rating)) + 
  geom_line(aes(colour=Sex, group=Sex), size=2) + 
  labs(y = "Mean Ratings", 
  title = "Means for Emotional Stability by Sex and Perceiver") +
  theme(plot.title=element_text(size=16.5)) +
  theme(axis.text=element_text(size=12)) +
  theme(axis.title=element_text(size=15)) +
  theme(legend.title = element_text(size=16)) +
  theme(legend.text = element_text(size = 14))

dev.off()


Perceiver <- c("Self","Partner","Self","Partner")
Sex <- c("Male","Male","Female","Female")
Mean_Rating <- c(6.5,6.82,6.55,6.34)
l <- cbind.data.frame(Perceiver,Sex,Mean_Rating)

png(filename="loyal_interaction1.png")

ggplot(l, aes(x=Perceiver, y=Mean_Rating)) + 
  geom_line(aes(colour=Sex, group=Sex), size=2) + 
  labs(y = "Mean Ratings", 
       title = "Means for Loyal by Sex and Perceiver") +
  theme(plot.title=element_text(size=16.5)) +
  theme(axis.text=element_text(size=12)) +
  theme(axis.title=element_text(size=15)) +
  theme(legend.title = element_text(size=16)) +
  theme(legend.text = element_text(size = 14))

dev.off()

Perceiver <- c("Self","Partner","Self","Partner")
Sex <- c("Male","Male","Female","Female")
Mean_Rating <- c(6.54,6.78,6.68,6.2)
f <- cbind.data.frame(Perceiver,Sex,Mean_Rating)

png(filename="faithful_interaction.png")

ggplot(f, aes(x=Perceiver, y=Mean_Rating)) + 
  geom_line(aes(colour=Sex, group=Sex), size=2) + 
  labs(y = "Mean Ratings", 
       title = "Means for Faithful by Sex and Perceiver") +
  theme(plot.title=element_text(size=16.5)) +
  theme(axis.text=element_text(size=12)) +
  theme(axis.title=element_text(size=15)) +
  theme(legend.title = element_text(size=16)) +
  theme(legend.text = element_text(size = 14))

dev.off()



