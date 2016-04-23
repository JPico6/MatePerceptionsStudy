########Try random forest on study 2 data

library(randomForest)
library(ggplot2)

db <- read.csv("DataOriginal.csv",header=TRUE)

dbMen <- db[which(db$Sex=="Male"),]
dbWomen <- db[which(db$Sex=="Female"),]

dbRelation <- db[which(db$Status == "In a committed relationship"),]
dbRelation <- na.omit(dbRelation$Partner_Compared_Overall)




#create interaction terms
db$Ambitious <- db$ImpOppSex_Ambitious * db$You_Relative_Ambitious
db$Body <- db$ImpOppSex_Body * db$You_Relative_Body
db$Face <- db$ImpOppSex_Face * db$You_Relative_Face
db$Kids <- db$ImpOppSex_Kids * db$You_Relative_Kids
db$Stable <- db$ImpOppSex_Stable * db$You_Relative_Stable
db$Sex <- db$ImpOppSex_Sex * db$You_Relative_Sex
db$Finances <- db$ImpOppSex_Finances * db$You_Relative_Finances
db$Generous <- db$ImpOppSex_Generous * db$You_Relative_Generous
db$Humor <- db$ImpOppSex_Humor * db$You_Relative_Humor
db$Healthy <- db$ImpOppSex_Healthy * db$You_Relative_Healthy
db$Independent <- db$ImpOppSex_Independent * db$You_Relative_Independent
db$Intelligent <- db$ImpOppSex_Intelligent * db$You_Relative_Intelligent
db$Kind <- db$ImpOppSex_Kind * db$You_Relative_Kind
db$Loyal <- db$ImpOppSex_Loyal * db$You_Relative_Loyal
db$Responsible <- db$ImpOppSex_Responsible * db$You_Relative_Responsible
db$Sociable <- db$ImpOppSex_Sociable * db$You_Relative_Sociable

#create interactions for partner evaluations 
db$Ambitious <- db$ImpYou_Ambitious * db$Partner_Ambitious
db$Body <- db$ImpYou_Body * db$Partner_Body
db$Face <- db$ImpYou_Face * db$Partner_Face
db$Kids <- db$ImpYou_Kids * db$Partner_Kids
db$Stable <- db$ImpYou_Stable * db$Partner_Stable
db$Sex <- db$ImpYou_Sex * db$Partner_Sex
db$Finances <- db$ImpYou_Finances * db$Partner_Finances
db$Generous <- db$ImpYou_Generous * db$Partner_Generous
db$Humor <- db$ImpYou_Humor * db$Partner_Humor
db$Healthy <- db$ImpYou_Healthy * db$Partner_Healthy
db$Independent <- db$ImpYou_Independent * db$Partner_Independent
db$Intelligent <- db$ImpYou_Intelligent * db$Partner_Intelligent
db$Kind <- db$ImpYou_Kind * db$Partner_Kind
db$Loyal <- db$ImpYou_Loyal * db$Partner_Loyal
db$Responsible <- db$ImpYou_Responsible * db$Partner_Responsible
db$Sociable <- db$ImpYou_Sociable * db$Partner_Sociable


#########For self
dbT <- db[,c(54,73:87)]
dbT <- na.omit(dbT)

set.seed(8345)
rf <- randomForest(You_Relative_Overall~., data=dbT, ntree=500, mtry = 2, importance=TRUE)
rf

varImpPlot(rf, main="Variable Importance for Self-Assessing Total Sample")
varUsed(rf)
names(dbT)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), 
                                Importance=imp[,1])

png(filename="study2Self.png")

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), 
                                   y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(a) Predictor Importance by Self Traits") +
  theme(plot.title=element_text(size=18))
p

dev.off()

#########For partners
dbT <- db[,c(72,89:103)]
dbT <- na.omit(dbT)

set.seed(8345)
rf <- randomForest(Partner_Compared_Overall~., data=dbT, ntree=500, mtry = 4, importance=TRUE)
rf

varImpPlot(rf, main="Variable Importance for Partner-Assessing Total Sample")
varUsed(rf)
names(dbT)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), 
                                Importance=imp[,1])

png(filename="study2Partners.png")

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), 
                                   y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(b) Predictor Importance by Partner Traits") +
  theme(plot.title=element_text(size=18))
p

dev.off()


#interaction terms of self-perceptions x what you find important
db$Ambitious <- db$ImpYou_Ambitious * db$You_Relative_Ambitious
db$Body <- db$ImpYou_Body * db$You_Relative_Body
db$Face <- db$ImpYou_Face * db$You_Relative_Face
db$Kids <- db$ImpYou_Kids * db$You_Relative_Kids
db$Stable <- db$ImpYou_Stable * db$You_Relative_Stable
db$Sex1 <- db$ImpYou_Sex * db$You_Relative_Sex
db$Finances <- db$ImpYou_Finances * db$You_Relative_Finances
db$Generous <- db$ImpYou_Generous * db$You_Relative_Generous
db$Humor <- db$ImpYou_Humor * db$You_Relative_Humor
db$Healthy <- db$ImpYou_Healthy * db$You_Relative_Healthy
db$Independent <- db$ImpYou_Independent * db$You_Relative_Independent
db$Intelligent <- db$ImpYou_Intelligent * db$You_Relative_Intelligent
db$Kind <- db$ImpYou_Kind * db$You_Relative_Kind
db$Loyal <- db$ImpYou_Loyal * db$You_Relative_Loyal
db$Responsible <- db$ImpYou_Responsible * db$You_Relative_Responsible
db$Sociable <- db$ImpYou_Sociable * db$You_Relative_Sociable

dbT <- db[,c(54,73:88)]
dbT <- na.omit(dbT)


set.seed(8345)
rf <- randomForest(You_Relative_Overall~., data=dbT, ntree=500, mtry = 2, importance=TRUE)
rf

varImpPlot(rf, main="Variable Importance for Self-Assessing Total Sample")
varUsed(rf)
names(dbT)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), 
                                Importance=imp[,1])

png(filename="study2Self.png")

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), 
                                   y=Importance)) +
  geom_bar(stat="identity", fill="#42A5CC") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("(a) Predictor Importance by Self Traits") +
  theme(plot.title=element_text(size=18))
p

dev.off()


