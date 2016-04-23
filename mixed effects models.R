

###############Tests of relationship length on self and partner enhancement
library(lme4)
library(lmerTest)
library(reshape2)
library(ggplot2)

#create difference score
df <- read.csv("Reorganized1.csv", header=TRUE)

df$discrF <- df$Foverall.2 - df$Foverall.3
df$discrM <- df$Moverall.2 - df$Moverall.3
df1 <- df[,(c(1,2,9,10))]

df2 <- melt(df1, id.vars=c("couple","reLength"))

as.factor(df2$couple)
df2$couple <- factor(df2$couple)

ggplot(df2, aes(x=reLength, y=value)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

ggplot(df2, aes(x=reLength, y=value)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region


ML1 <- lmer(value~reLength + (1 | couple),data=df2)
summary(ML1)

ML2 <- lmer(value~reLength + (0 + reLength | couple) + (1 | couple),data=df2) 

MLhyp <- lmer(value~reLength + (1 | couple),data=df2, REML=FALSE)
MLhyp2 <- lmer(value~reLength + (0 + reLength | couple) + (1 | couple),data=df2, REML=FALSE) 

MLnull <- lmer(value~ + (1 | couple), data=df2, REML=FALSE)
anova(MLnull,MLhyp)   #gives us the chi 2 comparison of null and hyp models
anova(MLnull,MLhyp2)   #MLhyp works better






#calculate r2
cor(ML1$value, fitted(lmer(value~reLength + (1 | couple),data=df2)))^2

cor(l$RT, fitted(lmer(RT ~ cNativeEnglish * cFrequency + Trial +
                        + (1 | Word) + (1 | Subject), data = l)))^2

anova(ML1,ML2)
#the first one is the best
#help("pvalues")     #for help in getting p values
#the model was fitted by restricted maximum likelihood (REML)


plot(ML1,type=c("p","smooth")) ## fitted vs residual
plot(ML1,sqrt(abs(resid(.)))~fitted(.), ## scale-location
     type=c("p","smooth"))
qqmath(ML1,id=0.05)

iqrvec <- sapply(simulate(ML1,1000),IQR)
obsval <- IQR(df2$value)
post.pred.p <- mean(obsval>=c(obsval,iqrvec))



lsmeans(ML1, test.effs="reLength")
plot(ML1)


plot(fitted(ML1),residuals(ML1))
hist(residuals(ML1))
coef(ML1)

