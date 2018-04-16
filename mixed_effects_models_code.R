#实验示例1
install.packages("lme4")
isntall.packages("lmerTest")
library(lme4)
library(lmerTest)
rdata = read.csv('https://raw.githubusercontent.com/pholiulei/lmer/master/rdata.csv')
rdata$subject = as.factor(rdata$subject)
rdata$sentence = as.factor(rdata$sentence)
rdata$attitude = as.factor(rdata$attitude)
m1 = lmer(formula = pitchMean ~ attitude + (1 | subject) + (1 | sentence),
          data = rdata)
anova(m1)

m2 = lmer(pitchMean ~ attitude + (1 + attitude | subject) + (1 + attitude | sentence),
          data = rdata)
anova(m2)

anova(m1, m2)


#实验示例2
rdata_L2 = read.csv('https://raw.githubusercontent.com/pholiulei/lmer/master/rdata_L2.csv')
rdata$subject = as.factor(rdata$subject)
rdata$sentence = as.factor(rdata$sentence)
rdata$group = as.factor(rdata$group)
rdata$attitude = as.factor(rdata$attitude)
m3 = lmer(pitchMean ~ group + attitude + group:attitude 
          + (1 + attitude | subject) + (1 + group + attitude + group:attitude | sentence),
          data = rdata_L2)
anova(m3)


#后续检验
install.packages("lsmeans")
library(lsmeans)
lsmeans(m3, pairwise ~ attitude, adjust = "tukey")
lsmeans(m3, pairwise ~ group | attitude, adjust = "tukey")
lsmeans(m3, pairwise ~ attitude | group, adjust = "tukey")


#效应量计算
install.packages("r2glmm")
library(r2glmm)
r2beta(m3, method = 'kr', partial = TRUE)


