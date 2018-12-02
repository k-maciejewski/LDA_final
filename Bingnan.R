library(foreign)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(nlme)
library(RLRsim)
library(CompRandFld)
library(MASS)
library(gee)
library(geepack)
library(VIM)

#import the data
my.data <- read.csv("F:/courses/longitudinal/FINAL/frmgham2.csv")
head(my.data)
summary(my.data$AGE)


#EDA of relationship between age and smoking status
boxplot.1 <- ggplot(data = my.data, aes(x = CURSMOKE, y = AGE, group=CURSMOKE))+
  geom_boxplot()+
  ggtitle('Boxplot of smoking status against age')
boxplot.1
boxplot.2 <- ggplot(data = my.data, aes(x = CURSMOKE, y = AGE, group=CURSMOKE))+
  geom_boxplot()+
  facet_wrap(~ SEX,ncol=2)+
  ggtitle('Boxplot of smoking status against age grouped by sex')
boxplot.2

#EDA of relationship between number of cigarettes smoked per day and age
plot.1 <- ggplot(data=my.data, aes(x = AGE, y = CIGPDAY))+
  geom_point()+
  geom_smooth(method='loess')+
  ggtitle('association between age and cig per day')
plot.1
plot.2 <- ggplot(data=my.data, aes(x = AGE, y = CIGPDAY))+
  geom_point()+
  geom_smooth(method='loess')+
  facet_wrap(~SEX, ncol=2)+
  ggtitle('association between age and cig per dat grouped by sex')
plot.2

#MISSING DATA ANALYSIS
aggr(my.data, prop=T, numbers=T)
md.pattern(my.data)
# HDLC and LDLC have a lot of missing data, and these variables should be eliminated from the model.

#fit a saturated GLS model
my.data.complete <- my.data %>%
  dplyr::select(-c(HDLC,LDLC)) %>%
  na.omit()
model.saturated <- gee(CURSMOKE ~ as.factor(SEX) + as.factor(PERIOD) + AGE +AGE*as.factor(SEX) + 
                            + SYSBP + DIABP + SYSBP*DIABP + BPMEDS + as.factor(educ)
                            + TOTCHOL + BMI + GLUCOSE + DIABETES + HEARTRTE + PREVAP
                            + PREVCHD + PREVMI + PREVSTRK + PREVHYP,
                              data = my.data.complete,
                              id = RANDID,
                              family = binomial,
                              corstr = ('unstructured'))
summary(model.saturated)
#P-values of variables in satureated model
round(2*pnorm(abs(coef(summary(model.saturated))[,5]), lower.tail=F),5)
##Findings:
#SEX, PERIOD, AGE, SYSBP & DIABP, educ, TOTCHOL, BMI, HEARTRT are significant.
