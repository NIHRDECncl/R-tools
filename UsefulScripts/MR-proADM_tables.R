#for tables

rm(list = ls())
#path to the working directory
wd <- "H:/DEC projects/MRproADM/Rcalculations"
#wd <- "C:/Users/Sara/Desktop/Rcalculations"
#wd<- "C:/Users/Sara Graziadio/Desktop/Sara/Sara/Rcalculations"
setwd(wd)

#load in packages required
source("loadpackages.R")
loadpackages()

mydata = read.csv("Raw_data/MRproADMdataReadyForAnalysis.csv")

mydata$admissionCRPraw[is.na(mydata$admissionCRPraw)] <- median(mydata$admissionCRPraw,na.rm=T)
mydata$admissionWBC[is.na(mydata$admissionWBC)] <- median(mydata$admissionWBC,na.rm=T)

mydata_new= mydata[(!is.na(mydata$NEWS1)),]#patients with no follow up NEWS (and who didn't deteriorate)
mydata=mydata_new

m =mean(mydata$TimeToMRproADMRaw,na.rm=T)
s= sd(mydata$TimeToMRproADMRaw,na.rm=T)

mydata_new1= mydata[mydata$TimeToMRproADMRaw>(m-3*s), ]
mydata_new= mydata_new1[mydata_new1$TimeToMRproADMRaw<(m+3*s), ]
mydata=mydata_new


###
mydata$Comorb0[mydata$Comorb0=="Unknown"] <- NA
mydata$Comorb0<- droplevels(mydata$Comorb0)
summary(mydata$Comorb0)

mydata$Comorb1[mydata$Comorb1=="Unknown"] <- "Absent"
mydata$Comorb1 <- droplevels(mydata$Comorb1)
summary(mydata$Comorb1)
######

mydata$age2=mydata$age^2


#table 1 (descriptive)

install.packages("doBy")
library(psych)

outcome= mydata$DET72
outcome= mydata$NonNEWSdet

describeBy(mydata$age, outcome)
describeBy(mydata$MRproADMRaw, outcome)
describeBy(mydata$admissionCRPraw, outcome)
describeBy(mydata$admissionWBC, outcome)
describeBy(mydata$LengthOfStay, outcome)
describeBy(mydata$TimeToTransFromAS, outcome)
describeBy(mydata$DetTime, outcome, na.rm=TRUE)

 
 with(mydata, tapply(admissionCRPraw, outcome, quantile, probs= c(0.25,0.5,0.75)))
 with(mydata, tapply(MRproADMRaw, outcome, quantile, probs= c(0.25,0.5,0.75)))
 with(mydata, tapply(LengthOfStay, outcome, quantile, probs= c(0.25,0.5,0.75)))
 with(mydata, tapply(admissionWBC, outcome, quantile, probs= c(0.25,0.5,0.75)))
 with(mydata, tapply(DetTime, outcome, quantile, probs= c(0.25,0.5,0.75),  na.rm=TRUE))
 with(mydata, tapply(TimeToTransFromAS, outcome, quantile, probs= c(0.25,0.5,0.75)))
 
table(mydata$gender, outcome)
prop.table(table(mydata$gender, outcome),2)


table(mydata$admissionNEWS, outcome)
prop.table(table(mydata$admissionNEWS, outcome),2)

table(mydata$Comorb0, outcome)
prop.table(table(mydata$Comorb0, outcome),2)

table(mydata$Comorb1, outcome)
prop.table(table(mydata$Comorb1, outcome),2)

table(mydata$ASbedType, outcome)
prop.table(table(mydata$ASbedType, outcome),2)


####Table univariate (table 2)

outcome= mydata$DET72
outcome= mydata$NonNEWSdet

mAge=glm(outcome~(MRproADMRaw), family = binomial, data=mydata)
MR=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mAge=glm(outcome~(admissionCRPraw), family = binomial, data=mydata)
CRP=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mAge=glm(outcome~(admissionWBC), family = binomial, data=mydata)
WBC=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mAge=glm(outcome~(gender), family = binomial, data=mydata)
gen=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mAge=glm(outcome~(age), family = binomial, data=mydata)
ageT=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mydata$age2=mydata$age^2
mAge=glm(outcome~(age+age2), family=binomial(link = logit), data=mydata)
age2T=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI


mAge=glm(outcome~(Comorb0), family = binomial, data=mydata)
co0=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mAge=glm(outcome~(Comorb1), family = binomial, data=mydata)
co1=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

uni=rbind(MR, CRP, WBC, gen, ageT, age2T, co0, co1)
write.csv(uni, file = "univariate.csv")

####Lenght of stay (univariate)
outcome= mydata$LengthOfStay

mAge=lm(log10(outcome)~log10(MRproADMRaw), data=mydata)
MR=cbind(coef(mAge),confint(mAge, level=0.95), coef(summary(mAge))[,4])#odds ratio and CI

mAge=lm(log10(outcome)~log10(admissionCRPraw),  data=mydata)
CRP=cbind(coef(mAge),confint(mAge), coef(summary(mAge))[,4])#odds ratio and CI

mAge=lm(log10(outcome)~log10(admissionWBC), data=mydata)
WBC=cbind(coef(mAge),confint(mAge),  coef(summary(mAge))[,4])#odds ratio and CI

mAge=lm(log10(outcome)~factor(gender),  data=mydata)
gen=cbind(coef(mAge),confint(mAge), coef(summary(mAge))[,4])#odds ratio and CI

mAge=lm(log10(outcome)~(age), data=mydata)
ageT=cbind(coef(mAge),confint(mAge),  coef(summary(mAge))[,4])#odds ratio and CI

mydata$age2=mydata$age^2
mAge=lm(log10(outcome)~(age+age2), data=mydata)
age2T=cbind(coef(mAge),confint(mAge, level=0.95), coef(summary(mAge))[,4])#odds ratio and CI



mAge=lm(log10(outcome)~factor(Comorb0),  data=mydata)
co0=cbind(coef(mAge),confint(mAge), coef(summary(mAge))[,4])#odds ratio and CI

mAge=lm(log10(outcome)~factor(Comorb1),  data=mydata)
co1=cbind(coef(mAge),confint(mAge),  coef(summary(mAge))[,4])#odds ratio and CI

uni=rbind(MR, CRP, WBC, gen, ageT, age2T, co0, co1)
write.csv(uni, file = "univariate.csv")

### table 3 Logistic regressions

outcome= mydata$DET72
outcome= mydata$NonNEWSdet

mAge=glm(outcome~factor(admissionNEWS), family = binomial, data=mydata)
o1a=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mAge=glm(outcome~factor(admissionNEWS)+MRproADMRaw, family = binomial, data=mydata)
o1b=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mAge=glm(outcome~factor(admissionNEWS)+MRproADMRaw+Comorb0+MRproADMRaw*Comorb0, family = binomial, data=mydata)
o1c=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

mAge=glm(outcome~factor(admissionNEWS)+MRproADMRaw+Comorb1+age+age2, family = binomial, data=mydata)
o1c=cbind(coef(mAge),confint(mAge), exp(cbind(OR = coef(mAge),confint(mAge))), coef(summary(mAge))[,4])#odds ratio and CI

# mAge0=glm(outcome~factor(admissionNEWS)+MRproADMRaw, family = binomial, data=mydata)
# mAge1=glm(outcome~MRproADMRaw, family = binomial, data=mydata)
# 
# anova(mAge, mAge1, test = "Chisq")

o1=rbind(o1a, o1b, o1c)
write.csv(o1, file = "logistic_O2.csv")

###linear regression lenght of stay
outcome= mydata$LengthOfStay
mAge=lm(log10(outcome)~factor(admissionNEWS),  data=mydata)
o3a=cbind(coef(mAge),confint(mAge), coef(summary(mAge))[,4], anova(mAge)$"Pr(>F)"[1])#odds ratio and CI

mAge=lm(log10(outcome)~factor(admissionNEWS)+log10(MRproADMRaw),  data=mydata)
o3b=cbind(coef(mAge),confint(mAge), coef(summary(mAge))[,4], anova(mAge)$"Pr(>F)"[1])#odds ratio and CI


mAge=lm(log10(outcome)~factor(admissionNEWS)+log10(MRproADMRaw)+age,  data=mydata)
o3c=cbind(coef(mAge),confint(mAge), coef(summary(mAge))[,4], anova(mAge)$"Pr(>F)"[1])#odds ratio and CI


o3=rbind(o3a, o3b, o3c)
write.csv(o3, file = "lm_O3.csv")

#table 4 model comparison

outcome= mydata$DET72
clm=which( colnames( mydata)=="DET72" )

###NEWS
mAge0=glm(outcome~factor(admissionNEWS), family = binomial, data=mydata, na.action=na.exclude)
aic=extractAIC(mAge0)
mydata$prob=predict(mAge0,type=c("response"))
predRisk1 <- predRisk(mAge0)

library(pROC)
g <- roc(outcome ~ prob, data = mydata, ci=TRUE)
ci=ci.auc(g)

library ("PredictABEL")
predRisk0 <- predRisk(mAge0)

o1a=cbind(deviance(mAge0),aic[2],ci[2], ci[1], ci[3], 0, 0, 0, 0, 0, 0, 0, 0, 0)#odds ratio and CI

##NEWS+MR
mAge1=glm(outcome~factor(admissionNEWS)+MRproADMRaw, family = binomial, data=mydata, na.action=na.exclude)
aic=extractAIC(mAge1)
mydata$prob=predict(mAge1,type=c("response"))
library(pROC)
g <- roc(outcome ~ prob, data = mydata, ci=TRUE, of="thresholds", thresholds="best")
ci=ci.auc(g)
# rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp", "npv",
#           "ppv")
# all=ci.coords(g, x=0.3129972, input = "threshold", ret=rets)
# LRp=all[3,2]/(1-all[2,2])     #positive likelihood ratio=sensitivity/1-specificity
# LRn=(1-all[3,2])/(all[2,2])   #negative likelihood ratio =(1-sensitivity)/specificity
chi=anova(mAge0, mAge1, test="Chisq")
predRisk1 <- predRisk(mAge1)
library ("Hmisc")
rt=improveProb(predRisk0, predRisk1, outcome)
o1b=cbind(deviance(mAge1),aic[2],ci[2], ci[1], ci[3],chi[2,4],chi[2,3], chi[2,5], rt$nri, rt$se.nri, rt$z.nri, rt$idi, rt$se.idi, rt$z.nri)#odds ratio and CI

##NEWS+MR+others
mAge2=glm(outcome~factor(admissionNEWS)+MRproADMRaw+Comorb0+MRproADMRaw*Comorb0, family = binomial, data=mydata, na.action=na.exclude)
aic=extractAIC(mAge2)
mydata$prob=predict(mAge2,type=c("response"))
library(pROC)
g <- roc(outcome ~ prob, data = mydata, ci=TRUE, of="thresholds", thresholds="best")
ci=ci.auc(g)
# rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp", "npv",
#           "ppv")
# all=ci.coords(g, x="best", input = "threshold", ret=rets)
# LRp=all[3,2]/(1-all[2,2])     #positive likelihood ratio=sensitivity/1-specificity
# LRn=(1-all[3,2])/(all[2,2])   #negative likelihood ratio =(1-sensitivity)/specificity
mydata_nomd=data.frame(mydata$admissionNEWS,mydata$MRproADMRaw,mydata$Comorb0, outcome, mydata$age) #remove missing data from outcome 1b for model comparison
mydata_nomd=droplevels(data.frame(na.omit(mydata_nomd))) 
mAge1_nomd=glm(outcome~factor(mydata.admissionNEWS)+mydata.MRproADMRaw, family = binomial, data=mydata_nomd)
mAge2_nomd=glm(outcome~factor(mydata.admissionNEWS)+mydata.MRproADMRaw+mydata.Comorb0+mydata.MRproADMRaw*mydata.Comorb0, family = binomial, data=mydata_nomd)

chi=anova(mAge1_nomd, mAge2_nomd, test="Chisq")
library ("PredictABEL")
predRisk1 <- predRisk(mAge1_nomd)
predRisk2 <- predRisk(mAge2_nomd)
library ("Hmisc")
rt=improveProb(predRisk1, predRisk2, mydata_nomd$outcome)
o1c=cbind(deviance(mAge2),aic[2],ci[2], ci[1], ci[3],chi[2,4],chi[2,3], chi[2,5], rt$nri, rt$se.nri, rt$z.nri, rt$idi, rt$se.idi, rt$z.nri)#odds ratio and CI

o1=rbind(o1a, o1b, o1c)
write.csv(o1, file = "logistic_O1.csv")

##-outcome 2: deteriorations events

outcome= mydata$NonNEWSdet
clm=which( colnames( mydata)=="NonNEWSdet" )


###NEWS
mAge0=glm(outcome~factor(admissionNEWS), family = binomial, data=mydata, na.action=na.exclude)
aic=extractAIC(mAge0)
mydata$prob=predict(mAge0,type=c("response"))
predRisk1 <- predRisk(mAge0)

library(pROC)
g <- roc(outcome ~ prob, data = mydata, ci=TRUE)
ci=ci.auc(g)

library ("PredictABEL")
predRisk0 <- predRisk(mAge0)

o1a=cbind(deviance(mAge0),aic[2],ci[2], ci[1], ci[3], 0, 0, 0, 0, 0, 0, 0, 0, 0)#odds ratio and CI

##NEWS+MR
mAge1=glm(outcome~factor(admissionNEWS)+MRproADMRaw, family = binomial, data=mydata, na.action=na.exclude)
aic=extractAIC(mAge1)
mydata$prob=predict(mAge1,type=c("response"))
library(pROC)
g <- roc(outcome ~ prob, data = mydata, ci=TRUE, of="thresholds", thresholds="best")
ci=ci.auc(g)
# rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp", "npv",
#           "ppv")
# all=ci.coords(g, x=0.3129972, input = "threshold", ret=rets)
# LRp=all[3,2]/(1-all[2,2])     #positive likelihood ratio=sensitivity/1-specificity
# LRn=(1-all[3,2])/(all[2,2])   #negative likelihood ratio =(1-sensitivity)/specificity
chi=anova(mAge0, mAge1, test="Chisq")
predRisk1 <- predRisk(mAge1)
library ("Hmisc")
rt=improveProb(predRisk0, predRisk1, outcome)
o1b=cbind(deviance(mAge1),aic[2],ci[2], ci[1], ci[3],chi[2,4],chi[2,3], chi[2,5], rt$nri, rt$se.nri, rt$z.nri, rt$idi, rt$se.idi, rt$z.nri)#odds ratio and CI

##NEWS+MR+others
mAge2=glm(outcome~factor(admissionNEWS)+MRproADMRaw+Comorb1+age+age2, family = binomial, data=mydata, na.action=na.exclude)
aic=extractAIC(mAge2)
mydata$prob=predict(mAge2,type=c("response"))
library(pROC)
g <- roc(outcome ~ prob, data = mydata, ci=TRUE, of="thresholds", thresholds="best")
ci=ci.auc(g)
# rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp", "npv",
#           "ppv")
# all=ci.coords(g, x="best", input = "threshold", ret=rets)
# LRp=all[3,2]/(1-all[2,2])     #positive likelihood ratio=sensitivity/1-specificity
# LRn=(1-all[3,2])/(all[2,2])   #negative likelihood ratio =(1-sensitivity)/specificity
mydata_nomd=data.frame(mydata$admissionNEWS,mydata$MRproADMRaw,mydata$Comorb1,mydata$age, mydata$age2, outcome) #remove missing data from outcome 1b for model comparison
mydata_nomd=droplevels(data.frame(na.omit(mydata_nomd))) 
mAge1_nomd=glm(outcome~factor(mydata.admissionNEWS)+mydata.MRproADMRaw, family = binomial, data=mydata_nomd)
mAge2_nomd=glm(outcome~factor(mydata.admissionNEWS)+mydata.MRproADMRaw+mydata.Comorb1+mydata.age+mydata.age2, family = binomial, data=mydata_nomd)

chi=anova(mAge1_nomd, mAge2_nomd, test="Chisq")
predRisk1 <- predRisk(mAge1_nomd)
predRisk2 <- predRisk(mAge2_nomd)
library ("Hmisc")
rt=improveProb(predRisk1, predRisk2, mydata_nomd$outcome)
o1c=cbind(deviance(mAge2),aic[2],ci[2], ci[1], ci[3],chi[2,4],chi[2,3], chi[2,5], rt$nri, rt$se.nri, rt$z.nri, rt$idi, rt$se.idi, rt$z.nri)#odds ratio and CI

o2=rbind(o1a, o1b, o1c)
write.csv(o2, file = "logistic_O2.csv")

###logistc regression lenght of stay
outcome= mydata$LengthOfStay
mAge0=lm(log10(outcome)~factor(admissionNEWS),  data=mydata)
aic=extractAIC(mAge0)
r=summary(mAge0)$r.squared
o3a=cbind(deviance(mAge0),aic[2],r, 0, 0, 0)#odds ratio and CI


mAge1=lm(log10(outcome)~factor(admissionNEWS)+log10(MRproADMRaw),  data=mydata)
aic=extractAIC(mAge1)
r=summary(mAge1)$r.squared
chi=anova(mAge0,mAge1, test="Chisq")
o3b=cbind(deviance(mAge1),aic[2],r,chi[2,4],chi[2,3], chi[2,5])#odds ratio and CI


mAge2=lm(log10(outcome)~factor(admissionNEWS)+log10(MRproADMRaw)+age,  data=mydata)
aic=extractAIC(mAge2)
r=summary(mAge2)$r.squared
chi=anova(mAge1,mAge2, test="Chisq")
o3c=cbind(deviance(mAge2),aic[2],r,chi[2,4],chi[2,3], chi[2,5])#odds ratio and CI


o3=rbind(o3a, o3b, o3c)
write.csv(o3, file = "lm_O3.csv")



####for graph of ROCs with pROC
#http://web.expasy.org/pROC/screenshots.html
outcome= mydata$DET72
mAge0=glm(outcome~factor(admissionNEWS), family = binomial, data=mydata, na.action=na.exclude)
prob0=predict(mAge0,type=c("response"))
mAge1=glm(outcome~factor(admissionNEWS)+MRproADMRaw, family = binomial, data=mydata, na.action=na.exclude)
prob1=predict(mAge1,type=c("response"))
mAge2=glm(outcome~factor(admissionNEWS)+MRproADMRaw+Comorb0+Comorb0*MRproADMRaw, family = binomial, data=mydata, na.action=na.exclude)
prob2=predict(mAge2,type=c("response"))

# g0 <- roc(outcome ~ prob, data = mydata, ci=TRUE, of="thresholds", thresholds="best")
# g1 <- roc(outcome ~ prob1, data = mydata, ci=TRUE, of="thresholds", thresholds="best")
# g2 <- roc(outcome ~ prob2, data = mydata, ci=TRUE, of="thresholds", thresholds="best")

png('ROC_1.png')
g0 <- plot.roc(outcome, prob0,main="ROC curves for prediction of Deterioration", percent=TRUE, col="#1c61b6", ci=TRUE)
ciobj <- ci.se(g0,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="blue", alpha=0.5) # plot as a blue shape

g1 <- lines.roc(outcome, prob1, percent=TRUE, col="#008600", ci=TRUE)
ciobj <- ci.se(g1,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="green", alpha=0.5) # plot as a blue shape

g2 <- lines.roc(outcome, prob2, percent=TRUE, col="#840000", ci=TRUE)
ciobj <- ci.se(g2,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="tomato", alpha=0.5) # plot as a blue shape

legend("bottomright", legend=c("Predictor set a", "Predictor set b", "Predictor set c"), col=c("#1c61b6", "#008600","#840000"), lwd=2)
dev.off()

g0 <- plot.roc(outcome, prob0,main="ROC curves", percent=TRUE, col="#1c61b6", ci=TRUE, cex.axis = 1.2, cex.lab = 1.5)
g1 <- lines.roc(outcome, prob1, percent=TRUE, col="#008600", ci=TRUE)
g2 <- lines.roc(outcome, prob2, percent=TRUE, col="#840000", ci=TRUE)

legend("bottomright",cex=1.2, legend=c("Predictor set a", "Predictor set b", "Predictor set c"), col=c("#1c61b6", "#008600","#840000"), lwd=2)

####
outcome= mydata$NonNEWSdet
mAge0=glm(outcome~factor(admissionNEWS), family = binomial, data=mydata, na.action=na.exclude)
prob0=predict(mAge0,type=c("response"))
mAge1=glm(outcome~factor(admissionNEWS)+MRproADMRaw, family = binomial, data=mydata, na.action=na.exclude)
prob1=predict(mAge1,type=c("response"))
mAge2=glm(outcome~factor(admissionNEWS)+MRproADMRaw+Comorb1+age+age2, family = binomial, data=mydata, na.action=na.exclude)
prob2=predict(mAge2,type=c("response"))


png('ROC_2.png')
g0 <- plot.roc(outcome, prob0,main="ROC curves for prediction of Deterioration events", percent=TRUE, col="#1c61b6", ci=TRUE)
ciobj <- ci.se(g0,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="blue", alpha=0.5) # plot as a blue shape

g1 <- lines.roc(outcome, prob1, percent=TRUE, col="#008600", ci=TRUE)
ciobj <- ci.se(g1,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="green", alpha=0.5) # plot as a blue shape

g2 <- lines.roc(outcome, prob2, percent=TRUE, col="#840000", ci=TRUE)
ciobj <- ci.se(g2,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="tomato", alpha=0.5) # plot as a blue shape

legend("bottomright", legend=c("Predictor set a", "Predictor set b", "Predictor set c"), col=c("#1c61b6", "#008600","#840000"), lwd=2)
dev.off()

 g0 <- plot.roc(outcome, prob0,main="ROC curves", percent=TRUE, col="#1c61b6", ci=TRUE,cex.axis = 1.2, cex.lab = 1.5)
 g1 <- lines.roc(outcome, prob1, percent=TRUE, col="#008600", ci=TRUE)
 g2 <- lines.roc(outcome, prob2, percent=TRUE, col="#840000", ci=TRUE)

legend("bottomright",cex=1.2, legend=c("Predictor set a", "Predictor set b", "Predictor set c"), col=c("#1c61b6", "#008600","#840000"), lwd=2)

###length of stay
# outcome= mydata$LengthOfStay
# mAge0=lm(log10(outcome)~factor(admissionNEWS),  data=mydata)
# mAge1=lm(log10(outcome)~factor(admissionNEWS)+log10(MRproADMRaw),  data=mydata)
# mAge2=lm(log10(outcome)~factor(admissionNEWS)+log10(MRproADMRaw)+age,  data=mydata)

g <- ggplot(mydata, aes(x = log(MRproADMRaw), y = log(Length Of Stay))
g <- g + geom_point() + geom_smooth(method = lm)
g <- g + labs(title = "Linear prediction of Length of Stay")
g <- g + xlab("Log MR-proADM (nmol/l)")
g <- g + ylab("Log Length of stay (hrs)")
g <- g + theme(text = element_text(size = 18))
g
####supplementary materials

los_grouped <- ifelse(log(mydata$LengthOfStay) > 4.34,1,0)#4.34=median of lenght of stay
outcome= factor(los_grouped)

mmr=glm(outcome~factor(mydata$admissionNEWS)+(mydata$MRproADMRaw), family=binomial(link='logit'))
mmrN=glm(outcome~(mydata$MRproADMRaw), family=binomial(link='logit'))
anova(mmr,mmrN)

m0=glm(outcome~factor(mydata$admissionNEWS), family=binomial(link='logit'))
o0=cbind(coef(m0),confint(m0), exp(cbind(OR = coef(m0),confint(m0))), coef(summary(m0))[,4])#odds ratio and CI
anova(m1)
m1=glm(outcome~factor(mydata$admissionNEWS)+log10(mydata$MRproADMRaw), family=binomial(link='logit'))
o1=cbind(coef(m1),confint(m1), exp(cbind(OR = coef(m1),confint(m1))), coef(summary(m1))[,4])#odds ratio and CI

m2=glm(outcome~factor(mydata$admissionNEWS)+log10(mydata$MRproADMRaw)+mydata$age, family=binomial(link='logit'))
o2=cbind(coef(m2),confint(m2), exp(cbind(OR = coef(m2),confint(m2))), coef(summary(m2))[,4])#odds ratio and CI

otot=rbind(o0, o1, o2)
write.csv(otot, file = "lr_lenghtofstay.csv")

prob0=predict(m0,type=c("response"))
prob1=predict(m1,type=c("response"))
prob2=predict(m2,type=c("response"))

library(pROC)
g0 <- plot.roc(outcome, prob0,main="ROC curves for prediction of Length of stay", percent=TRUE, col="#1c61b6", ci=TRUE)
ciobj <- ci.se(g0,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="blue", alpha=0.5) # plot as a blue shape

g1 <- lines.roc(outcome, prob1, percent=TRUE, col="#008600", ci=TRUE)
ciobj <- ci.se(g1,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="green", alpha=0.5) # plot as a blue shape

g2 <- lines.roc(outcome, prob2, percent=TRUE, col="#840000", ci=TRUE)
ciobj <- ci.se(g2,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj,  col="tomato", alpha=0.5) # plot as a blue shape

legend("bottomright", legend=c("Predictor set a.", "Predictor set b", "Predictor set c"), col=c("#1c61b6", "#008600","#840000"), lwd=2)

library(pROC)

g0 <- roc(outcome ~ prob0, data = mydata, ci=TRUE, of="thresholds", thresholds="best")
g1 <- roc(outcome ~ prob1, data = mydata, ci=TRUE, of="thresholds", thresholds="best")
g2 <- roc(outcome ~ prob2, data = mydata, ci=TRUE, of="thresholds", thresholds="best")

ci0=ci.auc(g0)
ci1=ci.auc(g1)
ci2=ci.auc(g2)

chi0=anova(m0, m1, test="Chisq")
chi1=anova(m1, m2, test="Chisq")

predRisk0 <- predRisk(m0)
predRisk1 <- predRisk(m1)
predRisk2 <- predRisk(m2)
library ("Hmisc")
rt0=improveProb(predRisk0, predRisk1, outcome)
rt1=improveProb(predRisk1, predRisk2, outcome)

o1c=cbind(deviance(mAge2),aic[2],ci[2], ci[1], ci[3],chi[2,4],chi[2,3], chi[2,5], rt$nri, rt$se.nri, rt$z.nri, rt$idi, rt$se.idi, rt$z.nri)#odds ratio and CI


write.csv(o1, file = "logistic_O1.csv")
###correlations
#legend
g0 <- g0 + scale_colour_discrete(name="Outcome 1",labels=c("No Deterioration", "Deterioration"))+ scale_shape_discrete(name="Outcome 1",labels=c("No Deterioration", "Deterioration"))+scale_fill_discrete(guide=FALSE)
outcome= factor(mydata$DET72)


png('descriptives.png')

g0 <- ggplot(mydata, aes(log10(admissionCRPraw), log10(MRproADMRaw), shape = outcome, fill = outcome))
g0 <- g0 + geom_point(aes(colour = outcome)) + geom_smooth(method = lm)
g0 <- g0 + labs(title = "MR-proADM vs CRP")+ ylab("Log MR-proADM (nmol/l)")+ xlab("Log CRP (mg/l)")
g0 <- g0 + theme_bw()+theme(legend.position="none")+ theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 14))

g1 <- ggplot(mydata, aes(log10(admissionWBC), log10(MRproADMRaw), shape = outcome, fill = outcome))
g1 <- g1 + geom_point(aes(colour = outcome)) + geom_smooth(method = lm)
g1 <- g1 + labs(title = "MR-proADM vs WBC")
g1 <- g1 + ylab("Log MR-proADM (nmol/l)")+ xlab("WBC (x109/l)")+xlim(0.4, 1.5)
g1 <- g1 + theme_bw()+theme(legend.position="none")+ theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 14))


g2 <- ggplot(mydata, aes(age, log10(MRproADMRaw), shape = outcome, fill = outcome))
g2 <- g2 + geom_point(aes(colour = outcome)) + geom_smooth(method = lm)
g2 <- g2 + labs(title = "MR-proADM vs Age")
g2 <- g2 + ylab("Log MR-proADM (nmol/l)") + xlab("Age (years)")
g2 <- g2 + theme_bw()+theme(legend.position="none")+ theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 14))

g3 <- ggplot(mydata, aes(factor(admissionNEWS), log10(MRproADMRaw), fill = outcome))
g3 <- g3 + geom_boxplot(aes(fill = outcome))
g3 <- g3 + labs(title = "MR-proADM vs NEWS")
g3 <- g3 + ylab("Log MR-proADM (nmol/l)")
g3 <- g3 + xlab("NEWS")
g3 <- g3 + theme_bw()+theme(legend.position="none")+ theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 14))

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(g0, g1, g2, g3, cols=2)

dev.off()

outcome= factor(mydata$DET72)

mydata_nomd=data.frame(mydata$admissionNEWS,mydata$MRproADMRaw,mydata$Comorb0, outcome) #remove missing data from outcome 1b for model comparison
mydata_nomd=droplevels(data.frame(na.omit(mydata_nomd))) 

png('interaction.png')
g <- ggplot(mydata_nomd, aes(mydata.Comorb0,log10(mydata.MRproADMRaw)))+ geom_boxplot(aes(fill=outcome))
g <- g + labs(title = "MR-proADM interaction with COPD/HF")
g <- g + xlab("COPD/HF")
g <- g + ylab("Log MR-proADM (nmol/l)")
g <- g + scale_fill_discrete(name="Outcome 1",labels=c("No Deterioration", "Deterioration"))
g <- g + theme(legend.text = element_text(size = 13))
g <- g + theme(legend.title = element_text(size=14))
g <- g +theme_bw()+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 14))
g

dev.off()


outcome= factor(mydata$DET72)

g2 <- ggplot(mydata, aes(age, log10(LengthOfStay)))
g2 <- g2 + geom_point() + geom_smooth(method = lm)
g2 <- g2 + labs(title = "Length of stay vs Age")
g2 <- g2 + ylab("Log Length of stay (hrs)") + xlab("Age (years)")
g2 <- g2 + theme_bw()+theme(legend.position="none")+ theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 14))

cor(mydata$age, log10(mydata$MRproADMRaw), method="pearson") 

g3 <- ggplot(mydata, aes(factor(admissionNEWS), log10(LengthOfStay), fill = outcome))
g3 <- g3 + geom_boxplot(aes(fill = outcome))
g3 <- g3 + labs(title = "Length Of Stay vs NEWS")
g3 <- g3 + ylab("Log Length Of Stay(hours)")
g3 <- g3 + xlab("NEWS")
g3 <- g3 + theme_bw()+theme(legend.position="none")+ theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 14))


###curiosity

outcome= mydata$LengthOfStay
mAge2=lm(log10(outcome)~admissionNEWS+log10(MRproADMRaw)+age+factor(DET72)+factor(DET72)*age ,  data=mydata)

mAge3=lm(log10(outcome)~admissionNEWS+log10(MRproADMRaw)+age+factor(NonNEWSdet)+factor(NonNEWSdet)*age ,  data=mydata)
