# SURVIVAL ANALYSIS----
#===========================================-
# CLEAR R WORKING SPACE----
#============================================-
rm(list=ls())
#============================================-
# Set path
setwd("C:/CDK") 
#===========================================-
#Load packages
library(dplyr)
library(survival)
library(survminer)
library(readxl)
library(summarytools)
#============================================-
# Import lung data set
#============================================-
CDK <- read_excel("CDK.xlsx",sheet = "colon")
#============================================-
#  Data processing
#============================================-
colon<-CDK|>
  select(time,status,age,sex,rx,adhere)
#===========================================-
lung$status<-as.factor(lung$status)
# Value label
#+++++++++++++++
#============================================-
#================Sex======================-
colon$sex<-ordered(colon$sex,levels=c(0,1),
                     labels=c("Female","Male")) 
#===========================================-
#===============Kaplan- Meier curve==========-
# Fit survival data using the Kaplan-Meier method
Surv_object<- Surv(time = colon$time, event = colon$status)
Surv_object
#================================================-
# Fit the object======================-
fit1 <- survfit(Surv_object ~ status, data = colon)
summary(fit1)
# fit the object with sex==========================
fit2 <- survfit(Surv_object ~ sex, data = colon)
summary(fit2)
# fit the object with rx==========================
fit3 <- survfit(Surv_object ~ rx, data = colon)
summary(fit3)
#======================================-
### Kaplan Meir curve
#dev.off()
ggsurvplot(fit1, data =colon, pval = TRUE)
ggsurvplot(fit2, data =colon, pval = TRUE)
ggsurvplot(fit3, data =colon, pval = TRUE)
#============================================-
# Cox regression model
#=============================================-
fit.coxph <- coxph(Surv_object ~ rx + age + sex,
                   data = colon)
summary(fit.coxph)
#============================================-
# Survival analysis using ovarian data set
#==============================================-
# Import data
CDK <- read_excel("CDK.xlsx",sheet = "ovarian")
ovarian<-CDK|>
  select(futime,fustat,age,resid.ds,rx,ecog.ps)
#==============================================-
#Data management
# Change data labels
ovarian$rx <- factor(ovarian$rx,
                     levels = c("1", "2"),
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds,
                           levels = c("1", "2"),
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps,
                          levels = c("1", "2"),
                          labels = c("good", "bad"))
#=======================================================-
#Examine distribution
#=====================================================-
hist(ovarian$age,col = "blue",
     main = "Simple histogram shows Age distribution")
#======================================================-
#Dichotomize age
ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)
#=================================================-
# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object
#=========================================================-
# Fit 
fit4 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit4)
#==========================================================-
#Plot Kaplan Meir
ggsurvplot(fit4, data = ovarian, pval = TRUE)
#==========================================================-