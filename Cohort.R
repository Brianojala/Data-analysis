#=========================================-
# Prospective Cohort Study Analysis----
#Dataset Name:Heart Disease 
# ========================================-
# CLEAR WORKSPACE---- 
# =========================================-
rm(list = ls(all.names = TRUE))
# =========================================-
# Load Packages
library(tidyverse)
library(dplyr)
library(expss)
library(summarytools) 
library(table1)
library(psych)
library(gtsummary)
library(flextable)
library(officer)
library(broom)
library(broom.helpers)
library(gt)
#library(plyr)
library(readxl)
library(writexl)
library(finalfit)
#library(scales)
library(caret) # For machine learning
library(doSNOW) # machine learning
library(kernlab)
#===========================----
# New packages
#install.packages("kernlab")
#==================================-
#1.0 Working directory----
setwd("C:/Epidemiology")
#1.1 Import data----
heart <- read_excel("heart.xlsx",sheet = "heart")
#================================-
#1.2 Data processing----
heart<-heart|>
  mutate(
    sex= factor(sex,
                levels = c(0,1),
                      labels = c("Female","Male"),
                      exclude = NA),
    cp=factor(cp,
              levels = c(0,1,2,3),
              labels = c("typical angina","atypical angina",
                         "non- anginal pain","asymptomatic"),
              exclude = NA),
    fbs=factor(fbs,
                levels = c(0,1),
                labels = c("False","True"),
                exclude = NA),
    restecg=factor(restecg,
                    levels = c(0,1,2),
                    labels = c("Normal","having ST-T wave abnormality",
                               "showing probable"),
                    exclude = NA),

    exang=factor(exang,
                  levels = c(0,1),
                  labels=c("No","Yes"),
                  exclude = NA),
    
    slope=factor(slope,
                    levels = c(0,1,2),
                    labels=c("up sloping","flat",
                             "down sloping"),
                     exclude = NA),
    ca=factor(ca,
               levels = c(0,1,2,3)),
              thal=factor(thal,
                      levels = c(0,1,2,3),
                      labels = c("NULL","normal blood flow","fixed defect",
                                 "reversible defect"),
                      exclude = NA),
              target=factor(target,
                                  levels = c(0,1),
                                  labels = c("Absent","Present")))|>
      apply_labels(
        age= 	"Patients Age in years" ,
        sex=	"Gender",
        cp=	"Type of chest pain",
       trestbps=	"Patient's level of BP at resting mode in mm/HG" ,
        chol=	"Serum cholesterol in mg/dl",
        fbs=	"Blood sugar levels on fasting > 120 mg/dl",
      restecg=	"Result of electrocardiogram while at rest" ,
      thalach=	"Maximum heart rate achieved" ,
     exang=	"Angina induced by exercise" ,
     oldpeak=	"Exercise induced ST-depression" ,
     slope="ST segment measured in terms of slope",
     ca=	"The number of major vessels",
      thal=	"A blood disorder called thalassemia",
      target=	"Heart Disease")|>
  mutate(
    Age_cat=case_when(
      age<35~1,
      age>=35 & age<40~2,
      age>=40 & age<45~3,
      age>=45 & age<50~4,
      age>=50 & age<55~5,
      age>=55 & age<60~6,
      age>=60 & age<65~7,
      age>=65~8
    ))|>
  mutate(
    Age_cat=factor(Age_cat,
                          levels = c(1,2,3,4,5,6,7,8),
                          labels = c("29-34","35-39","40-44","45-49",
                                     "50-54","55-59","60-64","65 and Above"),
                          
    ))
#1.3 Save in Epidemiology file----
#save(heart,file="C:/Epidemiology/heart.RData")
#=============================================-
#1.4 Load the dataset----
#load("C:/Epidemiology/dat_1.RData")
# 1.5 Export to CSV format----
write.csv(heart,file="heart.csv")
#===============================- 
#2.0 Data Visualization---- 
#2.1 Stack bar graph----
df<-heart%>%
  dplyr::group_by(sex,target)%>%
  dplyr::tally()%>%
  dplyr::mutate(percentage=n/sum(n))
# plotting the graph 
ggplot(data=df,aes(x=sex,y=n,fill=target))+
  geom_bar(stat = "identity",width = 0.7)+
  geom_text(aes(label = paste0(sprintf("%1.1f",percentage*100),"%")),
            position = position_stack(vjust = 0.5),colour="black")+
  theme_classic()+
  labs(title = "Heart disease distribution",
       y= "Percentage distribution",
       x="Gender")
#==========================================-
#1
data <- data.frame("Sex" = c("Female", "Male"),
                   "Freq" = c(87,183),
                   "Percent" = c("32.2%", "67.8%"))
#++++++++++++++++++++++++++++++++++++++++++++
ggplot(data, aes(x = Sex, y = as.numeric(Freq))) + 
  geom_bar(stat = "identity", color = "black", fill = "dodgerblue1")+
  geom_text(label = with(data, paste(Freq, paste0('(', Percent, ')'))), vjust=-1) +
  ylim(0, 200)+
  labs(title = "Participants in study",
       y="Frequency",
       x="Sex")
#+++++++++++++++++++++++++++++++++++++
#==========================================-
##
#3.0 STATISTICAL DATA ANALYSIS----
#3.1 Descriptive statistics----
Tab<-heart|>
  select(age,sex,cp,trestbps,chol,
         fbs,restecg,thalach,exang,oldpeak,
         slope,ca,thal,target)|>
  mutate(
      Sex= relevel(sex,ref = "Female"),
      cp= relevel(cp,ref = "typical angina"),
      fbs= relevel(fbs,ref = "False"),
      restecg=relevel(restecg,ref = "Normal"),
      exang=relevel(exang,ref = "No"),
      #slope=relevel(slope,ref ="Up Sloping"),
      ca=relevel(ca,ref = "0"),
      thal=relevel(thal,ref="normal blood flow")
  )

Mystat<-list(all_continuous()~"{mean} ± {sd}",
             all_categorical()~"{n} ({p})")
MyDigit<-list(all_continuous()~c(2,2),all_categorical()~c(0,2))
Table1<-Tab%>%
  tbl_summary(by=target,missing = "no",statistic = Mystat,digits = MyDigit)%>%
  bold_labels()
Table1
#====================================-
# 3.1.0 Bivariate Analysis----
#====================================-
Tab_1 <-Tab |>
  tbl_summary(
    by = target,  # Uncomment if you want group-wise summary
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    percent = "column",
    missing = "no"
  ) |>
  add_overall() |>
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) |>
  modify_footnote(all_stat_cols() ~ "Median (IQR)") |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Heart Disease **") |>
  modify_caption("Table 1:Bivariate Analysis") |>
  bold_labels() |>
  add_n() |>
  as_flex_table()
sect_properties <- prop_section(page_size = page_size(orient = "portrait"))#, width = 8.3, height = 11.7)
save_as_docx(Tab_1,path="Table4c.docx", pr_section = sect_properties)
Tab_1
#===============================================-
#3.2 Logistic Regression----
#3.2.1 Un Adjusted Odd Ratios----
OR1<-Tab|>
  select(age,sex,cp,trestbps,chol,
                fbs,restecg,thalach,exang,oldpeak,
                slope,ca,thal,target)|>
  tbl_uvregression(method = glm,y=target,
                   method.args = list(family=binomial()),
                   exponentiate = TRUE,pvalue_fun = ~style_pvalue(.x,digits = 3))|>
  modify_column_merge(pattern = "{estimate} ({ci})",rows = ! is.na(estimate))|>
  modify_header(estimate~"**OR(95%C.I)**")|>
  bold_labels()
OR1
#3.2.2 Adjusted Odds ratio----
OR2<- glm(target~age+sex+cp+trestbps+chol+
          fbs+restecg+thalach+exang+oldpeak+
          slope+ca+thal,data = Tab,family = binomial())|>
  tbl_regression(
    exponentiate = TRUE,pvalue_fun = ~style_pvalue(.x,digits = 3))|>
  modify_column_merge(pattern = "{estimate} ({ci})",rows = ! is.na(estimate))|>
  modify_header(estimate~"**OR(95%C.I)**")|>
  bold_labels()

OR2
# Merging Tables----
Table_2<-tbl_merge(
  tbls = list(OR1,OR2),
  tab_spanner = c("**Unadjusted**","**Adjusted**")
)
Table_2
# Reporting in word document
Table_2|>
  as_gt()|>
  gtsave(filename = "TABle_2gi.docx",path = "C:/Epidemiology")
#============================================-
# 
# Outcome 
dependent <- "target"

# Explanatory variables
explanatory <- c(
  "age",
  "sex",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal"
)
#3.2.3 Forest plot---- 
plot1=heart %>% or_plot(dependent, explanatory, remove_ref = TRUE, , table_text_size = 4, title_text_size = 18,
                         dependent_label = "Predictors of heart disease", prefix = "Forest Plot: - ", suffix = "")
plot1
ggsave("forest2_mdr.png", plot = plot1, width = 10, height = 6, dpi = 300)
#=======================================-

