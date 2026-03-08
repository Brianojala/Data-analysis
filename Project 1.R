# CHRONIC KIDNEY DISEASE DATA SET----
#==========================================-
# CLEAR WORKING SPACE
rm(list = ls(all.names = TRUE))
#========================================-
# SET WD
setwd("C:/CDK") 
#==================================-
# LOAD PACKAGES
#===================================-
library(tidyverse)
library(expss)
library(table1)
library(gtsummary)
library(flextable)
library(officer)
library(broom)
library(broom.helpers)
library(gt)
library(readxl)
library(writexl)
library(finalfit)
library(ggplot2)
library(e1071)
library(psych)
#=====================================- 
# LOAD DATA SET
CDK <- read_excel("CDK.xlsx",sheet = "CDK")
#======================================-
# View Data set
#=======================================-
view(CDK)
#========================================- 
#1.1 DATA CLEANING-----  
#1.1.1 Keep variables-----
CDK<-CDK|>
  select(patient_age,gender,bp_systolic,bp_diastolic,
         blood_urea,blood_glucose_random,diabetes,hypertension,
         drug_name,drug_dosage_mg,exposure_days,nephrotoxic_label,
         ckd_risk_label)
#==================================================-
CDK<-CDK|>
  mutate(
    diabetes= factor(diabetes,
                     levels = c(0,1),
                     labels = c("No","Yes"),
                     exclude = NA),
    hypertension=factor(hypertension,
                        levels = c(0,1),
                        labels = c("No","Yes"),
                        exclude = NA),
    nephrotoxic_label=factor(nephrotoxic_label,
                             levels = c(0,1),
                             labels = c("non-nephrotoxic","nephrotoxic"),
                             exclude = NA),
    ckd_risk_label=factor(ckd_risk_label,
                          levels = c(0,1,2),
                          labels = c("Low risk","Moderate risk",
                                     "High risk"),
                          exclude = NA),
    gender=factor(gender,
                  labels = c("Female","Male"),
                  exclude = NA),
    drug_name=factor(drug_name,
                     labels = c("Amphotericin-B ","Aspirin","Cisplatin",
                                "Gentamicin","Ibuprofen","Paracetamol ",
                                "Tobramycin ","Vancomycin"),
                     exclude = NA))|>
  apply_labels(
    patient_age= 	"Patient Age(Years)" ,
    gender=	"sex",
    bp_systolic="Systolic blood pressure(mm/Hg)",
    bp_diastolic="Diastolic blood pressure(mm/HG)",
    blood_urea=	"Blood urea(mmol/L)",
    drug_dosage_mg=	"Drug dosage(mg)",
    exposure_days=	"Days of exposure" ,
    drug_name="Drug Type",
    blood_glucose_random="Blood glucose",
    nephrotoxic_label=	"nephrotoxic medication" ,
    ckd_risk_label=	"Risk of chronic Kidney disease ")|>
  mutate(
    Age_cat=case_when(
      patient_age>=18& patient_age<=22~1,
      patient_age>=23 & patient_age<=27~2,
      patient_age>=28 & patient_age<=32~3,
      patient_age>=33 & patient_age<=37~4,
      patient_age>=38 & patient_age<=42~5,
      patient_age>=43 & patient_age<=47~6,
      patient_age>=48 & patient_age<=52~7,
      patient_age>=53 & patient_age<=57 ~8,
      patient_age>=58 & patient_age<=62~9,
      patient_age>=63 & patient_age<=67~10,
      patient_age>=68 & patient_age<=72~11,
      patient_age>=73 & patient_age<=77~12,
      patient_age>=78 & patient_age<=82~13,
      patient_age>=83 & patient_age<=87~14,
      patient_age>=88 & patient_age<=92~15
    ))%>%
  mutate(
    Age_cat=factor(Age_cat,
                   levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                   labels = c("18-22","23-27","28-32","33-37",
                              "38-42","43-47","48-52","53-57","58-62",
                              "63-67","68-72","73-77","78,82","83-87",
                              "88-92")))|>
  apply_labels(
    Age_cat= 	"Patients Age group(years)")
  
#==================================================-
#  Adding another column-> Grouping exposure days of drug use
CDK<-within(CDK,{
    exposure_cat<-NA
    exposure_cat[exposure_days>=1 & exposure_days<=4]<-"1-4"
    exposure_cat[exposure_days>=5 & exposure_days<=9]<-"5-10"
    exposure_cat[exposure_days>=10 & exposure_days<=14]<-"10-14"
    exposure_cat[exposure_days>=15 & exposure_days<=19]<-"15-19"
    exposure_cat[exposure_days>=20 & exposure_days<=24]<-"20-24"
    exposure_cat[exposure_days>=25 & exposure_days<=29]<-"25-29"
    exposure_cat[exposure_days>=30 & exposure_days<=34]<-"30-34"
  }) 
#=======label exposure_cat to exposure days
CDK<-apply_labels(CDK,
                  exposure_cat="exposure days")
#==========================================================-
#1.1.2 Save  Data set as CDK.RData-----
save(CDK,file="C:/CDK/CDK.RData")
#=================================================- 
# Explore
view(CDK)
#====================================================- 
# 2.0 DATA VISUALIZATION----
# 2.1 Checking the distribution of CKD with female data set----
# Filter female data set
#=============================================- 
# Explore gender
CDK%>%count(gender,sort = TRUE)
CDKf<-CDK|>
  filter(gender=="Female")
#========================================-
View(CDKf)
#=========================================- 
# 2.2 Visualize categorical variables
#++++++++++++++++++++++++++++++++++++++++++
# 2.2.1 Patient Age categories-------- 
# Summarized using count()
df<-CDKf%>%
  select(Age_cat,gender)%>%
  count(Age_cat)|>
ggplot(aes(x=reorder(Age_cat,n),y=n))+
  geom_bar(stat = "identity",fill="violet",color="white")+
  geom_text(aes(label = n),hjust=1.45)+coord_flip()+
  theme_classic()+labs(x="Patients Age(Years)",y="Count",
                       title = "Female Patients Age Distribution")
df
#=============================================================-
# 2.2.2 Patients Health Condition-----
# 2.2.2.1 Proportion of female with Hypertension-----
df1<-CDKf%>%
  select(hypertension)%>%
  count(hypertension)%>%
  mutate(Percentage=n/sum(n),
         perce_label=paste0(round(Percentage*100),"%"))%>%
  ggplot(aes(x=reorder(hypertension,Percentage),
                 y=Percentage))+
  geom_bar(stat="identity",fill="pink",color="black")+
  geom_text(aes(label=perce_label),vjust=-0.25)+
  labs(x="Hypertension status",y="Percent",
       title = "% of female patient with hypertension problem")+
  scale_y_continuous(labels = scales::percent)+
  theme_bw() 
df1
#====================================================-
# 2.2.2.2 Proportion of female patients with diabetes-----
df2<-CDKf%>%
  select(diabetes)%>%
  count(diabetes)%>%
  mutate(Percentage=n/sum(n),
         perce_label=paste0(round(Percentage*100),"%"))%>%
  ggplot(aes(x=reorder(diabetes,Percentage),
             y=Percentage))+
  geom_bar(stat="identity",fill="purple",color="black")+
  geom_text(aes(label=perce_label),vjust=-0.25)+
  labs(x="Diabetes status",y="Percent",
       title = "% of female patient with diabetes problem")+
  scale_y_continuous(labels = scales::percent)+
  theme_classic() 
df2
#=================================================- 
#2.2.2.3 Proportion of female patients with diabetes-----
  df3<-CDKf%>%
  select(drug_name)%>%
  count(drug_name)%>%
  mutate(Percentage=n/sum(n),
         perce_label=paste0(round(Percentage*100),"%"))%>%
  ggplot(aes(x=reorder(drug_name,Percentage),
             y=Percentage))+
  geom_bar(stat="identity",fill="skyblue",color="black")+
  geom_text(aes(label=perce_label),vjust=-0.25)+
  labs(x="Drug type",y="Percent",
       title = "% of female patient use drug")+
  scale_y_continuous(labels = scales::percent)+
  theme_classic() 
df3
#=======================================================- 
#2.2.2.4 Proportion of exposure days----
#============================================-
Exposure<-data.frame("exposurecat"= c("1-4","5-10","10-14","15-19",
                                      "20-24","25-29"),
                   "Freq" = c(97,132,131,127,140,149),
                   "Percent" = c("12.5%","17.0%", "16.9%",
                                 "16.4%","18.0%","19.2%"))
Exposure$exposurecat<-as.factor(Exposure$exposurecat)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Exposure|>
ggplot(aes(x =exposurecat, y = as.numeric(Freq))) + 
  geom_bar(stat = "identity", color = "black", fill = "dodgerblue1")+
  geom_text(label= with(Exposure, paste(Freq, paste0('(', Percent, ')'))), 
            vjust=-1) +
  ylim(0, 200)+
  labs(title = "Days of drug consumption by female patients",
       y="Female patients",
       x="Days of consumption drug")
#=============================================================- 
# 2.3 Visualize continuous variables
# 2.3.1 Distribution of Patient Age (Years)
#2.3.1.1: Normality Assumption
CDKf%>%
  ggplot(aes(x=patient_age))+
  geom_histogram(fill="blue",color="white")+
  theme_classic()+
  labs(title = "Age distribution of female patient",
       y="Counts",
       x= "Patients Age(Years)")
# Testing normality for clarity using shapiro wilk test
shapiro.test(CDKf$patient_age)
# Note: Normality assumption in age is violate
# Reason : P-value <-0.05 thus fail to reject Ho
# 2.2.3.2: Identify Outliers in patient Age----
boxplot(CDKf$patient_age,col = "violet")
#==========================================-
# 2.3.2 TEST NORMALITY  ASSUMPTION USING SHAPIRO WILK TEST----
# Please note that:The remaining continuous scale variables i---- 
#used Shapiro test----
#==========================================-
shapiro.test(CDK$bp_systolic)
shapiro.test(CDK$bp_diastolic)
shapiro.test(CDK$blood_urea)
shapiro.test(CDK$blood_glucose_random)
shapiro.test(CDK$drug_dosage_mg)
#===========================================-
#N/B: All the variables met normality assumption except----
#Patient age and drug dosage(mg) 
#============================================-
#2.3.2.1 Describe continuous variables----
CSV<-CDK|>
  select(patient_age,bp_diastolic,drug_dosage_mg,bp_systolic,
         blood_urea,blood_glucose_random)
describe(CSV)
#==================================================- 
#PART 3: STATISTICAL ANALYSIS----
# 3.1 Descriptive Statistics----
Table<-CDK|>
  select(gender,diabetes,hypertension,drug_name,exposure_cat,ckd_risk_label,
         bp_systolic,bp_diastolic,blood_urea,blood_glucose_random,
         ,exposure_days,nephrotoxic_label)

Mystat<-list(all_continuous()~"{mean} ± {sd}",
             all_categorical()~"{n} ({p})")
MyDigit<-list(all_continuous()~c(2,2),all_categorical()~c(0,2))
Table1<-Table%>%
  tbl_summary(by=nephrotoxic_label,missing = "no",statistic = Mystat,digits = MyDigit)%>%
  bold_labels()
Table1
#========================================================-

Table2<-CDK%>%
  select(patient_age,drug_dosage_mg,nephrotoxic_label)
table1(~patient_age+drug_dosage_mg|nephrotoxic_label,data=Table2)
#==================================================-
# 3.2: Inferential Statistics-----
#Objective 1:To Understand how clinical and drug-specific factors----
#together influence kidney health----
# 3.2.1 Test of hypothesis-----
# Ho: No association Vs H1: There is association---- 
# Statistical method used: Chi-square test, Welch Test----
#====================================================-
Tab_1 <-CDK|>
  tbl_summary(
    by =nephrotoxic_label,  # Uncomment if you want group-wise summary
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    percent = "column",
    missing = "no"
  ) |>
  add_overall() |>
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) |>
  modify_footnote(all_stat_cols() ~ "Mean (SD)") |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**nephrotoxic**") |>
  modify_caption("Table 1:Charateristics of patient information") |>
  bold_labels() |>
  add_n() |>
  as_flex_table()
sect_properties <- prop_section(page_size = page_size(orient = "portrait"))#, width = 8.3, height = 11.7)
save_as_docx(Tab_1,path="Table2c.docx", pr_section = sect_properties)
Tab_1
#===============================================-
#3.2 LOGISTIC REGRESSION----
#3.2.1 Univariate Logistic regression ----
# 3.2.1.1 An Adjusted Odds Ratio----
OR1<-CDK|>
  select(gender,diabetes,hypertension,exposure_cat,ckd_risk_label,
         bp_systolic,bp_diastolic,blood_urea,blood_glucose_random,
         ,exposure_days,nephrotoxic_label)|>
  tbl_uvregression(method = glm,y=nephrotoxic_label,
                   method.args = list(family=binomial()),
                   exponentiate = TRUE,pvalue_fun = ~style_pvalue(.x,digits = 3))|>
  modify_column_merge(pattern = "{estimate} ({ci})",rows = ! is.na(estimate))|>
  modify_header(estimate~"**OR(95%C.I)**")|>
  bold_labels()
OR1
#============================================-
#3.2.2 Fitted multiple logistic regression model----
# Multiple logistic regression model was fitted to control the confounder----
#variables that might distort the result-----
#3.2.2 Adjusted Odds ratio----
OR2<- glm(nephrotoxic_label~gender+diabetes+hypertension+exposure_cat+ckd_risk_label+
          bp_systolic+bp_diastolic+blood_urea+blood_glucose_random+
          exposure_days,data = CDK,family = binomial())|>
  tbl_regression(
    exponentiate = TRUE,pvalue_fun = ~style_pvalue(.x,digits = 3))|>
  modify_column_merge(pattern = "{estimate} ({ci})",rows = ! is.na(estimate))|>
  modify_header(estimate~"**OR(95%C.I)**")|>
  bold_labels()
OR2
# 3.2.2.1Merging Tables----
Table_2<-tbl_merge(
  tbls = list(OR1,OR2),
  tab_spanner = c("**Unadjusted**","**Adjusted**")
)
Table_2
# 3.2.2.1Reporting Table2 in a word document----
Table_2|>
  as_gt()|>
  gtsave(filename = "TABle_2.docx",path = "C:/CDK")
#============================================-
# 3.3 Forest plot-----
# 3.3.1 Final fit of 95%Cl(OR)------ 
# Outcome 
dependent <- "nephrotoxic_label"
# Explanatory variables
explanatory <- c(
  "gender",
  "diabetes",
  "hypertension",
  "exposure_cat",
  "ckd_risk_label",
  "bp_systolic",
  "bp_diastolic",
  "blood_urea",
  "blood_glucose_random",
  "exposure_days")
plot1=CDK %>% or_plot(dependent, explanatory, remove_ref = TRUE, , table_text_size = 4, title_text_size = 18,
                        dependent_label = "Predictors of chronic kidney disease", prefix = "Forest Plot: - ", suffix = "")
plot1
ggsave("forest1.png", plot = plot1, width = 10, height = 6, dpi = 300)
#=======================================-
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PLEASE CONTINUE WITH SECOND PART TO ANSWER OBJECTIVE 2
#NEXT: APPLICATION OF MACHINE LEARNING IN EPIDEMIOLOGICAL REASEARCH
#===============================================-
#ABOUNT ME: 
#NAME:OJALA BRIAN OLOO
#CADRE:CONSULTANT DATA ANALYST
#STATION:MASENO UNIVERSITY
#OFFICE: LECTURER PARLOUR
# EDUCATION LEVEL: BSC APPLIEND STATISTICS WITH IT
#Currently finalizing MSC.Epidemiology AND BIOSTATISTICS
# How to get me
# CALL:0743670039 OR 
# EMAIL: Brianojala4@gmail.com
#THANK FOR YOUR TIME----------
