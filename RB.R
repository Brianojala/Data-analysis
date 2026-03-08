#NAME:BRIAN OJALA
#CADRE: DATA ANALYSTCONSULTANT/TRAINER
#DATE:18-11-2025
#TOPIC 1: INTRODUCTION TO R 
#	Creating an R file
#	Writing Scripts in R file
#	Saving an R file
#	Execution of  an R file
#==========================================
# Introduction of this part is in slide
# PART 2: Data Operator
#Operators are the constructs which can
# manipulate the value of operators for example
#==========Assignment Operator===============
# There are three types of Assignment operators used
# in R
#1 Left Assignment  x<- c(1,2)
#2 equal Assignment x= c(1,2)
#3 right Assignment C(1,2)->x
x<-c(1,2) # But left one is the most preferable
x=c(1,2)
c(1,2)->x
#=============Arithmetic Operators============
X<-8
Y<-4
X+Y    # Addition
X-Y    #Subtraction
X*Y    #Multiplication
X/Y    # Division
Y^2    # Exponent
8+4-2*6/3
#==============Relation Operators============
a=10
b=8
a==b   # Equal to
a!=b   # Not equal to
a>b    # Greater than
a<b    # Less than
a>=b   #Greater than or equal to
a<=b   # Less than or equal to

#============================================
# PART 3: DATA TYPES IN R
#Vector, matrix, Array,List and Data frame
#===========================================
#========== Vector==========
#  A vector is a sequence of data elements of the
# same basic type.
#  Atomic of vectors
#1 Logical- which is TRUE/FALSE
V1<-c(TRUE,FALSE)
V1
#2 Integer-15L,30L,100L
V2<-c(15L,30L,100L)
V2
#3 Numeric this can be 15, 3.142,948
V3<-c(15,3.142,948) 
V3
# Character
V4<-c("Hello","how","is","you")
V4
#============Matrix=============
#Matrix are R objects in which the elements
#are arranged in a two-dimensional rectangular 
#layout
Matrix<-matrix(c(5:29),5,5)
Matrix
Matrix1<-matrix(c(2:7),2,3)
Matrix1
#Method 2: You can create a matrix from
# avector
M1<-c(1,2,3)
M2<-c(4,5,6)
matrix<-rbind(M1,M2)
matrix
# OR
Matri_1<-matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,
                byrow=F)
Matri_1
#==============Array================
#Array are the R data objects which can  
# store data in more than two dimensions
arr=array(c(0:15),dim = c(4,4,2,2))
arr
arr=array(c(1:9),dim = c(3,3,4,2))
arr
#==============List==================
#List are the R objects which contain elements
# of different types like numbers,strings,vectors,
# and other list inside it.
# For example
L1<-c(5.678,32,95,31.6)
L2<-c("Hey","How are you","Thank you")
mylist<-c(L1,L2)
mylist
#================Data Frame==============
# Data frame is a table or a two-dimensional
# array-like structure in which each column
#contains one set of values from each column
ID<-c(1:9)
age<-c(19,33,20,21,18,21,22,17,29)
lwt<-c(182,155,105,108,107,124,118,103,123)
race<-c("Black","Other","White","White","White",
        "Other","White","Other","White")
Smoke<-c(0,0,1,1,1,0,0,0,1)
birth<-data.frame(ID,age,lwt,race,Smoke)
birth
#============================================
#PART3 : DATA MANAGEMENT in R
# DATA SET NAME: STI DATA FROM KEMRI
#============================================
# Clear R environment
rm(list=ls())
#Create working directory
# Set working directory using setwd("C:/CDK")
setwd("C:/CDK")
# Now Lets import data set called STI1
library(readr)
STI1 <- read_csv("STI1.csv")
View(STI1)
# Explore the data set
length(STI1$IdNumber)
attributes(STI1)
names(STI1)
dim(STI1)
#=================================-
#DATA MANAGEMENT/Manipulation
library(tidyverse)
library(expss)
#==================================
# First let delete unwanted variables from
# the STI1 data set
Dropvar<-names(STI1)%in%c("Date...8","Date...9",
                          "DATE")
STI1<-STI1[!Dropvar]
#=====================================
# Value label
STI1$Gender<-ordered(STI1$Gender,levels=c(1,2),
                         labels=c("Female","Male"))
STI1$Church<-ordered(STI1$Church,levels=c(1,2,3,4,5,6,7),
                    labels=c("Anglican","Apostolic","Atheist",
                             "Methodist","Pentecostal","Roman catholic",
                             "Other"))
#=======================================
#variable label
STI1<-apply_labels(STI1,
                   Church="Participants with their churches",
                   Weight="Weight in Kgs")
#==============================================
# Dates and time
library(lubridate)
STI1<-STI1%>%
  mutate(Today_date=dmy(Today_date),Interview_Date=dmy(Interview_Date))%>%
  mutate(AgeinDays=(Today_date-Interview_Date))
  
#===============================================
# Reordering columns
STI1<- STI1[, c("IdNumber","Interview_Date","Gender", "Today_date",
                "AgeinDays","Church","Weight","Occupation")]
#=================================================
# Import second data set called STI2
#=================================================
library(readxl)
STI2 <- read_excel("STI2.xlsx")
View(STI2)
#=================================================
# Combining tables
STI<-merge(STI1,STI2,by="IdNumber")
#DF<-left_join(STI1,STI2,by="IdNumber")
#DF_1<-right_join(STI1,STI2,by="IdNumber")
#DF_2<-inner_join(STI1,STI2,by="IdNumber")
#DF_3<-full_join(STI1,STI2,by="IdNumber")
#=================================================
# Renaming variable using rename function in dplyr package
STI<-STI%>%
  rename(InterviewDate=Interview_Date,
         Noofdays=AgeinDays)
#===================================================
# Creating new column using mutate()
STI<-STI%>%
  mutate(BMI=Weight/(Height/100)^2)
#---------------------------------------------
#Mutate Age categories from Age using within()
STI<-within(STI,{
  Agegroup<-NA
  Agegroup[Age>=16 & Age<=25]<-"16-25"
  Agegroup[Age>=26 & Age<=35]<-"26-35"
  Agegroup[Age>=36 & Age<=45]<-"36-45"
  Agegroup[Age>=46 & Age<=55]<-"46-55"
  Agegroup[Age>=56]<-"56+"
})
#===================================================
#Filtering variables using filter()

Dat_male<-STI%>%
  filter(Gender=="Male" & BMI<=18.49)
#======================================================
# Selecting the variables using select() in dplyr package
Dat_1<-STI%>%
  select(IdNumber,Gender,BMI)
#======================================================
# Grouping using group-by()
# Density plot
plot(density(STI$Height),xlab = "Height in CM",
     ylab = "Density",main = "Participant height")
#============================================
#Standardization
std<-STI%>%
  mutate(heig=Height-mean(Height))%>%
  mutate(he=heig/sd(heig))
summary(std$he)
hist(std$he) 
#====================
plot(density(std$he),xlab = "Height in CM",
     ylab = "Density",main = "Participant height")
#==============================================
