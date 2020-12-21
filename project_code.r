#-------------------DATA INGESTION AND SUMMARY STATISTICS-------------------------------
cleveland.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(cleveland.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                            "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
summary(cleveland.data)
str(cleveland.data)
#-----------------------------LOAD PACKAGES---------------------------------
install.packages("corrplot")
install.packages("tidyverse")
install.packages("ggcorrplot")
install.packages("caret")
library('corrplot')
library('tidyverse')
library('ggcorrplot')
library('ggplot2')
library('plyr')
library('caret')
#-----------------------------DATA CLEAN UP---------------------------------
#Any value for "num" greater than zero indicates the presence of heart disease.
cleveland.data$num[cleveland.data$num>0]<-1
cleveland.data$num
head(cleveland.data)
sum(is.na(cleveland.data))
cleveland.data <- na.omit(cleveland.data)
#str(heart.data)
#-----------------------------CORRELATION ANALYSIS--------------------------
# Displaying the coralation matrix
corr <- cor(cleveland.data)
# Visualize the correlation matrix
corrplot(corr)
corr<-cor(cleveland.data)
head(corr)
head(cleveland.data)
names(cleveland.data)
cleveland.data<-cleveland.data[]
#According to the Correlation matrix, trestbps, chol, fbs and restecg are the least correlated variables with
#num, the variable we would like to predict. Therefore, we will drop these from our analysis.

#----------------------------CHECK REGRESSION ASSUMPTIONS----------------------
#First, we check to make sure the x-variables are normally distributed
hist(cleveland.data$age) #normally distributed
hist(cleveland.data$sex) #1 for male, 0 for female
hist(cleveland.data$cp) #1: typical angina, 2: atypical angina, 3: non-anginal pain, 4: asymptomatic
hist(cleveland.data$thalach) #Slightly left-skewed (negatively skewed) but approximately normal
hist(cleveland.data$exang) #Exercise induced angina (1 = yes; 0 = no)
hist(cleveland.data$oldpeak) # Heavily right-skewed. ST depression induced by exercise relative to rest
hist(cleveland.data$slope) # The slope of the peak exercise ST segment (1: upsloping,2: flat,3: downsloping)

#---------------------------CREATE DUMMY VARIABLES---------------------------
#Create dummy variables for CP:
cleveland.data$cp.typAng<-ifelse(cleveland.data$cp==1,1,0)
cleveland.data$cp.atypAng<-ifelse(cleveland.data$cp==2,1,0)
cleveland.data$cp.nonAng<-ifelse(cleveland.data$cp==3,1,0)
cleveland.data$cp.asymp<-ifelse(cleveland.data$cp==4,1,0)

#Create dummy variables for Slope:
cleveland.data$slope.Up<-ifelse(cleveland.data$slope==1,1,0)
cleveland.data$slope.Flat<-ifelse(cleveland.data$slope==2,1,0)
cleveland.data$slope.Down<-ifelse(cleveland.data$slope==3,1,0)
length(cleveland.data$slope.Down)

#--------------------------ELIMATE VARIABLES--------------------------------
cleveland.data = subset(cleveland.data, select = c(-cp,-slope,-trestbps,-chol,-fbs,-restecg,-cp.asymp,-slope.Down))
cleveland.data=subset(cleveland.data, select = c(-cp.asymp,-slope.Down))
names(cleveland.data)

#----------------------------MULTIPLE LINEAR REGRESSION----------------------

logistic <- glm(num ~ ., data=cleveland.data, family="binomial")
summary(logistic)
