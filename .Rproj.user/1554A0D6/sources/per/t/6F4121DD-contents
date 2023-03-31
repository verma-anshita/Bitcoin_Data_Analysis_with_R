#install.packages("tidyverse")
#install.packages('caret', dependencies=TRUE)

##Packages
library(tidyverse)
library(caret)

#####################################################

mydata<-read.csv("Bitcoindata.csv")

#Understanding Data
head(mydata)

#checking data types and details
summary(mydata)

#checking if null are there
sum(is.na(mydata)) #contains NO NA values

#checking Shape, Rows and Coulumns of dataset
dim(mydata)

#######################################################
# Analysing Bitcoin Data
ggplot(data = bitcoin,mapping = aes(x = Date, y = Close))+geom_point(size=.5)
#######################################################

#Taking a subset of this data
bitcoin <- mydata[c(1:6000),c(1:8)]

dim(bitcoin)

# steps -add new column with next days closing value, then use this for predicting trend 
#of going up or down; multinomial prediction, use some graphs or some analysis

#writing loop to create new price column
for(i in 1:nrow(bitcoin)){
  bitcoin$NewPrice[i]<- bitcoin$Open[i+1] #next rows value to determine trend
}

head(bitcoin)
tail(bitcoin)

#checking if null are there
sum(is.na(bitcoin))

#Replacing NA by zero
bitcoin[is.na(bitcoin)] = 0
#checking if null are there
sum(is.na(bitcoin))

#adding column for trend which will used for predicting future trends
for(i in 1:nrow(bitcoin)){
  if (bitcoin$NewPrice[i] > bitcoin$Close[i]){
    bitcoin$Trend[i] <- 1            # 1 showing upward trend
  } else {
    bitcoin$Trend[i] <- 0             # 0 showing downward trend
  } 
}

head(bitcoin)
##########################################################

# Splitting into Train and Test Data
set.seed(1234)
indexSet<-sample(2,nrow(bitcoin),replace=T,prob = c(0.8,0.2))
train<-bitcoin[indexSet==1,]
test<-bitcoin[indexSet==2,]

##########################################################

#Fitting the model Using Logistic Regression
#help(glm)
model= glm(Trend~Open+High+Low+Close, data = train,family = binomial)
summary(model)

##########################################################
#Fitting the model Using Logistic Regression removing Low Column as p-value for Low is high>0.05

model= glm(Trend~Open+High+Close, data = train,family = binomial)
summary(model)
print(model)

##########################################################
#Prdeicting output values
predictTest = predict(model, newdata = test, 
                      type = "response")
predicted_classes <- as.factor(ifelse(predictTest >= 0.5, 
                                      1, 0))
predicted_classes

##########################################################
#Performance Accuracy

accuracy<-mean(head(predicted_classes,5500)==head(bitcoin$Trend,5500))
accuracy

##########################################################

predictedvalue<-predict.glm(model,newdata=data.frame(Open=55690.64,High=55822.91
,Close=55803.50),type = "response")
output<-ifelse(predictedvalue> 0.5, 1, 0)
output

predictedvalue<-predict.glm(model,newdata=data.frame(Open=	
                                                       55816.61,High=55835.57
                                                     ,Close=	
                                                       55779.38),type = "response")
output<-ifelse(predictedvalue> 0.5, 1, 0)
output

predictedvalue<-predict.glm(model,newdata=data.frame(Open=	
                                                       5586.61,High=5585.57
                                                     ,Close=	
                                                       5579.38),type = "response")
output<-ifelse(predictedvalue> 0.5, "Upward", "Downward")
output


##########################################################
