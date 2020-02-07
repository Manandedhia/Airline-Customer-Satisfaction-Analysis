install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("kernlab")
library(kernlab) 
library(arules)

setwd("D:/SEM 1/Intro to DS")

#Saving the csv file to a dataframe

SurveyData <- read.csv("satisfactionSurvey.csv")

#Analyse the dataset
summary(SurveyData)
str(SurveyData)

#table(SurveyData$Airline.Name)

#trim the dataset to remove unnecessary spaces
trimws(SurveyData$Satisfaction, which = c("both"))
trimws(SurveyData$Airline.Status, which = c("both"))
trimws(SurveyData$Airline.Code, which = c("both"))
trimws(SurveyData$Airline.Name, which = c("both"))
trimws(SurveyData$Age, which = c("both"))
trimws(SurveyData$Gender, which = c("both"))
trimws(SurveyData$Price.Sensitivity, which = c("both"))
trimws(SurveyData$Year.of.First.Flight, which = c("both"))
trimws(SurveyData$No.of.Flights.p.a., which = c("both"))
trimws(SurveyData$No..of.other.Loyalty.Cards, which = c("both"))
trimws(SurveyData$X..of.Flight.with.other.Airlines, which = c("both"))
trimws(SurveyData$Type.of.Travel, which = c("both"))
trimws(SurveyData$Shopping.Amount.at.Airport, which = c("both"))
trimws(SurveyData$Eating.and.Drinking.at.Airport, which = c("both"))
trimws(SurveyData$Class, which = c("both"))
trimws(SurveyData$Day.of.Month, which = c("both"))
trimws(SurveyData$Flight.Distance, which = c("both"))
trimws(SurveyData$Flight.date, which = c("both"))
trimws(SurveyData$Flight.cancelled, which = c("both"))
trimws( SurveyData$Flight.time.in.minutes,which = c("both"))
trimws(SurveyData$Orgin.City, which = c("both"))
trimws(SurveyData$Origin.State,which = c("both"))
trimws(SurveyData$Destination.City, which = c("both"))
trimws(SurveyData$Destination.State, which = c("both"))
trimws(SurveyData$Scheduled.Departure.Hour, which = c("both"))
trimws(SurveyData$Arrival.Delay.in.Minutes, which = c("both"))
trimws(SurveyData$Arrival.Delay.greater.5.Mins, which = c("both"))



#Change the column names
colnames(SurveyData)[colnames(SurveyData)=="Airline.Status"] <- "Airline_Status"
colnames(SurveyData)[colnames(SurveyData)=="Price.Sensitivity"] <- "Price_Sensitivity"
colnames(SurveyData)[colnames(SurveyData)=="Year.of.First.Flight"] <- "Year_of_First_Flight"
colnames(SurveyData)[colnames(SurveyData)=="No.of.Flights.p.a."] <- "No_of_Flights_per_annum"
colnames(SurveyData)[colnames(SurveyData)=="X..of.Flight.with.other.Airlines"] <- "Percent_of_Flight_with_other_Airlines"
colnames(SurveyData)[colnames(SurveyData)=="Type.of.Travel"] <- "Type_of_Travel"
colnames(SurveyData)[colnames(SurveyData)=="No..of.other.Loyalty.Cards"] <- "No_of_other_Loyalty_Cards"
colnames(SurveyData)[colnames(SurveyData)=="Shopping.Amount.at.Airport"] <- "Shopping_Amount_at_Airport"
colnames(SurveyData)[colnames(SurveyData)=="Eating.and.Drinking.at.Airport"] <- "Eating_and_Drinking_at_Airport"
colnames(SurveyData)[colnames(SurveyData)=="Day.of.Month"] <- "Day_of_Month"
colnames(SurveyData)[colnames(SurveyData)=="Flight.date"] <- "Flight_date"
colnames(SurveyData)[colnames(SurveyData)=="Airline.Code"] <- "Airline_Code"
colnames(SurveyData)[colnames(SurveyData)=="Airline.Name"] <- "Airline_Name"
colnames(SurveyData)[colnames(SurveyData)=="Origin.City"] <- "Origin_City"
colnames(SurveyData)[colnames(SurveyData)=="Origin.State"] <- "Origin_State"
colnames(SurveyData)[colnames(SurveyData)=="Destination.City"] <- "Destination_City"
colnames(SurveyData)[colnames(SurveyData)=="Destination.State"] <- "Destination_State"
colnames(SurveyData)[colnames(SurveyData)=="Scheduled.Departure.Hour"] <- "Scheduled_Departure_Hour"
colnames(SurveyData)[colnames(SurveyData)=="Departure.Delay.in.Minutes"] <- "Departure_Delay_in_Minutes"
colnames(SurveyData)[colnames(SurveyData)=="Arrival.Delay.in.Minutes"] <- "Arrival_Delay_in_Minutes"
colnames(SurveyData)[colnames(SurveyData)=="Flight.cancelled"] <- "Flight_cancelled"
colnames(SurveyData)[colnames(SurveyData)=="Flight.time.in.minutes"] <- "Flight_time_in_minutes"
colnames(SurveyData)[colnames(SurveyData)=="Flight.Distance"] <- "Flight_Distance"
colnames(SurveyData)[colnames(SurveyData)=="Arrival.Delay.greater.5.Mins"] <- "Arrival_Delay_greater_5_Mins"


#Converting the NAs in the three columns to their mean values
SurveyData$Flight_time_in_minutes[is.na(SurveyData$Flight_time_in_minutes)] <- round(mean(SurveyData$Flight_time_in_minutes, na.rm = TRUE))
SurveyData$Arrival_Delay_in_Minutes[is.na(SurveyData$Arrival_Delay_in_Minutes)] <- round(mean(SurveyData$Arrival_Delay_in_Minutes, na.rm = TRUE))
SurveyData$Departure_Delay_in_Minutes[is.na(SurveyData$Departure_Delay_in_Minutes)] <- round(mean(SurveyData$Departure_Delay_in_Minutes, na.rm = TRUE))
summary(SurveyData)

# Converting the data types to numeric.

convertData <- function(SurveyData){
  for (i in 1:28){
    SurveyData[,i] <- as.numeric(SurveyData[,i])
    
    
  }
  return(SurveyData)
}

#Verify
str(SurveyData)


res <- SurveyData %>% group_by(Age) %>% summarise(Freq=n(), percLessThan3= sum(Satisfaction<3, na.rm = TRUE))
res$perc <- (res$percLessThan3/res$Freq)*100

res_Airline <- SurveyData %>% group_by(Airline_Name) %>% summarise(Freq=n(), percLessThan3= sum(Satisfaction<3, na.rm = TRUE))
res_Airline$perc <- (res_Airline$percLessThan3/res_Airline$Freq)*100


ggplot(res_Airline, aes(x=Airline_Name ,y = Freq, fill=perc))+geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(res, aes(x=Age ,y = Freq, fill=perc))+geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Data Set for the FlyFast Airways Inc
FlyfastData <- filter(SurveyData, Airline_Name == "FlyFast Airways Inc. ")

FlyfastData$Satisfaction <- as.numeric(FlyfastData$Satisfaction)

FlyfastData <- convertData(FlyfastData)


#LINEAR MODELLING

model <- lm(formula = Satisfaction ~., data = FlyfastData)
summary(model)
# Multiple R-squared:  0.4341
# Adjusted R-squared:  0.4332 


model1 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model1)
#Multiple R-squared:  0.4327, Adjusted R-squared:  0.4324



#without Arrival_Delay_greater_5_Mins
model12 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled, data = FlyfastData)
summary(model12)
#Multiple R-squared:  0.4052
#Adjusted R-squared:  0.4049

#without Flight_cancelled
model13 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour + Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model13)
#Multiple R-squared:  0.4317
#Adjusted R-squared:  0.4314

# without Scheduled_Departure_Hour
model14 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model14)
#Multiple R-squared:  0.4326
#Adjusted R-squared:  0.4323

#without Type_of_Travel
model15 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum +Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model15)
#Multiple R-squared:  0.2289
#Adjusted R-squared:  0.2285

# without No_of_Flights_per_annum
model16 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight + Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model16)
#Multiple R-squared:  0.4304
#Adjusted R-squared:  0.4301

#without Year_of_First_Flight
model17 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model17)
#Multiple R-squared:  0.4324
#Adjusted R-squared:  0.4321

#without Gender
model18 <- lm(formula = Satisfaction ~Airline_Status+Age +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model18)
#Multiple R-squared:  0.4286
#Adjusted R-squared:  0.4283

#without Age
model19 <- lm(formula = Satisfaction ~Airline_Status+Gender +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model19)
#Multiple R-squared:  0.4311
#Adjusted R-squared:  0.4308

#without Airline_Status
model20 <- lm(formula = Satisfaction ~Gender +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = FlyfastData)
summary(model20)
#Multiple R-squared:  0.371
#Adjusted R-squared:  0.3707

# 
# #Airline_Status +Gender + No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins + Age
# 

#associationrules


createBucketforSat <- function(vec){ 
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 3] <- "High"
  vBuckets[vec < 3] <- "Low"
  return(vBuckets)
}

createBuckets <- function(vec){
  q <- quantile(vec, c(0.3, 0.6), na.rm = "TRUE")
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

createBucketsforPrice <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v > 1] <- "High"
  vBucket[v < 1] <- "Low"
  return(vBucket)
}

createBucketsforFlightsPA <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>24] <- "High"
  vBucket[v<24] <- "Low"
  return(vBucket)
}

CreateBucketsforFlightsWOA <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>6] <- "High"
  vBucket[v<6] <- "Low"
  return(vBucket)
}

newData <- FlyfastData
newData$Year_of_First_Flight <- NULL
newData$Day_of_Month <- NULL
newData$Scheduled_Departure_Hour <- NULL

newData$Satisfaction <- createBucketSat(newData$Satisfaction) #converting satisfaction into Low, Average and High
newData$Satisfaction <- as.factor(newData$Satisfaction) 

newData$Price_Sensitivity <- createBucketsforPrice(newData$Price_Sensitivity) #converting Price Sensitivity into Low, Average and High
newData$Price_Sensitivity <- as.factor(newData$Price_Sensitivity)

newData$Age <- createBuckets(newData$Age) #converting Age into Low, Average and High
newData$Age <- as.factor(newData$Age)

newData$No_of_other_Loyalty_Cards <- createBuckets(newData$No_of_other_Loyalty_Cards) #converting Loyalty cards into Low, Average and High
newData$No_of_other_Loyalty_Cards <- as.factor(newData$No_of_other_Loyalty_Cards)

newData$Shopping_Amount_at_Airport <- createBuckets(newData$Shopping_Amount_at_Airport) #converting shopping amount at airport into Low, Average and High
newData$Shopping_Amount_at_Airport <- as.factor(newData$Shopping_Amount_at_Airport)

newData$No_of_Flights_per_annum <- createBucketsforFlightsPA(newData$No_of_Flights_per_annum) #converting no of flights per annum into Low, Average and High
newData$No_of_Flights_per_annum <- as.factor(newData$No_of_Flights_per_annum)

newData$Percent_of_Flight_with_other_Airlines <- CreateBucketsforFlightsWOA(newData$Percent_of_Flight_with_other_Airlines) #converting percent of flight with other airlines into Low, Average and High
newData$Percent_of_Flight_with_other_Airlines <- as.factor(newData$Percent_of_Flight_with_other_Airlines)

newData$Eating_and_Drinking_at_Airport <- createBuckets(newData$Eating_and_Drinking_at_Airport)
newData$Eating_and_Drinking_at_Airport <- as.factor(newData$Eating_and_Drinking_at_Airport)

newData$Departure_Delay_in_Minutes <- createBuckets(newData$Departure_Delay_in_Minutes)
newData$Departure_Delay_in_Minutes <- as.factor(newData$Departure_Delay_in_Minutes)

newData$Arrival_Delay_in_Minutes <- createBuckets(newData$Arrival_Delay_in_Minutes)
newData$Arrival_Delay_in_Minutes <- as.factor(newData$Arrival_Delay_in_Minutes)

newData$Flight_time_in_minutes <- createBuckets(newData$Flight_time_in_minutes)
newData$Flight_time_in_minutes <- as.factor(newData$Flight_time_in_minutes)

newData$Flight_Distance <- createBuckets(newData$Flight_Distance)
newData$Flight_Distance <- as.factor(newData$Flight_Distance)

View(newData)

newData$Airline_Status<-as.factor(newData$Airline_Status)
newData$Gender<-as.factor(newData$Gender)
newData$Type_of_Travel<-as.factor(newData$Type_of_Travel)
newData$Class<-as.factor(newData$Class)
newData$Flight_date<-as.factor(newData$Flight_date)
newData$Airline_Code<-as.factor(newData$Airline_Code)
newData$Airline_Name<-as.factor(newData$Airline_Name)
newData$Origin_State<-as.factor(newData$Origin_State)
newData$Orgin.City<-as.factor(newData$Orgin.City)
newData$Destination_City<-as.factor(newData$Destination_City)
newData$Destination_State<-as.factor(newData$Destination_State)
newData$Flight_cancelled<-as.factor(newData$Flight_cancelled)
newData$Arrival_Delay_greater_5_Mins<-as.factor(newData$Arrival_Delay_greater_5_Mins)

#APRIORI ALGORITHM

rule <- apriori(newData, parameter = list(support = 0.1, confidence = 0.5), appearance = list(rhs=c("Satisfaction=Low")))
inspect(rule)
summary(rule)

#SUPPORTVECTORMACHINE

satisfactionData <- read.csv("D:/SEM 1/Intro to DS/satisfactionSurvey.csv")
satdf <-  data.frame(satisfactionData)
View(satdf)

satdf$satisfied <- NA
satisfied <- replicate(length(satdf$Satisfaction), "TRUE")
satisfied[as.numeric(as.character(satdf$Satisfaction)) >= 3] <- "TRUE"
satisfied[as.numeric(as.character(satdf$Satisfaction)) < 3] <- "FALSE"
satdf$satisfied <- satisfied

twothird <- floor(2 * dim(satdf)[1]/3) 
twothird
randIndex <- sample(1:dim(satdf)[1])
TrainData <- satdf[randIndex[1:twothird],]
View(TrainData)

TestData <- satdf[randIndex[(twothird+1):dim(satdf)[1]],]
View(TestData)

dim(TrainData)
dim(TestData)


svmOutput <- ksvm(satisfied ~ Airline.Status+Gender+Type.of.Travel+Flight.cancelled, data=TrainData,
                  kernel = "rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)


svmOutput

OUTPUT:
  > svmOutput <- ksvm(satisfied ~ Airline.Status+Gender+Type.of.Travel+Flight.cancelled, data=TrainData,
                      +                   kernel = "rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
> svmOutput
Support Vector Machine object of class "ksvm" 

SV type: C-svc  (classification) 
parameter : cost C = 5 

Gaussian Radial Basis kernel function. 
Hyperparameter : sigma =  0.625 

Number of Support Vectors : 25992 

Objective Function Value : -129825.9 
Training error : 0.149913 
Cross validation error : 0.150029 
Probability model included.


svmPred <- predict(svmOutput, TestData, type = "votes")
View(svmPred)

str(svmPred)
head(svmPred)



