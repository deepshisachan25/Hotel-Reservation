
datafilelocation = 'C:\\Users\\deeps\\OneDrive\\Documents\\WEBSTER\\Analytics programming with R\\Notes\\Hotel Reservation dataset\\Hotel_Reservations.csv'

Hotel_Reservations <- read.csv(datafilelocation, header = TRUE, sep = ",")
Hotel_Reservations

dim(Hotel_Reservations)
library(tidyverse)
class(Hotel_Reservations)
names(Hotel_Reservations)
Hotel_Reservations <- data.frame(Hotel_Reservations)
head(Hotel_Reservations)

tail(Hotel_Reservations)

library(dplyr)
##To remove Booking ID variable.
Hotel_Reservations <- select(Hotel_Reservations,c(-1))

library(ggplot2)
library(dplyr)
library(magrittr)
library(purrr)

# We can calculate how many NAs there are in each variable by using the map() in the purrr package
Hotel_Reservations %>% 
  map(is.na) %>%
  map(sum)

# Using ggplot() geom_histogram to graph quantitative variables

ggplot(data = Hotel_Reservations)+
  geom_histogram(mapping = aes(x = no_of_adults), binwidth = 0.5)

ggplot(data = Hotel_Reservations)+
  geom_histogram(mapping = aes(x = no_of_weekend_nights), binwidth = 0.5)

ggplot(data = Hotel_Reservations)+
  geom_histogram(mapping = aes(x = lead_time), binwidth = 0.5)

ggplot(data = Hotel_Reservations)+
  geom_histogram(mapping = aes(x = repeated_guest), binwidth = .5)

ggplot(data = Hotel_Reservations)+
  geom_bar(mapping = aes(x = no_of_special_requests))

ggplot(data = Hotel_Reservations)+
  geom_bar(mapping = aes(x = booking_status))

library(lessR)
BarChart(no_of_children,data=Hotel_Reservations)
BarChart(no_of_weekend_nights,data=Hotel_Reservations)
BarChart(no_of_week_nights,data=Hotel_Reservations)
BarChart(type_of_meal_plan,data=Hotel_Reservations)
BarChart(required_car_parking_space,data=Hotel_Reservations)
BarChart(room_type_reserved,data=Hotel_Reservations)
Histogram(lead_time,data=Hotel_Reservations)
BarChart(arrival_year,data=Hotel_Reservations)
BarChart(arrival_month,data=Hotel_Reservations)
BarChart(arrival_date,data=Hotel_Reservations)
BarChart(market_segment_type,data=Hotel_Reservations) #market segment designation from where hotel was booked.
BarChart(repeated_guest,data=Hotel_Reservations)
BarChart(no_of_previous_cancellations,data=Hotel_Reservations)
Histogram(no_of_previous_bookings_not_canceled,data=Hotel_Reservations)
Histogram(avg_price_per_room,data=Hotel_Reservations)
BarChart(no_of_special_requests,data=Hotel_Reservations)
BarChart(booking_status,data=Hotel_Reservations)

######*************Code For EDA***************************
ggplot(data=Hotel_Reservations, aes(x=lead_time, y=1,fill=lead_time))+
  geom_bar(stat="identity") + facet_wrap(~booking_status)

ggplot(data=Hotel_Reservations, aes(x=avg_price_per_room, y=1,fill=avg_price_per_room))+
  geom_bar(stat="identity") + facet_wrap(~booking_status)

ggplot(Hotel_Reservations,aes(x=lead_time,y=avg_price_per_room,col=booking_status))+geom_point()

BarChart(no_of_special_requests,data=Hotel_Reservations,by=booking_status)
BarChart(avg_price_per_room,data=Hotel_Reservations,by=booking_status)

##outliers
ggplot(data=Hotel_Reservations, mapping=aes(x=lead_time))+geom_boxplot() ###in days

ggplot(data=Hotel_Reservations, mapping=aes(x=avg_price_per_room))+geom_boxplot() ##in dollars

##***** Coverting booking status variable to numeric
Hotel_bookingstatus<-if_else(Hotel_Reservations$booking_status=="Canceled",1,0 )
Hotel_Reservations$booking_status<- Hotel_bookingstatus
print(Hotel_bookingstatus)

#******** correlation matrix ********************************
library(corrplot)

# Compute the correlation coefficients between two variables using the cor() function
# Correlation coefficents measure the  the level of the association between two variables x and y. 
# Its value ranges between -1 (perfect negative correlation: when x increases, y decreases) 
# and +1 (perfect positive correlation: when x increases, y increases).
# A value closer to 0 suggests a weak relationship between the variables.  
# A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the outcome variable (y) 
# is not explained by the predictor (x) so we should probably look for better predictor variables.

##We can see that we don't have strong correlation between the variables.
# Another test that we can use to understand the strength of the relationships betwen variables is to use a correlation plot.
# Correlation plots show the relationship of all the variables in the data set an are plotted against each other
# The closer a correlation is "1" the stronger the relationship
# Since our outcome variable is not numeric, we can change it to a numeric 1 or 0 to use in the correlation plot to see the correlations 

quantVars <- unlist(lapply(Hotel_Reservations, is.numeric))  
Hotelreservations.values <- Hotel_Reservations[ , quantVars]

Hotelreservation.Corr <- cor(Hotelreservations.values)

corrplot(Hotelreservation.Corr, method="number")

##**********************************************************************

# eliminate some outliers
leadtime <- Hotel_Reservations[Hotel_Reservations$lead_time < 300, ]
head(Hotel_Reservations)

##***************************************
### canceled = 1, not_canceled=0
summary(Hotel_Reservations)

### ***** Linear Regression MODEL******#
###make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set

sample <- sample(c(TRUE, FALSE), nrow(Hotel_Reservations), replace=TRUE, prob=c(0.7,0.3))
train <- Hotel_Reservations[sample, ]
test <- Hotel_Reservations[!sample, ]  

Modelformula= booking_status ~ no_of_weekend_nights+required_car_parking_space+lead_time+ arrival_month+ avg_price_per_room +no_of_special_requests

#fit logistic regression model
model<- glm(Modelformula,  family="binomial", data=train)
summary(model)

# visualize the model
library(coefplot)
coefplot(model)
summary(model)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model)

install.packages("caret")
library(caret)
Variable_Importance<-caret::varImp(model)
Variable_Importance$variablename=rownames(Variable_Importance)
class(Variable_Importance)
VariableImportance= Variable_Importance[order(Variable_Importance$Overall, decreasing = TRUE), ]   
library(ggplot2)

##To visualize important variable

library(lessR)
ScatterPlot(VariableImportance, x =Overall ,y= variablename)

ggplot(data=VariableImportance, aes(x=Overall, y=reorder(variablename,Overall),fill=Overall))+
  geom_bar(stat="identity") + ggtitle("Important_Variable")

## Here, we can see lead_time is more predictor variable followed by no_of special request, avg price.

###Higher values indicate more importance. These results match up nicely with the p-values from the model.
#Lead_time is by far the most important predictor variable, followed by no_of_special_request, average price per room, arrival_month and then no_of_weekend_nights.

car::vif(model)

#As a rule of thumb, VIF values above 5 indicate severe multicollinearity.
#Since none of the  predictor variables in our models have a VIF over 5, we can assume that multicollinearity is not an issue in our model.

##***********Once we’ve fit the logistic regression model, we can then use it to make predictions about whether or not an individual will cancel reservation based on their lead_time, special_request, avg_price_per_room,

newdata <- data.frame(no_of_weekend_nights=1 ,required_car_parking_space =0,lead_time = 250, arrival_month = 10,  no_of_special_requests=1, 
                     repeated_guest =1, avg_price_per_room = 200)

## The probability of booking got canceled with a lead time of 250days, arrival month 10, 1 special request, avg price of 200$ per room
#predict probability of canceling. 
predict(model, newdata, type="response")
predicted <- predict(model, test, type="response")

predicted1<- ifelse(predicted>0.51, 1, 0)
predicted<- ifelse(predicted>0.51, 1, 0)

head(predicted)
tail(predicted)

## Determine accuracy of model.

predicted1=as.factor(predicted1)
test$booking_status=as.factor(test$booking_status)
confusionMatrix(test$booking_status, predicted1)

### tried different value . 0.51 is the probability of getting 1 cancelation with highest accuracy.

cf <- caret::confusionMatrix(data=predicted1,
                             reference=test$booking_status)


###A confusion matrix in R is a table that will categorize the predictions against the actual values.
##It includes two dimensions, among them one will indicate the predicted values and another one will represent the actual values.

fourfoldplot(as.table(cf),color=c("red", "green"),main = "Confusion Matrix")

###A confusion matrix in R will be the key aspect of classification data problems. 
#Try to apply all these above-illustrated techniques to your preferred dataset and observe the results.


##*************************ROC CURVE

library(pROC)
roc_score=roc(test[,18], predicted) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")


