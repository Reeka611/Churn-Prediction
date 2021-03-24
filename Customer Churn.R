library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(randomForest)
library(party)
library(tidyverse)
library(miscset)
library(RColorBrewer)
library(rattle)
library(rpart)
library(rpart.plot)

df<-read.csv("C:/Users/HAZARIKA/Desktop/Presentations/Customer Churn/Telco data.csv")
str(df)
summary(df)
names(df)
df <- df %>% mutate_if(is.character, as.factor)
df$SeniorCitizen <- as.factor(df$SeniorCitizen)
glimpse(df)
df %>% map(~ sum(is.na(.)))
df <- df %>%
  mutate(TotalCharges = replace(TotalCharges,
                                is.na(TotalCharges),
                                median(TotalCharges, na.rm = T)))
sum(is.na(df$TotalCharges))
datc$SeniorCitizen <- as.factor(mapvalues(datc$SeniorCitizen,
                                          from=c("0","1"),
                                          to=c("No", "Yes")))

Average<-aggregate(df[,20], list(df$Churn), mean)
plot(Average)

p1<-ggplot(df) + geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "dodge")
p2<-ggplot(df) + geom_bar(aes(x = gender, fill = Churn), position = "dodge")
p3<-ggplot(df) + geom_bar(aes(x = Partner, fill = Churn), position = "dodge")
p4<-ggplot(df) + geom_bar(aes(x = Dependents, fill = Churn), position = "dodge")
grid.arrange(p1, p2, p3, p4, ncol=2)
dependents <- df %>% filter(Dependents == "No")

ggplotGrid(ncol=2,
           lapply(c("PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup",
                    "DeviceProtection"),
                  function(col){
                    ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
                  }))
ggplotGrid(ncol=2,
           lapply(c("TechSupport","StreamingTV","StreamingMovies","Contract",
                    "PaperlessBilling"),
                  function(col){
                    ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
                  }))

ggplot(dependents) +
  geom_bar(aes(x=PaymentMethod,fill=Churn), position = "dodge")

# removing customerID; doesn't add any value to the model
df$customerID <- NULL

# train/test split; 75%/25%

# setting the seed for reproducibility
set.seed(5)
inTrain <- createDataPartition(y = df$Churn, p=0.75, list=FALSE)
train <- df[inTrain,]
str(train)
test <- df[-inTrain,]
str(test)
# fitting the model
fit <- glm(Churn~., data=train, family=binomial)
summary(fit)
fit1 <- glm(Churn~tenure+Contract+PaperlessBilling+TotalCharges+InternetService+MultipleLines+PaymentMethod, data=train, family=binomial)
summary(fit1)
#Feature Analysis:
#The top three most-relevant features include Contract, tenure and PaperlessBilling.
anova(fit, test="Chisq")
#Analyzing the deviance table we can see the drop in deviance when adding each variable one at a time. 
#Adding Internet Service, Contract and total Charges significantly reduces the residual deviance.
# making predictions
predict <- predict(fit, test,type="response")
predict <- ifelse(predict > 0.5,"Yes","No")
(conf<-table(predict,test$Churn))
TP<-conf[2, 2]
FN<-conf[1, 2]
FP<-conf[2, 1]
TN<-conf[1, 1]
print(conf)

acc<-(TP+TN)/(TP+FN+FP+TN)
prec<-TP/(TP+FP) #Pos Pred Value
rec<-TP/(TP+FN) #Sensitivity
spec<-TN/(TN+FP) #Specificity

confusionMatrix(as.factor(predict), test$Churn,positive = "Yes")

#true positives (TP): These are cases in which we predicted yes (they churned), and they did churn.TP=254
#true negatives (TN): We predicted no, and they didn't churn.TN=1167
#false positives (FP): We predicted yes, but they didn't actually churn. (Also known as a "Type I error.").FP=126
#false negatives (FN): We predicted no, but they actually churned. (Also known as a "Type II error.") FN=213
#The diagonal entries give our correct predictions, with the upper left being TN and the lower right being TP. 
#The upper right gives the FN while the lower left gives the FP.


#Decision Tree
#Decision Tree visualization
formula<-Churn~tenure+Contract+PaperlessBilling+InternetService+TotalCharges
dtree <- rpart(formula, data=train, method="class")
fancyRpartPlot(dtree)

#From this decision tree, we can interpret the following:
  
#The contract variable is the most important. Customers with month-to-month contracts are more likely to churn.
#Customers with DSL internet service are less likely to churn.
#Customers who have stayed longer than 15 months are less likely to churn.

#Decision Tree Confusion Matrix
#Using all the significant variables for confusion matrix table and make prediction.
tree <- rpart(Churn~., train,method="class")
pred_tree <- predict(tree, test,type="class")
confusionMatrix(as.factor(pred_tree), test$Churn,positive = "Yes")

#Random Forest
rfModel <- randomForest(Churn ~., data = train,importance=TRUE)
print(rfModel)
plot(rfModel)
#The black line represents the entire sample, the green line represents the error rate 
#where Y = 0 and the red line represents the error rate when Y = 1.
#We see that the negative outcomes have worse classification, 
#and the overall error rate is around 20%.
#As the number of trees increases, the error rate decreases, 
#and then becomes almost constant. We are not able to decrease the error rate 
#after about 100 trees.
pred_rf <- predict(rfModel, test,type="class")
confusionMatrix(pred_rf, test$Churn,positive = "Yes")
varImpPlot(rfModel)
#Similar to the decision tree, this random forest model has identified contract status and tenure length as important predictors for churn. 
#Internet service status does not appear as important in this model, and the total charges & Monthly charges variables are now highly emphasized.

#Data visualization based on models
#Our modeling efforts pointed to several important churn predictors: contract status, tenure length, and total charges. 
#Let's examine how these variables split by churn status.
p21 <- ggplot(df, aes(x = Contract, fill = Churn)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Churn rate by contract status")
p21
#the churn rate of month-to-month contract customers is much higher than the longer contract customers. 
#Customers who are more willing to commit to longer contracts are less likely to leave.

p23 <- ggplot(df, aes(x = tenure, fill = Churn)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Months",
       title = "Churn rate by tenure")
p23
#the length of time of the customer decreases the likelihood of churn. There is a large spike at 1 month, 
#indicating that there are a large portion of customers that will leave after just one month of service.

p24 <- ggplot(df, aes(x = TotalCharges, fill = Churn)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Total Charges",
       title = "Churn rate by Total Charges")
p24
#customers who have spent more with the company tend not to leave.

