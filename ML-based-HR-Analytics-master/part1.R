library(ggplot2)
library(pROC)
library(corrplot)
library(caTools)
library(caret)
library(e1071)
library(rpart)
library(rattle)
library(randomForest)
library(gridExtra)
library(reshape)
library(rpart.plot)
library(ROCR)
library(dplyr)
library(tidyr)
library(magrittr)

DataSet=read.csv("C:/Users/SWATI/Desktop/projects+libraries/Minor project/HR_comma_sep.csv")
colnames(DataSet)[9] = "department"
head(DataSet)
str(DataSet)

round(prop.table(table(DataSet$left))*100)
##sapply(DataSet,function(x){round(prop.table(table(x))*100)})

Employee_left=subset(DataSet,DataSet$left==1)
print(mean(Employee_left$satisfaction_level))
print(median(Employee_left$satisfaction_level))
tr <- function(a){
            ggplot(data = Employee_left, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
    geom_density()
}
tr(Employee_left$satisfaction_level)

print(mean(Employee_left$last_evaluation))
print(median(Employee_left$last_evaluation))

tr(Employee_left$last_evaluation)

ggplot(subset(DataSet,left==1), aes(x = factor('Salary'), fill = factor(salary))) +
       geom_bar(width = 1, position = "fill", color = "black") + coord_polar(theta = "y")+theme_bw()+
labs(title="Salary")

ggplot(subset(DataSet,left==1), aes(time_spend_company))+
  geom_histogram(binwidth=0.5,fill='lightgreen')+
  labs(x="Time spent company", title="Time Spend in company")+
  theme_bw()

ggplot(subset(DataSet,left==1), aes(department))+
geom_bar(fill='lightgreen',width=0.5)+
labs(x="Department", title="Department")+
theme_bw()

round(prop.table(table(DataSet$department))*100)

left_dept=subset(DataSet,DataSet$left==1)
(table(left_dept$department))/(table(DataSet$department))

#SELECTING PREDICTIVE MODEL

# Spliting data into train and test set
# By using the sample.split() we are actually creating a vector with two values TRUE and FALSE. 
# By setting the SplitRatio to 0.7, we are splitting the original dataset to 70% training and 30% testing data.


set.seed(300)

DataSet$sp <- sample.split(DataSet$left, SplitRatio=0.7)
# where DataSet$sp== TRUE means to add only those rows that have value true for sp in the training dataset
train <- subset(DataSet, DataSet$sp==TRUE)
# where DataSet$sp== FALSE means to add only those rows that have value false for sp in the testing dataset
test <- subset(DataSet, DataSet$sp==FALSE)

# let us first start with logistic regression
# Train the model using the training sets and check score
model_glm <- glm(left ~ ., data = train, family='binomial')
# Predict Output of test data
predicted_glm <- predict(model_glm, test, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)
# Confusion matrix of Logistic regression
table(test$left, predicted_glm)
# Accuracy of model
mean(predicted_glm==test$left)


# Logistic regression is 77.55% accurate, which is fairly good
# But we would like to have more accuracy

# Let us try decision trees
model_dt <- rpart(left ~ ., data=train, method="class", minbucket=25)
# View decision tree plot
prp(model_dt)
# Predict Output of test data
predicted_dt <- predict(model_dt, test, type="class")
# Confusion matrix of decision tree
table(test$left, predicted_dt)
# Accuracy of decision tree
mean(predicted_dt==test$left)


# Decision tree has an astonishing accuracy of 97.06%
# Let's see if we can do better than this
# We shall do random forests with 200 trees
model_rf <- randomForest(as.factor(left) ~ ., data=train, nsize=20, ntree=200)
# Predict Output of test data
predicted_rf <- predict(model_rf, test)
# Confusion matrix of random forest
table(test$left, predicted_rf)
# Accuracy of random forest
mean(predicted_rf==test$left)


# Random forests increased the accuracy to 98.82%
# We shall see if Support vector machine performs as well as the above models
model_svm <- svm(left ~ ., data=train)
# Predict Output of test data
predicted_svm <- predict(model_svm, test)
predicted_svm <- ifelse(predicted_svm > 0.5,1,0)
# Confusion matrix of SVM
table(test$left, predicted_svm)
# Accuracy of SVM
mean(predicted_svm==test$left)


# SVM has accuracy of 96.46% which is good, but not the best.
# let us tune the parameter of the model to get better accuracy
model_svm <- svm(left ~ ., data=train, gamma=0.25, cost=10)
# Predict Output of test data
predicted_svm <- predict(model_svm, test)
predicted_svm <- ifelse(predicted_svm > 0.5,1,0)
# Confusion matrix of SVM
table(test$left, predicted_svm)
# Accuracy of SVM
mean(predicted_svm==test$left)


# Tuning the parameters increases the accuracy of SVM to 97.53%
# Let us plot the ROC curves for all the models
# Logistic regression
predict_glm_ROC <- predict(model_glm, test, type="response")
pred_glm <- prediction(predict_glm_ROC, test$left)
perf_glm <- performance(pred_glm, "tpr", "fpr")

# Decision tree
predict_dt_ROC <- predict(model_dt, test)
pred_dt <- prediction(predict_dt_ROC[,2], test$left)
perf_dt <- performance(pred_dt, "tpr", "fpr")

# Random forest
predict_rf_ROC <- predict(model_rf, test, type="prob")
pred_rf <- prediction(predict_rf_ROC[,2], test$left)
perf_rf <- performance(pred_rf, "tpr", "fpr")

# SVM
predict_svm_ROC <- predict(model_svm, test, type="response")
pred_svm <- prediction(predict_svm_ROC, test$left)
perf_svm <- performance(pred_svm, "tpr", "fpr")

# Area under the ROC curves
auc_glm <- performance(pred_glm,"auc")
auc_glm <- round(as.numeric(auc_glm@y.values),3)
auc_dt <- performance(pred_dt,"auc")
auc_dt <- round(as.numeric(auc_dt@y.values),3)
auc_rf <- performance(pred_rf,"auc")
auc_rf <- round(as.numeric(auc_rf@y.values),3)
auc_svm <- performance(pred_svm,"auc")
auc_svm <- round(as.numeric(auc_svm@y.values),3)
print(paste('AUC of Logistic Regression:',auc_glm))
print(paste('AUC of Decision Tree:',auc_dt))
print(paste('AUC of Random Forest:',auc_rf))
print(paste('AUC of Support Vector Machine:',auc_svm))


# Plotting the three curves
plot(perf_glm, main = "ROC curves for the models", col='blue')
plot(perf_dt,add=TRUE, col='red')
plot(perf_rf, add=TRUE, col='green3')
plot(perf_svm, add=TRUE, col='darkmagenta')
legend('bottom', c("Logistic Regression", "Decision Tree", "Random Forest", "Support Vector Machine"), fill = c('blue','red','green3','darkmagenta'), bty='n')


nrow(DataSet[as.numeric(DataSet$time_spend_company) >= 7,])
nrow(DataSet[DataSet$number_project == 7,])

# check summarization for those with 7 projects
summary(DataSet[DataSet$number_project == 7,])

# check summarization for those with more than 6 years experience
summary(DataSet[DataSet$time_spend_company >= 7,])

# Those people are being excluded because of the high ratio of 100% which seems like an anomaly.

DataSet <- DataSet %>% dplyr::filter(number_project <= 6, time_spend_company <= 6)


# prepare Data with correct variable types
DataSet$salary <- factor(DataSet$salary, levels = c("high", "low", "medium"), labels = c(1,2,3))

# generate readable code for sales factor
numberOfSalesDepartments <- length(DataSet$sales[duplicated(DataSet$sales) == FALSE])
salesLabels <- DataSet$sales[duplicated(DataSet$sales) == FALSE]
DataSet$sales <- factor(DataSet$sales, 
                           levels = salesLabels, 
                           labels = c(1:numberOfSalesDepartments))



# prepare Training and Test Set
set.seed(1234)
split <- sample.split(DataSet$left, SplitRatio = 0.8)
DataSetTraining <- subset(DataSet, split == TRUE)
DataSetTest <- subset(DataSet, split == FALSE)
# scale Data 
DataSetTraining[, c(1,2,4)] <- scale(DataSetTraining[, c(1,2,4)])
DataSetTest[, c(1,2,4)] <- scale(DataSetTest[, c(1,2,4)])


# write model backward elimination
StayOrLeaveModel <- glm(left ~ ., family = binomial(link = "logit"), DataSetTraining)
Summary(StayOrLeaveModel)
### the department does not have such a big impact thats 
# use all Variables and cut off each variable which does not have any real impact

StayOrLeaveModel <- glm(left ~ . - department, family = binomial(link = "logit"), DataSetTraining)
Summary(StayOrLeaveModel)


DataSetTest$prediction <- predict(StayOrLeaveModel, DataSetTest)
DataSetTest$prediction <- ifelse(DataSetTest$prediction > 0.5, 1, 0)
DataSetTest <- DataSetTest %>% arrange(desc(prediction))
# calculate confusion matrix / check
table(DataSetTest$left, DataSetTest$prediction)

# Filter Employees which are still in the Company to get the Employees who are on the risk of leaving
DataSetStayed <- DataSet[DataSet$left == 0,]
DataSetStayed[, c(1,2,4)] <- scale(DataSetStayed[,c(1,2,4)])
DataSetStayed$prediction <- predict(StayOrLeaveModel, DataSetStayed)
DataSetStayed <- DataSetStayed %>% arrange(desc(prediction))


head(DataSetStayed)
nrow(DataSetStayed[DataSetStayed$prediction >= 0.5,])

