# get the data

setwd("C:/Users/Radiet/Desktop/Autism Screening Adults")
getwd()

autismadults<-read.csv("Autism-Adult-Data - numerical order.csv", stringsAsFactors = F)



#exploring the data:- whole data exploration
str(autismadults)

#-------------------prepare data for SVM--------------------------

##remove outlier
autismadults <- autismadults[autismadults$age < 150,]

#delete missing values
autismadults<-na.omit(autismadults)

#drop unnecessary attributes relation, age description, used app before,result, 
autismadults<-autismadults[c(-20,-19,-18,-17)]

#change attributes to numeric for SVM except outcome variable which should be a factor
autismadults$gender <- as.numeric(autismadults$gender)
autismadults$ethnicity <- as.numeric(autismadults$ethnicity)
autismadults$jundice <- as.numeric(autismadults$jundice)
autismadults$family_autism <- as.numeric(autismadults$family_autism)
autismadults$contry_of_res <- as.numeric(autismadults$contry_of_res)
autismadults$Class.ASD <- factor(autismadults$Class.ASD)

# normalise the data - select one

autismadults.nm <- as.data.frame(autismadults)

# min-max scaling
autismadults.nm[1:16] <- as.data.frame(apply(autismadults[1:16], MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x))))
boxplot(autismadults.nm[-17], main = "Min-Max")


# randomise order of the data
set.seed(123)
autismadults_r <- autismadults.nm[order(runif(608)), ]

# create training (80%) and test data (20%) (Training from the first 80%)

autismadults_train <- autismadults_r[1:486, ]
autismadults_test <- autismadults_r[487:608, ]

# check the distribution of target variable
round(prop.table(table(autismadults$Class.ASD))*100, digits = 1)
round(prop.table(table(autismadults_train$Class.ASD))*100, digits = 1)
round(prop.table(table(autismadults_test$Class.ASD))*100, digits = 1)


library(MASS)
library(polycor)
library(kernlab)
library(caret)
library(lpSolve)
library(irr)

#try rbf kernel
autismadults_classifier_rbf <- ksvm(Class.ASD ~ ., data = autismadults_train,
                              kernel = "rbfdot")

autismadults_predictions_rbf <- predict(autismadults_classifier_rbf,
                                      autismadults_test)
head (autismadults_predictions_rbf)

table(autismadults_predictions_rbf, autismadults_test$Class.ASD)
autismadults.pred1 <- predict(autismadults_classifier_rbf, autismadults_test)
table(autismadults.pred1, autismadults_test$Class.ASD)
round(prop.table(table(autismadults.pred1, autismadults_test$Class.ASD))*100,1)

sum(diag(round(prop.table(table(autismadults.pred1, autismadults_test$Class.ASD))*100,1)))
head (autismadults_predictions_rbf)
#97.5 accuracy, 0fn, 3fp, kappa 0.9383
#confustion matrix

confusionMatrix(autismadults_predictions_rbf, autismadults_test$Class.ASD)


#...................................................................................

folds <- createFolds(autismadults$Class.ASD, k = 10)
cv_results <- lapply(folds, function(x) {
  autismadults_train_cv <- autismadults[-x, ]
  autismadults_test_cv <- autismadults[x, ]
  autismadults_model <- ksvm(Class.ASD ~ ., data = autismadults_train_cv)
  autismadults_pred <- predict(autismadults_model, autismadults_test_cv)
 autismadults_actual <- autismadults_test_cv$Class.ASD
  kappa <- kappa2(data.frame(autismadults_actual, autismadults_pred))$value
  return(kappa)
})
str(cv_results)
mean(unlist(cv_results))
# result mean kappa 0.9475389


