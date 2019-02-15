# read from numeric coded varaible

setwd("C:/Users/Radiet/Desktop/Autism Screening Adults")
getwd()

autismadults<-read.csv("Autism-Adult-Data - numerical order.csv", stringsAsFactors = F)

str(autismadults)

#Clear noise
autismadults <- autismadults[autismadults$age < 150,]

#delete missing values
autismadults<-na.omit(autismadults)

#drop unnecessary attributes 20=relation,19=age description, 17=used app before,18=result
autismadults<-autismadults[c(-20,-19,-18,-17)]
str(autismadults)

#change attributes to numeric for KNN
autismadults$A1_Score <- as.numeric(autismadults$A1_Score)
autismadults$A2_Score <- as.numeric(autismadults$A2_Score)
autismadults$A3_Score <- as.numeric(autismadults$A3_Score)
autismadults$A4_Score <- as.numeric(autismadults$A4_Score)
autismadults$A5_Score <- as.numeric(autismadults$A5_Score)
autismadults$A6_Score <- as.numeric(autismadults$A6_Score)
autismadults$A7_Score <- as.numeric(autismadults$A7_Score)
autismadults$A8_Score <- as.numeric(autismadults$A8_Score)
autismadults$A9_Score <- as.numeric(autismadults$A9_Score)
autismadults$A10_Score <- as.numeric(autismadults$A10_Score)
autismadults$gender <- as.numeric(autismadults$gender)
autismadults$ethnicity <- as.numeric(autismadults$ethnicity)
autismadults$jundice <- as.numeric(autismadults$jundice)
autismadults$family_autism <- as.numeric(autismadults$family_autism)
autismadults$age <- as.numeric(autismadults$age)
autismadults$contry_of_res <- as.numeric(autismadults$contry_of_res)

str(autismadults)

#change outcome variable to numeric
autismadults$Class.ASD <- factor(autismadults$Class.ASD, levels = c("NO", "YES"),
                         labels = c("NOT ASD", "ASD"))

# table or proportions with more informative labels
round(prop.table(table(autismadults$Class.ASD)) * 100, digits = 1)

summary(autismadults)
boxplot(autismadults[-17], main = "Box plot 1")

#see significance of variables
library(polycor)
autismadults.cor <- hetcor(autismadults)
autismadults.cor$type
print(autismadults.cor$correlations, digits = 2)

# randomise order of the data
set.seed(12345)
autismadults <- autismadults[order(runif(608)), ]

#normalise  numeric variable 
autismadults_mm <- as.data.frame(apply(autismadults[1:16], MARGIN = 2, FUN = function(x)
  (x - min(x))/diff(range(x))))


summary(autismadults_mm, main ="MinMax")

autismadults_3var<-autismadults_mm[c(11,13,16)]

boxplot(autismadults_mm[-17],main = "MinMax")
boxplot(autismadults_3var, main = "MinMax Excluding Binary Variables")


# create training (80%) and test data (20%) (Training from the first 80%)

autismadults_train <- autismadults_mm[1:486, ]
autismadults_test <- autismadults_mm[487:608, ]

# create labels (from 16th column) for training and test data
autismadults_train_labels <- autismadults[1:486, 17]
autismadults_test_labels <- autismadults[487:608, 17]


#check distribution of train and test labels
table(autismadults_train_labels)
trainper<-prop.table(table(autismadults_train_labels))*100
round(trainper,digits = 1)

table(autismadults_test_labels)
testper<-prop.table(table(autismadults_test_labels))*100
round(testper,digits = 1)

#...... create training (80%) and test data (20%) (Training from the last 80%)

autismadults_train <- autismadults_mm[121:608, ]
autismadults_test <- autismadults_mm[1:120, ]

# create labels (from 17th column) for training and test data
autismadults_train_labels <- autismadults[121:608, 17]
autismadults_test_labels <- autismadults[1:120, 17]

#check distribution of train and test labels
table(autismadults_train_labels)
trainper<-prop.table(table(autismadults_train_labels))*100
round(trainper,digits = 1)

table(autismadults_test_labels)
testper<-prop.table(table(autismadults_test_labels))*100
round(testper,digits = 1)


#..... Go ahead with the first (80%) for training set (data in random order)

autismadults_train <- autismadults_mm[1:486, ]
autismadults_test <- autismadults_mm[487:608, ]

# create labels (from 17th column) for training and test data
autismadults_train_labels <- autismadults[1:486, 17]
autismadults_test_labels <- autismadults[487:608, 17]

# training the model on the data

library(class)

# try k=25 as sqrt of 608
autismadults_test_pred <- knn(train = autismadults_train, 
                              test = autismadults_test, 
                              cl = autismadults_train_labels, k=25)

#inspect results 1=not asd, 2= asd
autismadults_test_pred
table(autismadults_test_pred)


# evaluating model performance

library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)# Inspect FP (1) and FN (2)
 
# Improve model performance using soft max normalization
library(DMwR)
library(grid)
autismadults_sm <- as.data.frame(apply(autismadults[1:16], MARGIN = 2, FUN = function(x)
  (SoftMax(x,lambda = 3, mean(x), sd(x)))))

#confirm softmax worked correctly
summary(autismadults_sm)

# Inspect using boxplots
boxplot (autismadults_sm, main = "Soft Max, lambda = 3")

autismadults_3varsm<-autismadults_sm[c(11,13,16)]
boxplot(autismadults_3varsm)

# create training (80%) and test data (20%) (data already in random order)

autismadults_train <- autismadults_sm[1:486, ]
autismadults_test <- autismadults_sm[487:608, ]

# create labels (from 16th column) for training and test data
autismadults_train_labels <- autismadults[1:486, 17]
autismadults_test_labels <- autismadults[487:608, 17]

# re-classify test cases
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test,
                              cl = autismadults_train_labels, k=25)

#inspect results
autismadults_test_pred
table(autismadults_test_pred)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)

#minmax and soft max normalization didn't make a difference to accuracy but increased FN


#try several different values of k (odd values) reverting to original normalisation

autismadults_train <- autismadults_mm[1:486, ]
autismadults_test <- autismadults_mm[487:608, ]

autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=1)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)

autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=5)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)

autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=9)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)

autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=13)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)

autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=17)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)

autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=21)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)

autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=25)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)


#............................confustion matrix...........................................
#more evaluation statistics

library(caret)

#confusion matrix
#make sure test and predicted have the same levels first
autismadults_test_labels<-as.factor(autismadults_test_labels)
levels(autismadults_test_labels)
levels(autismadults_test_pred)

#confustion matrix for K=25
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=25)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)
confusionMatrix(autismadults_test_pred, autismadults_test_labels)


#confustion matrix for K=1
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=1)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)
confusionMatrix(autismadults_test_pred, autismadults_test_labels)

#confustion matrix for K=5
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=5)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)
confusionMatrix(autismadults_test_pred, autismadults_test_labels)

#confustion matrix for K=9
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=9)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)
confusionMatrix(autismadults_test_pred, autismadults_test_labels)

#confustion matrix for K=13
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=13)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)
confusionMatrix(autismadults_test_pred, autismadults_test_labels)

#confustion matrix for K=17
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=17)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)
confusionMatrix(autismadults_test_pred, autismadults_test_labels)

#confustion matrix for K=21
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=21)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)
confusionMatrix(autismadults_test_pred, autismadults_test_labels)

#confustion matrix for K=25
autismadults_test_pred <- knn(train = autismadults_train, test = autismadults_test, cl = autismadults_train_labels, k=25)
CrossTable(x = autismadults_test_labels, y = autismadults_test_pred, prop.chisq=FALSE)
confusionMatrix(autismadults_test_pred, autismadults_test_labels)






#######################################


library(caret)
library(lpSolve)
library(irr)

autismadults$Class.ASD <- as.numeric(autismadults$Class.ASD)

autismadults_train_labels <- autismadults[1:547, 16]
folds <- createFolds(autismadults$Class.ASD, k = 10)

#folds for k=9 mean kappa 0.1728968
cv_results <- lapply(folds, function(x) {
  autismadults_train_cv <- autismadults[-x, ]
  autismadults_test_cv <- autismadults[x, ]
  autismadults_train_labels <- autismadults[-x,16]
  autismadults_pred <- knn (train = autismadults_train_cv, 
                            test = autismadults_test_cv, 
                            cl = autismadults_train_labels, k=9)
  length(autismadults_train_labels)
  autismadults_actual <- autismadults_test_cv$Class.ASD
  kappa <- kappa2(data.frame(autismadults_actual, autismadults_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))
# result, mean kappa  0.19

# K folds for K= 25
cv_results <- lapply(folds, function(x) {
  autismadults_train_cv <- autismadults[-x, ]
  autismadults_test_cv <- autismadults[x, ]
  autismadults_train_labels <- autismadults[-x,16]
  autismadults_pred <- knn (train = autismadults_train_cv, 
                            test = autismadults_test_cv, 
                            cl = autismadults_train_labels, k=25)
  length(autismadults_train_labels)
  autismadults_actual <- autismadults_test_cv$Class.ASD
  kappa <- kappa2(data.frame(autismadults_actual, autismadults_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))
# result K=0.20