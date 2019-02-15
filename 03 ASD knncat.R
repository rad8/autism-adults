# get the data

setwd("C:/Users/Radiet/Desktop/Autism Screening Adults")
getwd()

#read data
autismadults<-read.csv("Autism-Adult-Data.csv")

#exploring the data:- whole data exploration
str(autismadults)


autismadults$age <- as.numeric(autismadults$age)
autismadults$result <- factor(autismadults$result)


#check class
str(autismadults)

#summarise numerical variable
autismadults <- autismadults[autismadults$age < 150,]

#-------------------prepare data for KNNcat--------------------------

#delete missing values
autismadults<-na.omit(autismadults)



#drop unnecessary attributes 
#16=country of res, 17=used app before, 19=age desc, 20=relation
autismadults<-autismadults[c(-16,-17,-19,-20)] 


#normalise numeric variable age

# ...................Try alternative package for catagorical data...........

library(knncat)

# re-normalize numerical data using minmax
autismadults[11] <- as.data.frame(apply(autismadults[11], MARGIN = 2, FUN = function(x)
  (x - min(x))/diff(range(x))))



#create training and test dataset
#randomize 
set.seed(12345)
autismadults <- autismadults[order(runif(608)), ]

autismadults_train <- autismadults[1:485, ]
str(autismadults_train)


autismadults_test <- autismadults[486:608, ]
str(autismadults_test)
str(autismadults_train)

############# run the model with cross-validation
autismadults_test_pred <- knncat(train = autismadults_train, test = autismadults_test, k=c(5, 9, 13, 17,21, 25), classcol = 17)



#results
autismadults_test_pred $best.k
autismadults_test_pred $misclass.mat
#confusionMatrix(autismadults_test_pred, autismadults_test_labels)
# create labels (from 19th column) for training and test data

autismadults_test_labels <- autismadults[486:608, 17]
autismadults_test_pred<-as.factor(autismadults_test_pred)

confusionMatrix(autismadults_test_pred, autismadults_test_labels)


#results
# > autismadults_test_pred $best.k
#[1] 5
#> autismadults_test_pred $misclass.mat
#       [,1] [,2]
#[1,]    83    0
#[2,]    0    40


#######################################



