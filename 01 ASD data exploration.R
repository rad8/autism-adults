
# get the data

setwd("C:/Users/Radiet/Desktop/Autism Screening Adults")
getwd()

#read data
autismadults<-read.csv("Autism-Adult-Data.csv", stringsAsFactors = F)

#exploring the data:- whole data exploration
str(autismadults)


# convert data to appropriate class
autismadults$A1_Score <- factor(autismadults$A1_Score)
autismadults$A2_Score <- factor(autismadults$A2_Score)
autismadults$A3_Score <- factor(autismadults$A3_Score)
autismadults$A4_Score <- factor(autismadults$A4_Score)
autismadults$A5_Score <- factor(autismadults$A5_Score)
autismadults$A6_Score <- factor(autismadults$A6_Score)
autismadults$A7_Score <- factor(autismadults$A7_Score)
autismadults$A8_Score <- factor(autismadults$A8_Score)
autismadults$A9_Score <- factor(autismadults$A9_Score)
autismadults$A10_Score <- factor(autismadults$A10_Score)
autismadults$age <- as.numeric(autismadults$age)
autismadults$result <- factor(autismadults$result)
autismadults$gender <- factor(autismadults$gender)
autismadults$ethnicity <- factor(autismadults$ethnicity)
autismadults$jundice <- factor(autismadults$jundice)
autismadults$family_autism <- factor(autismadults$family_autism)
autismadults$contry_of_res <- factor(autismadults$contry_of_res)
autismadults$used_app_before <- factor(autismadults$used_app_before)
autismadults$age_desc <- factor(autismadults$age_desc)
autismadults$relation <- factor(autismadults$relation)
autismadults$Class.ASD <- factor(autismadults$Class.ASD)

#check class
str(autismadults)


#summarise numerical variable
autismadults <- autismadults[autismadults$age < 150,]
summary(autismadults$age)
boxplot(autismadults$age, main="Boxplot of age", ylab="age", outline = FALSE)
hist(autismadults$age, freq = F, main = "Histogram of Age", xlab = "age") 
lines(density(sort(autismadults$age)))




#exploring ASD cases
table(autismadults$Class.ASD)
Classper<-prop.table(table(autismadults$Class.ASD))*100
round(Classper,digits = 1)


#........ cross tab and chi square for individual characters........................
#exploring ethnicity ratio 
table(autismadults$ethnicity)
table(autismadults$ethnicity, useNA = "ifany")
ethnper<-prop.table(table(autismadults$ethnicity, useNA = "ifany"))*100
round(ethnper,digits = 1)
table(autismadults$relation, useNA = "ifany")

# exploring female male ratio
table(autismadults$gender)
genper<-table(autismadults$gender)
genper<-prop.table(table(autismadults$gender))*100
round(genper,digits = 1)

#.......Cross tabulation and chi square ..................

#Cross tabulation and chi square for  Class.ASD with ethnicity 
table.ethnicity.ASD<-xtabs(~ autismadults$ethnicity + autismadults$Class.ASD)
table.ethnicity.ASD
chisq.test(table.ethnicity.ASD)

#Cross tabulation and chi square for Class.ASD with gender
table.gender.ASD<-xtabs(~ autismadults$gender+ autismadults$Class.ASD)
table.gender.ASD
chisq.test(table.gender.ASD)



#Cross tabulation and chi square for Class.ASD with jundice
table.jundice.ASD<-xtabs(~ autismadults$jundice+ autismadults$Class.ASD)
table.jundice.ASD
chisq.test(table.jundice.ASD)


#Cross tabulation and chi square for Class.ASD with family_autism
table.famautism.ASD<-xtabs(~ autismadults$family_autism+ autismadults$Class.ASD)
table.famautism.ASD
chisq.test(table.famautism.ASD)


#Cross tabulation and chi square for Class.ASD with country of Residence
table.count.ASD<-xtabs(~ autismadults$contry_of_res+ autismadults$Class.ASD)
table.count.ASD
chisq.test(table.count.ASD)
table(autismadults$contry_of_res, useNA = "ifany")

#Cross tabulation and chi square for Class.ASD with used app before 
table.app.ASD<-xtabs(~ autismadults$used_app_before+ autismadults$Class.ASD)
table.app.ASD
chisq.test(table.app.ASD)


#Cross tabulation and chi square for Class.ASD with result
table.result.ASD<-xtabs(~ autismadults$result+ autismadults$Class.ASD)
table.result.ASD
chisq.test(table.result.ASD)


#Cross tabulation and chi square for Class.ASD with age description
table.agedesc.ASD<-xtabs(~ autismadults$age_desc+ autismadults$Class.ASD)
table.agedesc.ASD
chisq.test(table.agedesc.ASD)


#Cross tabulation and chi square for Class.ASD with relation
table.relation.ASD<-xtabs(~ autismadults$relation + autismadults$Class.ASD)
table.relation.ASD
chisq.test(table.relation.ASD)


#............... Chi Square test for A1-A10 scores..................

#Cross tabulation and chi square for Class.ASD with A1_Score
table.A1.ASD<-xtabs(~ autismadults$A1_Score+ autismadults$Class.ASD)
table.A1.ASD
chisq.test(table.A1.ASD)
#p-value = 8.302e-15

#Cross tabulation and chi square for Class.ASD with A2_Score
table.A2.ASD<-xtabs(~ autismadults$A2_Score+ autismadults$Class.ASD)
table.A2.ASD
chisq.test(table.A2.ASD)

#Cross tabulation and chi square for Class.ASD with A3_Score
table.A3.ASD<-xtabs(~ autismadults$A3_Score+ autismadults$Class.ASD)
table.A3.ASD
chisq.test(table.A3.ASD)

#Cross tabulation and chi square for Class.ASD with A4_Score
table.A4.ASD<-xtabs(~ autismadults$A4_Score+ autismadults$Class.ASD)
table.A4.ASD
chisq.test(table.A4.ASD)

#Cross tabulation and chi square for Class.ASD with A5_Score
table.A5.ASD<-xtabs(~ autismadults$A5_Score+ autismadults$Class.ASD)
table.A5.ASD
chisq.test(table.A5.ASD)


#Cross tabulation and chi square for Class.ASD with A6_Score
table.A6.ASD<-xtabs(~ autismadults$A6_Score+ autismadults$Class.ASD)
table.A6.ASD
chisq.test(table.A6.ASD)

#Cross tabulation and chi square for Class.ASD with A7_Score
table.A7.ASD<-xtabs(~ autismadults$A7_Score+ autismadults$Class.ASD)
table.A7.ASD
chisq.test(table.A7.ASD)

#Cross tabulation and chi square for Class.ASD with A8_Score
table.A8.ASD<-xtabs(~ autismadults$A8_Score+ autismadults$Class.ASD)
table.A8.ASD
chisq.test(table.A8.ASD)

#Cross tabulation and chi square for Class.ASD with A9_Score
table.A9.ASD<-xtabs(~ autismadults$A9_Score+ autismadults$Class.ASD)
table.A9.ASD
chisq.test(table.A9.ASD)

#Cross tabulation and chi square for Class.ASD with A10_Score
table.A10.ASD<-xtabs(~ autismadults$A10_Score+ autismadults$Class.ASD)
table.A10.ASD
chisq.test(table.A10.ASD)
