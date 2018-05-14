################################################################################
######################### CLASSIFICATION OF RED WINES ##########################
################################################################################

# Based on the features of the wine, model will classify the particular wine as "worth buying" and
# "not worth buying"

# Libraries
library(e1071)
library(party)
library(ggplot2)

# Loading the data
getwd()
setwd("C:/Users/tomas/OneDrive/Dokumenty/Wine Classification")
wineRed_ds = read.table("winequality-red.csv", header = TRUE, sep=";", na.strings= "*")

# Working copy of the dataset
wineRed_ds1 = wineRed_ds
# Review of the variables
summary(wineRed_ds1)

# Adding the variable, which narrows the quality of the wine to two categories:
# "worth buying" and "not worth buying"
wineRed_ds1$Q = lapply(wineRed_ds1[,12], function (x)
{
  if(x >5)  { "worth buying"}
  else { "not worth buying"}   
})
summary(wineRed_ds1)
wineRed_ds1$Q = unlist(wineRed_ds1$Q)
wineRed_ds1$Q = as.factor(wineRed_ds1$Q)
str(wineRed_ds1)

# Deleting the "quality" variable - having the substitute nominal variable:
# "worth buying"
# "not worth buying"

wineRed_ds1 = wineRed_ds1[-c(12)]
str(wineRed_ds1)

##########################################################################
################### EXPLORATORY DATA ANALYSIS ############################
##########################################################################

# To have better idea about the wine data it will be displayed on the plots

# The histogram of the alcohol variable
p = ggplot(wineRed_ds1, aes(alcohol)) + geom_histogram(binwidth = 0.2, color = "black",
                                                       aes(fill = Q)) + theme_bw()
print(p)

# The histogram of the fixed acidity
p0 = ggplot(wineRed_ds1, aes(fixed.acidity)) + geom_histogram(binwidth = 0.4, color = "black",
                                                       aes(fill = Q)) + theme_bw()
print(p0)

# The histogram of the residual sugar
p2 = ggplot(wineRed_ds1, aes(residual.sugar)) + geom_histogram(binwidth = 0.5, color = "black",
                                                              aes(fill = Q)) + theme_bw()
print(p2)

# The histogram of the chlorides
p3 = ggplot(wineRed_ds1, aes(chlorides)) + geom_histogram(binwidth = 0.01, color = "black",
                                                               aes(fill = Q)) + theme_bw()
print(p3)

# The relation between the density and alcohol
p4 = ggplot(wineRed_ds1, aes(wineRed_ds1$density, wineRed_ds1$alcohol)) + 
  geom_point(aes(color = Q),size = 4, alpha = 0.3) + theme_bw() +
  xlab("Wine density") +
  ylab("Wine alcohol") +
  ggtitle("The plot of wine alcohol in relation to density with quality division")
print(p4)

# The relation between the density and alcohol
p5 = ggplot(wineRed_ds1, aes(pH, alcohol)) + 
  geom_point(aes(color = Q),size = 4, alpha = 0.3) + theme_bw() +
  xlab("Wine pH") +
  ylab("Wine alcohol") +
  ggtitle("The plot of pH in relation to alcohol with quality division")
print(p5)

# The relation between the fixed acidity and citric acid
p6 = ggplot(wineRed_ds1, aes(fixed.acidity, citric.acid)) + 
  geom_point(aes(color = Q),size = 4, alpha = 0.3) + theme_bw() +
  xlab("Wine fixed acidity") +
  ylab("Wine citric acid") +
  ggtitle("The plot of fixed acidity in relation to citric acid with quality division")
print(p6)

# The relation between the fixed acidity and volatile acidity
p7 = ggplot(wineRed_ds1, aes(fixed.acidity, volatile.acidity)) + 
  geom_point(aes(color = Q),size = 4, alpha = 0.3) + theme_bw() +
  xlab("Wine fixed acidity") +
  ylab("Wine volatile acidity") +
  ggtitle("The plot of fixed acidity in relation to volatile acidity with quality division")
print(p7)

# The relation between the volatile acidity and citric acid
p8 = ggplot(wineRed_ds1, aes(volatile.acidity, citric.acid)) + 
  geom_point(aes(color = Q),size = 4, alpha = 0.3) + theme_bw() +
  xlab("Wine volatile acidity") +
  ylab("Wine citric acid") +
  ggtitle("The plot of volatile acidity in relation to citric acid with quality division")
print(p8)

#############################################################################
######################### DECISION TREE ALGORITHM ###########################
#############################################################################

#------------------------------------
# Data model with ctree:
#------------------------------------
# The model is launched in the loop to check the performance on different trainset
# and testset

results = c()
for (i in 1:100){
# Dividing the dataset into trainset and testset
sam <- sample(2, nrow(wineRed_ds1), replace=TRUE, prob=c(0.7, 0.3))
trainData <- wineRed_ds1[sam==1,]
testData <- wineRed_ds1[sam==2,]

wine_ctree = ctree(formula = Q~., data = trainData)
# Printing the tree
#print(wine_ctree)
# Plotting the tree
#plot(wine_ctree)
#plot(wine_ctree, type="simple")

# Checking the classificator on the test data
table = table(predict(wine_ctree, testData[,1:11]), testData$Q)
classifErr = (table[1,1]+table[2,2])/(table[1,2]+table[2,1]+table[1,1]+table[2,2])
results[i] = classifErr
}

# Results after 100 iterations
mean(results)
sd(results)


########################################################################
############### NAIVE BAYES APPROACH (FOR COMPARISON) ##################
########################################################################

# Building model
nbClasif <- naiveBayes(formula = Q~., data=trainData, laplace = 0)

print(nbClasif)

# Using the classificator for train data
table = table(trainData$Q,predict(nbClasif,trainData))
confusionMatrix(table)

# Using the classificator for test data
testPred = predict(nbClasif,testData)
table = table(testData$Q,testPred)
confusionMatrix(table)




