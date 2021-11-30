# installs packages, only run this section once
install.packages('class')
install.packages('e1071')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('ggplot2')
install.packages('magrittr')
install.packages('arules')
install.packages('arulesViz')
install.packages('ROCR')

# imports libraries
library(class)
library(e1071)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(magrittr)
library(arules)
library(arulesViz)
library(ROCR)

##################
#    Functions   #
##################
# Select all the functions and run them to load

# Gets the basic statistical values of a vector x
getstats <- function(x){
  stat = c(mean(x),median(x),var(x),sd(x),max(x),min(x),length(x))
  name = c('Mean','Median','Var','SD','Max','Min','Length')
  return (data.frame(name,stat))
}

# Gets the basic statistical values of two vectors x and y
getstats2 <- function(x,y){
  stat = c(cor(x,y),cov(x,y))
  name = c('Cor','Cov')
  return (data.frame(name,stat))
}

# Linear regression
linreg <- function(x,y){
  beta_1 <- (sum(x*y)-mean(y)*sum(x))/(sum(x^2)-mean(x)*sum(x));
  beta_0 <- mean(y)-beta_1*mean(x);
  return(c(beta_0,beta_1));
}

# Entropy of a purity
entropy <- function(p){
  if (p==0 | p==1){
    return (0)
  } else {
    return (-(log2(p)*p+log2(1-p)*(1-p)))
  }
}

# Gini index of a purity
gini <- function(p){
  return (2*p*(1-p))
}

# Logistic function
logistic <- function(z){
  exp(z)/(1+exp(z))
}

# Likelihood function given probability, number of 1's and 0's
likelihood <- function(p,n1,n0){
  p^n1*(1-p)^(n0)
}

# Accuracy of a confusion matrix
accuracy <- function(matrix){
  return (sum(diag(matrix))/sum(matrix));
}

# Precision of a confusion matrix
precision <- function(matrix){
  return (matrix[2,2]/sum(matrix[2,]));
}

# TPR of a confusion matrix
tpr <- function(matrix){
  return (matrix[2,2]/sum(matrix[,2]));
}

# FNR of a confusion matrix
fnr <- function(matrix){
  return (matrix[1,2]/sum(matrix[,2]));
}

# FPR of a confusion matrix
fpr <- function(matrix){
  return (matrix[2,1]/sum(matrix[,1]));
}

# The whole diagnostics of a confusion matrix
diagnostics <- function(matrix){
  Diagnostics = c("Accuracy","Precision","TPR","FPR (T1 err)","FNR (T2 err)");
  Rate = c(accuracy(matrix),precision(matrix),tpr(matrix),fpr(matrix),fnr(matrix));
  Percentage = round(Rate*100,2)
  return (data.frame(Diagnostics,Rate,Percentage));
}

# The whole diagnostics given TP,FN,FP,TN in that order
diagnosticsmanual <- function(TP,FN,FP,TN){
  Diagnostics = c("Accuracy","Precision","TPR","FPR (T1 err)","FNR (T2 err)");
  Rate = c((TP+TN)/(TP+FN+FP+TN),TP/(TP+FP),TP/(TP+FN),FP/(FP+TN),FN/(TP+FN));
  Percentage = round(Rate*100,2)
  return (data.frame(Diagnostics,Rate,Percentage));
}

# Sum of Euclidean distances of a point to a list of points
euclidean <- function(centroid, p_list){
  dist <- numeric(nrow(p_list))
  for (i in 1:nrow(p_list)){
    dist[i] = sqrt(sum((centroid-p_list[i,])^2))
  }
  return (dist)
}

# Does the whole thing related to KNN
displayknn <- function(train.x,test.x,train.y,test.y,K){
  knn.pred = knn(train.x,test.x,train.y,k=K)
  confusion.matrix=table(knn.pred, test.y)
  predicted = as.factor(c(0,0,1,1))
  actual = as.factor(c(0,1,0,1))
  cm = table(predicted,actual)
  cm[1,1]='TN'
  cm[1,2]='FN'
  cm[2,1]='FP'
  cm[2,2]='TP'
  print(cm)
  print(confusion.matrix)
  print(diagnostics(confusion.matrix))
}

# KNN distances and fitted Y value
knn_fitted <- function(k, outcomes, features, test_point) {
  distances = cbind(euclidean(test_point, features), outcomes)
  k_nearest = distances[order(distances[,1]),][1:k,]
  colnames(k_nearest) = c('Euclidean', 'Labels')
  print('Test Point')
  print(test_point)
  print('k nearest neighbours')
  print(k_nearest)
  print('Fitted Y value')
  print(sum(k_nearest[,2])/k)
}

# For Naive Bayes function
probability <- function(table,y){
  prob = 1;
  for (i in 1:dim(table)[2]){
    prob <- prob*table[y,i]
  };
  return (prob);
}

# Does the whole thing related to Naive Bayes
naivebayes <- function(table){
  Class <- rownames(table);
  Probability <- probability(table,Class);
  return (data.frame(Probability));
}

# Does the whole thing related to Naive Bayes V2
nb <- function(tabulation, chars){
  colnames(tabulation) <- chars
  conditional = (tabulation/tabulation[,ncol(tabulation)])[,-ncol(tabulation)]
  Is.Y = tabulation[,ncol(tabulation)]
  conditional = cbind(conditional,Is.Y/sum(Is.Y))
  Probability <- naivebayes(conditional)[,1]
  LogProb <- log(Probability)
  conditional <- cbind(conditional,Probability,LogProb)
  colnames(conditional) <- c(chars[-length(chars)],"P(Y)","P(Y|X)", "log(P(Y|X))")
  return (conditional)
}

# Does the whole thing related to Decision Tree
dt <- function(Yes,No){
  FYes = Yes
  FNo = No
  Total = Yes+No
  Yes = Yes/Total
  No = No/Total
  Total = Total/sum(Total)
  data = as.data.frame(rbind(Total,Yes,No))
  purity = as.data.frame(rbind(FYes,FNo))
  purity = cbind(purity,rowSums(purity))
  
  names(data) = c('split1','split2')
  names(purity) = c(names(data),'Total')
  rownames(purity) = c('Yes','No')
  
  #print('Data')
  #print(data)
  #print('')
  print('Contigency Table')
  print(purity)
  print('')
  fle = entropy(purity['Yes','Total']/sum(purity[,'Total']))
  print('Base Entropy')
  print(fle)
  print('')
  sle = data['Total',1]*entropy(data['Yes',1])+
    data['Total',2]*entropy(data['Yes',2])
  print('Conditional Entropy')
  print(sle)
  print('')
  print('Entropy Reduction')
  print(fle-sle)
}

# Does the whole thing related to Decision Tree but with Gini index
dtgini <- function(Yes,No){
  FYes = Yes
  FNo = No
  Total = Yes+No
  Yes = Yes/Total
  No = No/Total
  Total = Total/sum(Total)
  data = as.data.frame(rbind(Total,Yes,No))
  purity = as.data.frame(rbind(FYes,FNo))
  purity = cbind(purity,rowSums(purity))
  
  names(data) = c('split1','split2')
  names(purity) = c(names(data),'Total')
  rownames(purity) = c('Yes','No')
  
  #print('Data')
  #print(data)
  #print('')
  print('Contigency Table')
  print(purity)
  print('')
  fle = gini(purity['Yes','Total']/sum(purity[,'Total']))
  print('Base Gini Index')
  print(fle)
  print('')
  sle = data['Total',1]*gini(data['Yes',1])+
    data['Total',2]*gini(data['Yes',2])
  print('Conditional Gini Index')
  print(sle)
  print('')
  print('Gini Index Reduction')
  print(fle-sle)
}

# Does the whole thing related to k-means clustering
kmc <- function(data,k){
  kout <- kmeans(data,centers=k)
  print('Clustering')
  print(kout$cluster)
  print('Centroids')
  print(kout$centers)
  print('Iterations taken')
  print(kout$iter)
  print('WSS assuming k = 1')
  print(kout$totss)
  print('WSS of each cluster')
  print(kout$withinss)
  print('The total WSS')
  print(kout$tot.withinss)
}

# Does the whole thing related to Apriori Algorithm
checkfrequentitemsets <- function(dataset,ml,mxl,minsupport,topk){
  itemsets <- apriori(dataset, parameter=list(minlen=ml, maxlen=mxl,support=minsupport, target="frequent itemsets"))
  print(inspect(head(sort(itemsets, by = "support"), topk)))
  print('')
  print('')
  print('')
  print('')
  print('')
  print('')
  print('Summary')
  print(summary(itemsets))
}

################################################################################

###################
#   Basic Stats   # 
###################

x = 1:4
y = c(5,1,2,1)
# edit up to here
getstats(x) # Mean, Median, Var, SD, Max, Min, Length
getstats2(x,y) # Cor, Cov
plot(x,y,cex=3)

###########################
#   Classifier Diagrams   # 
###########################

# Diagnostics
# Calculate all the metrics based on the predicted and actual values
predicted = c(0,0,1)
actual = c(0,1,0)
#edit up to here
confusion.matrix=table(predicted, actual)
print(confusion.matrix)
diagnostics(confusion.matrix)

# Diagnostics from given confusion matrix values
diagnosticsmanual(0,1,1,1) # args: TP, FN, FP, TN

###########
#   KNN   #
###########

# Diagnostics of KNN when given fitted and actual values, with varying threshold
actual = c(1,1,0,1,1,0,0,0,1,0)
fitted = c(0.9,0.8,0.8,0.6,0.5,0.5,0.5,0.3,0.2,0.1)
thresholds = c(0.7)
# edit up to here
for (i in 1:length(thresholds)){
  predicted = ifelse(fitted > thresholds[i], 1, 0)
  confusion.matrix=table(predicted, actual)
  print('Threshold')
  print(thresholds[i])
  print(confusion.matrix)
  print(diagnostics(confusion.matrix))
  print('#########################')
}

# Calculate Euclidean Distance of points from centroid if data given in table form
centroid <- c(0,0,0)
p_list <- matrix(c(0,3,0,2,0,0,0,1,3,0,1,2,-1,0,1,1,1,1), 
                 nrow = 6,
                 ncol = 3,
                 byrow = TRUE)
# edit up to here
euclidean(centroid, p_list)
plot(p_list,cex=3) #for 2D
points(centroid[1], centroid[2],pch=19,col='red')


# Calculate Euclidean Distance of all points from centroid if data given by feature
centroid <- c(2,2)
x1 = c(0,2,2.5,3,4,4)
x2 = c(1,4,2,4.5,2,3)
# edit up to here
p_list = cbind(x1, x2)
euclidean(centroid, p_list)
plot(x1,x2,cex=3) #if 2D
points(centroid[1], centroid[2],pch=19,col='red')

# Calculate KNN fitted outcome and distance from KNNs
k = 1
x1 = c(1,2,1,3,3)
x2 = c(3,2,1,3,1)
p_list = cbind(x1, x2)
y = c(1,1,0,1,0)
test.x1 = c(3)
test.x2 = c(2)
# edit up to here
plot(x1,x2,cex=3)
text(x1,x2, labels=y, cex=1, font=2)
test.x = cbind(test.x1,test.x2)
points(test.x,pch=19,col='red')
for (i in 1:length(test.x1)){
  knn_fitted(k, y, p_list, test.x[i,])
  print('#########################')
}

# Calculate the whole KNN thing
k = 2
x1 = c(25,35,45,10,35,52,23,40,90,48,33)
x2 = c(4,6,8,2,12,1.8,9.5,6.2,10,22,15)
test.x1 = c(48,17,11,22,33,44,55,66,77,88)
test.x2 = c(14.2,0,11,12,13,14,15,16,17,0)
train.y = c(0,0,0,0,0,0,1,1,1,1,1)
test.y = c(1,0,1,0,1,0,1,0,1,1)
# edit up to here
train.x = cbind(x1,x2)
test.x = cbind(test.x1,test.x2)
plot(x1,x2,cex=3)
text(x1,x2, labels=train.y, cex=1, font=2)
points(test.x,pch=19,col='red')
set.seed(1)
displayknn(train.x,test.x,train.y,test.y,k)

###################
#  Decision Tree  # 
###################

# Base entropy
entropy(1/2)
gini(1/2)

# Base entropy, Conditional entropy, Entropy reduction
# Yes = c(yes on the left, yes on the right)
# e.g. left: No(1364/1731), right: Yes(344/470)
Yes = c(1731-1364,344)
No = c(1364,470-344)
# edit up to here
dt(Yes,No)
#dtgini(Yes,No)

#################
#  Naive Bayes  # 
#################

# Bayes Theorem (test results qn)
C = 0.015 # P(having disease)
A_given_not_C = 0.025 # P(pos test given not have disease)
A_given_C = 0.99 # P(pos test given have disease)
# edit up to here
C_given_A = (A_given_C*C)/((A_given_C*C) + (1-C)*(A_given_not_C))
C_given_A # P(having disease given pos test)
1 - C_given_A # P(not having disease given pos test)

# The whole nb
Characteristics = c("Year 1","Total") #only need the conditionals and total
Banana <- c(150,250)
Orange <- c(80,250)
Other <- c(100,250)
water <- c(90,250)
tabulation = rbind(Banana,Orange,Other, water)
#edit up to here
colnames(tabulation) <- Characteristics
tabulation
nb(tabulation, Characteristics)

#######################
#  Linear Regression  # 
#######################

x = c(1,2,4)
y = c(0,25,9)**0.5
# edit up to here
plot(x,y,cex=3)
getstats(x)
getstats2(x,y)
linreg(x,y) #least squares estimates
lr = lm(y~x)
lr$coefficients
lr$fitted.values
lr$residuals 
abline(lr, col="red", lwd=3)

#########################
#  Logistic Regression  # 
#########################
# Functions to use : logistic(z) and likelihood(p,n1,n0)

x <- c(1,0,1,0,1,0,1)
y <- c(1,0,1,1,1,0,0)
# edit up to here
plot(y~x,cex=3)
logistic.fit <- glm(y ~ x, family = binomial(link="logit"))
coef <- logistic.fit$coefficients
coef
summary(logistic.fit)

#############
#  K-means  # 
#############

# The whole k-means (use this when initial centroids are not given)
x1 = c(3,3.5,4.5)
x2 = c(4,5,5)
x3 = c(0,3,5,2,9)
data = cbind(x1,x2)
data
class(data)
apply(data,1,sum)
apply(data,2,sum)
plot(x1,x2,cex=3) #for 2D
kmc(data,1)

# Finding Total WSS for GIVEN centroids (just plot and manually see how)
x1 = c(1,1.5)
x2 = c(1,2)
data = data.frame(x1,x2)
data
plot(x1,x2,cex=3)
centroids = cbind(c(2,2), c(4,4)) #put centroids in here
points(centroids[1,],centroids[2,],pch=19,col='red')
# how to calculate Total WSS manually:
# 1. separate the points into clusters based on centroids given
# 2. for each point in a cluster, take the difference between
# (x1,x2) in the point and centroid, each squared, then add the values together
# (if the point has the same x or y value as the centroid, that part cancels out)
# add up these values to get the WSS for the centroid
# add up those WSSs to get the Total WSS
kmc(data,2)
# use this if and only if the given centroids are the best centroids
# double check with the kmeans output centroids and see if it's the same as the given ones

#############
#  Apriori  #  GROCERIES DATA RESTRICTED!
#############
data(Groceries)

Groceries@itemInfo[11:20,]
Groceries@data[1:10,1:2]

checkfrequentitemsets(Adult,1,1,0.02,10)

rules <- apriori(Groceries, parameter=list(support=0.001,
                                           confidence=0.6, target = "rules"))
inspect(head(sort(rules, by="lift"), 3))

#plot(rules)
#highLiftRules <- head(sort(rules, by="lift"), 10)
#plot(highLiftRules, method="graph", control=list(alpha=1))

# The basic Association Rules stats
SX = .9
SY = .9
SXY = 15/100
#edit up to here
Confidence = SXY/SX
Confidence
# below only works if you know Support(Y)
Lift = Confidence/SY
Lift
Leverage = SXY - SX*SY
Leverage

# HOW TO APRIORI

# IDENTIFYING FREQUENT ITEMSETS
# 1. First figure out what the minimum support threshold is (e.g. 0.02)
# 2. Get the frequency of each 1-itemsets, get support by dividing by total entries
# 3. Filter out the 1-itemsets with support < MST (this is 1 iteration)
# 4. Derive all possible candidate 2-itemsets by merging the frequent 1-itemsets
# 5. Get the frequency -> support -> filter by MST for the candidate 2-itemsets (2 iterations)
# 6. Keep doing this until there are no more k-itemsets that meet MST or k = maxlen

# IDENTIFYING RULES
# 1. You need R for this, see code above