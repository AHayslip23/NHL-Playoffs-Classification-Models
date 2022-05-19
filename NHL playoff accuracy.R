#NHL Playoff Accuracy 
#how accurate can machine predict whether your team made the playoffs based on wins losses goals scored goals allowed and points 

playoffs <- read.csv('Teams.csv')
View(playoffs)

#dplyr 
#filter for NHL results only 
#filter for wins losses goals scored goals allowed and points 

library(dplyr)

#create new binary column for playoff appearances 
playoffs <- playoffs %>%
  mutate(Postseason = ifelse(playoff != "", 1, 0))

#filter for the only league being nhl, all other league are filtered out 
playoffs <- playoffs %>%
  filter(lgID == 'NHL')

#select seven variables 
playoffs <- playoffs %>%
  select(G, W, L, Pts, GF, GA, Postseason)

View(playoffs)

library(caTools)
set.seed(123)

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)
test = subset(playoffs, split == F)

#feature scaling 
train[-7] <- scale(train[-7])
test[-7] <- scale(test[-7]) 

View(train)
View(test)

#build y_pred 
library(class)
y_pred = knn(train = train[, -7],
             test = test[, -7], 
             cl = train[, 7], 
             k = 7, 
             prob = TRUE)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (115 + 231) / 398
print(accuracy)
#my model can predict whether an NHL team makes the playoffs with 86.9% accuracy 

#SVM classification  
library(caTools)
set.seed(123)

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)
test = subset(playoffs, split == F)

#feature scaling 
train[-7] <- scale(train[-7])
test[-7] <- scale(test[-7]) 

library(e1071)
classifier = svm(formula = Postseason ~.,
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear')

y_pred = predict(classifier, type = 'response', newdata = test[-7])
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (110 + 230) / 398
print(accuracy)
#performs at 85.4% not bad

#kernel svm  
split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)
test = subset(playoffs, split == F)

View(test)
View(train)

#feature scaling 
train[-7] <- scale(train[-7])
test[-7] <- scale(test[-7])

View(test)
View(train)

classifier = svm(formula = Postseason ~.,
                 data = train,
                 type = 'C-classification',
                 kernel = 'radial')

y_pred = predict(classifier, newdata = test[-7])
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy =  (118 + 227) / 398
print(accuracy)
#model is 86.7% accurate 

#naive bayes

playoffs$Postseason = factor(playoffs$Postseason, levels = c(0, 1))

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T) 
test = subset(playoffs, split == F) 

test[-7] <- scale(test[-7])
train[-7] <- scale(train[-7])

View(test)
View(train)

library(e1071)
classifier <- naiveBayes(x=train[-7],
                         y= train$Postseason)

y_pred = predict(classifier, newdata = test[-7])
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (107 + 204) / 398
print(accuracy)
#accuracy of 78.1% 

#decision tree 

playoffs$Postseason = factor(playoffs$Postseason, levels = c(0, 1))

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)
test = subset(playoffs, split == F)

train[-7] <- scale(train[-7])
test[-7] <- scale(test[-7])

library(rpart)
classifier <- rpart(formula = Postseason ~.,
                    data = train)

y_pred = predict(classifier, newdata = test[-7], type ='class')
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (128 + 214) / 398
print(accuracy)
#accuracy is 85.9% 

#random forest classification

playoffs$Postseason = factor(playoffs$Postseason, levels = c(0, 1))

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)
test = subset(playoffs, split == F)

train[-7] <- scale(train[-7])
test[-7] <- scale(test[-7])

library(randomForest)
classifer <-randomForest(x = train[-7],
                         y= train$Postseason,
                         ntree = 10) 

y_pred = predict(classifier, newdata = test[-7], type = 'class')
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (124 + 233) / 398
print(accuracy)
#accuracy is 89.7% 
#most accurate model is random classification 

#how accurate can we predict that a team made a playoffs using the classifciation model 

#caps

playoffs <- read.csv('Teams.csv')
View(playoffs)

playoffs <- playoffs %>%
  mutate(Postseason = ifelse(playoff != "", 1, 0))

#filter for the only league being nhl, all other league are filtered out 
playoffs <- playoffs %>%
  filter(lgID == 'NHL')

playoffs <- playoffs %>%
  filter(tmID == 'WAS')

#select seven variables 
playoffs <- playoffs %>%
  select(G, W, L, Pts, GF, GA, Postseason)

View(playoffs)

#knn

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)
test = subset(playoffs, split == F)

train[-7] <- scale(train[-7])
test[-7] <- scale(test[-7]) 

View(test)
View(train)

library(class)
y_pred = knn(train = train[, -7],
             test = test[, -7],
             cl = train[, 7],
             k = 7,
             prob = TRUE)


cm = table(test[, 7], y_pred)
print(cm)

accuracy = (4 + 7)/ 11
print(accuracy)

#100% accuracy with KNN model

#svm classification 
split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)
test = subset(playoffs, split == F)

train[-7] <- scale(train[-7])
test[-7] <- scale(test[-7]) 

library(e1071)
classifier <- svm(formula = Postseason ~.,
                  data = train,
                  type = 'C-classification',
                  kernel = 'radial')

y_pred = predict(classifier, type = 'response', newdata = test[-7])
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (3 + 6) / 11 
print(accuracy)

#81.8 accurate 

#kernel svm 
split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T) 
test = subset(playoffs, split == F)

classifer = svm(formula = Postseason ~.,
                data = train,
                type = 'C-classification',
                kernel = 'linear') 

y_pred = predict(classifer, newdata = test[-7])
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (4 + 6) / 11
print(accuracy)
#90.9 accurate 

#naive bayes 
playoffs$Postseason = factor(playoffs$Postseason, levels = c(0, 1))

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)  
test = subset(playoffs, split == F)

train[-7] <- scale(train[-7])
test[-7] <- scale(test[-7]) 

library(e1071)

classifier <- naiveBayes(x = train[-7],
                         y = train$Postseason)

y_pred = predict(classifier, newdata = test[-7])
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (1 + 7) / 11
print(accuracy)
#72.7% accuracy 

#decision tree 
playoffs$Postseason = factor(playoffs$Postseason, levels = c(0, 1))

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T) 
test = subset(playoffs, split == F)

train[-7] = scale(train[-7]) 
test[-7] = scale(test[-7])

library(rpart)
classifier = rpart(formula = Postseason ~.,
                   data = train)

y_pred = predict(classifier, newdata = test[-7], type = 'class')
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (4 + 6) / 11
print(accuracy)
#accuracy 90.9% 

#random forest 

split = sample.split(playoffs$Postseason, SplitRatio = 0.70)
train = subset(playoffs, split == T)
test = subset(playoffs, split == F)

train[-7] <- scale(train[-7]) 
test[-7] <- scale(test[-7])

library(randomForest)
classifier <- randomForest(x=train[-7],
                           y=train$Postseason, 
                           ntree = 10)

y_pred = predict(classifier, newdata = test[-7], type = 'class')
print(y_pred)

cm = table(test[, 7], y_pred)
print(cm)

accuracy = (4 + 6) / 11
print(accuracy)
#accuracy 90.9%
















