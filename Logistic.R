# subsetting 
Not_seven_train<-subset(visits_train, outcome!=7)
Not_seven_test<-subset(visits_test, outcome!=7)

# Logistic
glm.fit=glm(outcome~., data=Not_seven_train ,family=binomial)
summary(glm.fit)
coef(glm.fit)
glm.probs=predict(glm.fit, data=Not_seven_test, type="response")
a<-as.numeric(as.character(glm.probs))
b<-as.numeric(as.character(Not_seven_test$outcome))
a<-round(a)                               
MSE=((a-b)^2)
mean(MSE)

#takes time
library(knnGarden)
set.seed(1)
knnVCN(Not_seven_train$outcome,Not_seven_test$outcome,ShowObs=FALSE,K=1,method="euclidean",p=2)
table(knnVCN.pred,Not_seven_test$outcome)
mean(knnVCN.pred==Not_seven_test$outcome)

#doesn't work
lda.fit<-lda(outcome~., data=visits_train)
lda.fit

#doesn't work
qda.fit<-qda(outcome~., data=visits, family=binomial)
qda.fit

#Trees
library("tree")
tree.Rambam=tree(outcome~.,Not_seven_train)
plot(tree.Rambam)
text(tree.Rambam,pretty=0)
summary(tree.Rambam)                                                                 #Residual mean deviance:  0.5612
tree.Rambam
tree.pred=predict (tree.Rambam,Not_seven_test)                                       #predicts 7 in all cases
compare<-as.character(tree.pred)==as.character(Not_seven_test$outcome)
Prediction_accuracy=sum(compare)/length(compare)

set.seed(3)
cv.tree=cv.tree(tree.Rambam, FUN=prune.misclass)
names(cv.tree)
cv.tree

# Classification Tree with rpart
library(rpart)
fit <- rpart(outcome~., method="class", data=Not_seven_train)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
plot(fit, uniform=TRUE,main="Classification Tree for Outcome") # plot tree 
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#bagging
library("randomForest")
set.seed(1)
bag.r=randomForest(outcome~.,data=Not_seven_train, mtry=3, importance=TRUE)
bag.r
plot(bag.r)

#forest
library("randomForest")
rf.Rambam=randomForest(outcome~.,data=Not_seven_train, importance=TRUE)
rf.Rambam
varImpPlot(rf.Rambam)
rf.pred=predict(rf.Rambam, Not_seven_test)
table(rf.pred,Not_seven_test$outcome)
mean(rf.pred==Not_seven_test$outcome)


library(glmnet)
glm.fit<-glmnet(outcome, Not_seven_train, family="multinomial", lambda=0)


#a ggod comparison in a tree
#tree - to write correctly
library("tree")
tree.Rambam=tree(hospitalization_dur~.,visits_train)
plot(tree.Rambam)
text(tree.Rambam,pretty=0)
summary(tree.Rambam)                                                        
tree.Rambam
tree.pred=predict (tree.Rambam, visits_test)                                       
compare<-as.character(tree.pred)==as.character(visits_test$outcome)
Prediction_accuracy=sum(compare)/length(compare)

