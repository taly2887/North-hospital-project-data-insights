
#Simple linear for time -------------------------------overfit, R=1

lm.fit<-lm(hospitalization_dur~., data=visits_train)
summary(lm.fit)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma
pred<- predict(lm(hospitalization_dur~., data=visits_test))
MSE=mean((visits_test-pred)^2)

#Subset-----------------------------------------------------------------------does not wirk - time error
library (leaps)
regfit.full=regsubsets(hospitalization_dur~., visits_train, nvmax=1)
reg.summary=summary(regfit.full)
reg.summary
names(reg.summary)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab="RSS",type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab="Adjusted Rsq", type = "l")
plot(reg.summary$cp, xlab = "Number of Variables", ylab="Cp", type = "l")
plot(reg.summary$bic, xlab = "Number of Variables", ylab="BIC",type = "l")
which.max(reg.summary$adjr2)

lm.fit<-lm(hospitalization_dur~., data=visits)
summary(lm.fit)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma
#------------------------------------------------------------------------------------------------------------

#tree - to write correctly
library("tree")
tree.Rambam=tree(hospitalization_dur~.,visits_train)
plot(tree.Rambam)
text(tree.Rambam,pretty=0)
summary(tree.Rambam)                                                        
tree.Rambam
tree.pred=predict(tree.Rambam, visits_test)                                       
MSE_tree<-mean((tree.pred-visits_test$hospitalization_dur)^2)

#bagging
library(randomForest)
set.seed(1)
bag.time=randomForest(hospitalization_dur~.,data=visits_train ,mtry=10, importance=TRUE)
bag.time
yhat=predict(bag.time,newdata=visits_test)
plot(yhat,visits_test$hospitalization_dur)
abline(0,1)
MSE_bag<-mean((yhat-visits_test$hospitalization_dur)^2)
importance(bag.time)
varImpPlot(bag.time)

#forest
library("randomForest")
rf.Rambam=randomForest(hospitalization_dur~.,data=visits_train, importance=TRUE)
rf.Rambam
varImpPlot(rf.Rambam)
rf.pred=predict(rf.Rambam, visits_test)
MSE_forest<-(mean((rf.pred-visits_test$hospitalization_dur)^2))

MSE_tree
MSE_bag
MSE_forest
