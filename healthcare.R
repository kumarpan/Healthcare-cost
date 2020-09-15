library(data.table)
Claims = fread("C://Users/Sameer/Documents/Data Science/Analytics Edge/Unit 4/Healthcare Costs, Case - 2/ClaimsData.csv")


Claims<-fread(input = "ClaimsData.csv",sep=",",header = T,stringsAsFactors = T)
Claims<-as.data.frame(Claims)


class(Claims)
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl==TRUE)
ClaimsTest = subset(Claims, spl==FALSE)

list_name<-names(Claims)

Claims[,c(2:12,14)]<-sapply(Claims[,c(2:12,14)],FUN = as.factor)
sapply(Claims,class)
library(rpart)
library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class",cp=0.00001)

prp(ClaimsTree)

predTrain<-predict(ClaimsTree,type="class")

table(ClaimsTrain$bucket2009,predTrain)

ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00001,parms=list(loss=PenaltyMatrix))
#parms=list(loss=PenaltyMatrix)
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)

PenaltyError<-sum(table(ClaimsTrain$bucket2009,predTrain)*PenaltyMatrix)/nrow(ClaimsTrain)

predTest<-predict(ClaimsTree,newdata = ClaimsTest,type = "class")
conf_mat<-table(ClaimsTest$bucket2009,predTest)

PenaltyErrorTest<-sum(conf_mat*PenaltyMatrix)/nrow(ClaimsTest)


prp(ClaimsTree)


(ClaimsTree$parms)

library(caret)
numfolds<-trainControl(method="cv",number=5,summaryFunction=penaltyerror)
# Range of cp values, change it to find best cp value
cpGrid<-expand.grid(cp=seq(0.00001,0.00001,0.0001))
# Building the model with cv using caret library
set.seed(50)
model_cv<-train(as.factor(bucket2009) ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="rpart",trControl=numfolds,tuneGrid=cpGrid,maximize=F)

plot(model_cv$results[,c(1,2)],type="b")
res1<-model_cv$results[,c(1,2)]
res2<-model_cv$results[,c(1,2)]
res3<-model_cv$results[,c(1,2)]
  predTrain<-predict(model_cv)
  table(ClaimsTrain$bucket2009,predTrain)
  prp(model_cv$finalModel)
plot(model_cv)
model_cv

pred_cv<-predict(model_cv,newdata = ClaimsTest)
conf_cv<-table(ClaimsTest$bucket2009,pred_cv)

# Applying RandomForest
library(randomForest)

model_rf<-randomForest(as.factor(bucket2009) ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain,ntree=100,nodesize=100)
plot(model_rf)
model_rf$

  
  
penaltyerror<-function(data,lev=NULL,model=NULL)
{
error_mat<-as.matrix(table(data$obs,data$pred))*PenaltyMatrix
error<-sum(error_mat)/nrow(data)
names(error)<-c("Penalty Error")
error
}

#Random Forest using Caret

numfolds_rf<-trainControl(method = "cv",number=5,summaryFunction=penaltyerror)
# Range of cp values, change it to find best cp value
mtryGrid<-expand.grid(mtry=seq(1,14,1))
# Building the model with cv using caret library
model_rf<-train(as.factor(bucket2009) ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="rf",trControl=numfolds_rf,maximize=F,tuneGrid=mtryGrid)

tuneGrid=mtryGrid

# Using Xgboost
library(xgboost)
library(Matrix)
ClaimsTrain$bucket2009<-ClaimsTrain$bucket2009-1
sparse_train<-sparse.model.matrix(bucket2009~.-1-reimbursement2009,data = ClaimsTrain)
object.size(ClaimsTrain)
dtrain<-xgb.DMatrix(data = sparse_train,label=ClaimsTrain$bucket2009)
object.size(dtrain)

ClaimsTest$bucket2009<-ClaimsTest$bucket2009-1
sparse_test<-sparse.model.matrix(bucket2009~.-1-reimbursement2009,data = ClaimsTest)
object.size(ClaimsTest)
dtest<-xgb.DMatrix(data = sparse_test,label=ClaimsTest$bucket2009)
object.size(dtest)
watchlist<-list(train=dtrain,test=dtest)
model_xgb<-xgb.train(params = params,data = dtrain,verbose = 1,watchlist = watchlist,nrounds=1000,nthread=4,early_stopping_rounds = 100,feval = evalerror,maximize = F)

params=list(eta=0.01,max_depth=20,objective="multi:softmax",num_class=5)

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  
  err<-sum(table(labels,preds)*PenaltyMatrix)/nrow(dtrain)
  return(list(metric = "penalty_error", value = err))
}

pred_xgb<-predict(model_xgb,newdata = dtest)

table(ClaimsTest$bucket2009,pred_xgb)


