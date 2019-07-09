load('data/adult_split.rds')

train <- adult[adult$Set == 'Train', -12]
test <- adult[adult$Set == 'Test', -12]

rf <- randomForest::randomForest(x=train[,-11], y=train$Income,
                                 importance=TRUE, ntree=2000,
                                 xtest=test[,-11], ytest=test$Income,
                                 keep.forest=TRUE)

metrics <- function(df) {
  ix <- which(names(df) == 'Income')
  prob <- as.data.frame(predict(rf, df[,-ix], type='prob'))
  pred <- ROCR::prediction(prob[,2], df$Income)
  auc <- ROCR::performance(pred, "auc")
  auc <- auc@y.values[[1]]
  acc <- ROCR::performance(pred, 'acc')
  acc <- acc@y.values[[1]][which.min(abs(acc@x.values[[1]] - 0.5))]
  prec <- ROCR::performance(pred, 'prec')
  prec <- prec@y.values[[1]][which.min(abs(prec@x.values[[1]] - 0.5))]
  rec <- ROCR::performance(pred, 'rec')
  rec <- rec@y.values[[1]][which.min(abs(rec@x.values[[1]] - 0.5))]
  
  perf <- ROCR::performance(pred, "tpr", "fpr")
  ROCR::plot(perf, main=paste0('ROC curve (AUC ', round(auc, 4), ')'))
  lines(c(0, 1), c(0, 1), col='blue')
  
  round(c(auc=auc, accuracy=acc, precision=prec, recall=rec), 4)
}

conf.train <- rf$confusion
conf.test <- rf$test$confusion

train.metrics <- metrics(train)
test.metrics <- metrics(test)
