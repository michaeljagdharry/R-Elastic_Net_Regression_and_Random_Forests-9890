library(randomForest)

RF <- function(data=baseball.df, ntree=1000, nosplit = FALSE) {
  
  if (nosplit == TRUE) {
    
    t_start = Sys.time() #Start timer
    p=dim(data)[2]-1
    rf.data  =  randomForest(Wins~., data=data, 
                             mtry= floor(sqrt(p)), ntree=ntree, importance=TRUE)
    t_end = Sys.time() #End timer
    return(t_end - t_start)
  
  } else {
    
    t_start = Sys.time() #Start timer
    train = sort(sample(nrow(data), size = .8*nrow(data), rep=F)) #Random selection of 80% of indices
    p=dim(data)[2]-1
    rf.data  =  randomForest(Wins~., data=data, subset = train,
                             mtry= floor(sqrt(p)), ntree=ntree, importance=TRUE)
    t_end = Sys.time() #End timer
    
    x.train = data[train,]
    x.test = data[-train,]
    y.train      = data[train, "Wins"]
    y.test       = data[-train, "Wins"]
  
    (y.hat.test   = predict(rf.data, newdata = x.test))
    (y.bar.test <- as.data.frame(data[-train, "Wins"])[,1] %>% mean())
    (SSres.test <- sum((y.test-y.hat.test)^2))
    (SStot.test <- sum((y.test-y.bar.test)^2))
    (R2.test <- 1 - SSres.test/SStot.test)
  
    (y.hat.train <- predict(rf.data, newx = x.train))
    (y.bar.train <- as.data.frame(data[train, "Wins"])[,1] %>% mean())
    (SSres.train <- sum((y.train-y.hat.train)^2))
    (SStot.train <- sum((y.train-y.bar.train)^2))
    (R2.train <- 1 - SSres.train/SStot.train)
  
    R2 = data.frame(R2.train,R2.test)
    colnames(R2) = c("RF.Train.R2","RF.Test.R2")
  
    (resid.train = y.train - y.hat.train)
    (resid.test = y.test - y.hat.test)
  
    output = list(`RF` = rf.data,
                  `Test Residuals`=(resid.test),
                  `Train Residuals`=(resid.train),
                  `R2`=R2,
                  `Runtime` = t_end-t_start)
    return(output)
  }
}
