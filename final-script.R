# case.data<-read.csv(file.choose(),stringsAsFactors = TRUE,header=TRUE, sep = ",")
# View(case.data)
# summary(case.data)

# case.data<-case.data[-1124,]
# case.data<-case.data[-1124,]
# 
# levels(case.data$deposit)
# is.na(case.data$deposit) <- !case.data$deposit
# case.data[is.na(case.data$deposit),"deposit"]<- median(case.data$deposit,na.rm = T)
# 
# case.data$job[case.data$job==""]<-NA
# levels(case.data$job) <- c(levels(case.data$job), "unknown")
# case.data[is.na(case.data$job),"job"]<- "unknown"
# 
# case.data$education[case.data$education==""]<-NA
# levels(case.data$education) <- c(levels(case.data$education), "unknown")
# case.data[is.na(case.data$education),"education"]<- "unknown"
# 
# is.na(case.data$balance) <- !case.data$balance
# case.data[is.na(case.data$balance),"balance"]<- median(case.data$balance,na.rm = T)
# 
# case.data$contacted[case.data$contacted==""]<-NA
# levels(case.data$contacted) <- c(levels(case.data$contacted), -1)
# case.data[is.na(case.data$contacted),"contacted"]<- -1
# 
# is.na(case.data$loanvalue) <- !case.data$loanvalue
# case.data[is.na(case.data$loanvalue),"loan"]<- mean(case.data$loanvalue,na.rm = T)
# hist(case.data$loanvalue)
# 
# summary(case.data)
# 
# case.data$RefNum <- as.character(as.numeric(case.data$RefNum))
# 
case.data$lead <- as.character(as.numeric(case.data$lead))
# case.data<-case.data[,-21]

# install.packages("binaryLogic")
# library(binaryLogic)
'''
encode_binary <- function(x, order = unique(x), name = "v_") {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x2 <- as.binary(x)
  maxlen <- max(sapply(x2, length))
  x2 <- lapply(x2, function(y) {
    l <- length(y)
    if (l < maxlen) {
      y <- c(rep(0, (maxlen - l)), y)
    }
    y
  })
  d <- as.data.frame(t(as.data.frame(x2)))
  colnames(d) <- paste0(name, 1:maxlen)
  d
}

new_df <- cbind(df, encode_binary(case.data[["job"]], name = "job_"))
new_df <- cbind(df, encode_binary(case.data[["loan"]], name = "loan_"))
head(new_df)
'''

'''
monthToNum <- function(x, order = unique(x), name = "v_") {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x2 <- as.binary(x)
  maxlen <- max(sapply(x2, length))
  x2 <- lapply(x2, function(y) {
    l <- length(y)
    if (l < maxlen) {
      y <- c(rep(0, (maxlen - l)), y)
    }
    y
  })
  d <- as.data.frame(t(as.data.frame(x2)))
  colnames(d) <- paste0(name, 1:maxlen)
  d
}

new_df <- cbind(df, encode_binary(case.data[["job"]], name = "job_"))
new_df <- cbind(df, encode_binary(case.data[["loan"]], name = "loan_"))
head(new_df)
'''

# mz<-case.data$month
# mz2<-paste(toupper(substr(mz, start = 1, stop = 1)),substr(mz, start = 2, stop = 3), sep="")
# View(mz2)
# mz2<-sapply(mz2,function(x) grep(paste("(?i)",x,sep=""),month.abb))
# mz2->case.data$month
# mz->case.data$monthL

## modeling ##


# Classification Tree with rpart

#packages required
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("pROC")
# 
# 
library(rpart) #CART
library(rpart.plot)
library(caTools)
library(pROC)

### STEP 1 - DATA ALREADY

### STEP 2 - SPLIT data into train and test - stratified sampling
set.seed(123)
newDataset <- sample.split(Y=case.data$lead, SplitRatio =0.65)
trainData <- case.data[newDataset,]
testData <- case.data[!newDataset,]

### STEP 3 - BUILD the model - fit a DT using training data
# seems to make no change - education, housing, month
# better curve w/o marital
# better curve with job, loan, balance, age
loss_matr <- matrix(c(0, 1.5, 1, 0), nrow = 2, byrow = TRUE)
# TN FP
# FN TP
# AUC
# 0 2 1 0 and 0 0 0 0 - 0.5415
print(loss_matr)
hist(case.data$deposit)
DTModel <- rpart(lead ~age+balance+job+loan, 
                 method = "class", data = trainData,
                 parms = list(split = "gini", loss = loss_matr),
                 control = rpart.control(maxdepth = 5, 
                                         minbucket = 15))

rpart.plot(DTModel, type = 3, extra = 101, fallen.leaves = F, cex = 0.7)
DTModel

### STEP 4 - USE the model to make predictions on test data
predTest <- predict(DTModel, testData, type = "class")
probTest <- predict(DTModel, testData, type = "prob")
View(probTest)
View(predTest)

actualTest <- testData$lead
actualTest[10:25]
predTest[10:25]
tail(actualTest)
tail(predTest)

testData$Actual <- actualTest
testData$Predictions <- predTest
testData$Probability0 <- probTest[,"0"]
testData$Probability1 <- probTest[,"1"]
View(testData)


# case.data$Probability <- ifelse(case.data$Probability0 > case.data$Probability1, case.data$Probability0,case.data$Probability1)
# View(case.data)
# write.csv(case.data,"predictedbnc.csv")

### STEP 5 - CALCULATE the accuracy
t1 <- table(Predicted_Value = predTest, Actual_Value = actualTest)
t1
accuracy <- sum(diag(t1))/sum(t1)
accuracy

### ROC and Area under the Curve
ROC <- roc(actualTest, probTest[,2])
plot(ROC, col="blue")
AUC <- auc(ROC)
AUC
