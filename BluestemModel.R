#' ---------------------------------------------------------------
#' @version 1.0.0
#' @title Bluestem Brands Inc., Data Assignment
#' 
#' @description 
#' This script is used to analyze the Bluestem Customer Response Rate data. 
#' 
#' @author Vijayan Nagarajan
#' ---------------------------------------------------------------

#' Loading libraries
library(zoo)
library(Hmisc)
library(caret)
library(usdm)
library(h2o)
library(ROCR)

#' Set working directory
setwd("D:\\Bluestem\\")

#' Read data 
fullData <- read.table("BluestemData.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# ' Temporaray data
train<- fullData[,-1254]

#' Date conversion
dateDF <- NULL
dateDF$mailDate <- format(as.POSIXct(as.Date(fullData$mail_date,origin="1960-01-02")),"%d-%m-%Y")
dateDF$birthDate <- format(as.Date(as.character(fullData$TU_BIRTHDATE), format="%Y%m%d"),"%d-%m-%Y")
dateDF$baseRecVerificationDate <- format(as.POSIXct(as.Date(fullData$BaseRecVerificationDate,origin="1960-01-02")),"%d-%m-%Y")
dateDF$homePurchaseDate <- format(as.Date(as.character(fullData$HomePurchaseDate), format="%Y%m%d"),"%d-%m-%Y")
dateDF$mortageSecondDate <- format(as.Date(as.character(fullData$Mortgage2ndMostRecentDate), format="%Y%m%d"),"%d-%m-%Y")
dateDF$mortageRecentDate <- format(as.Date(as.character(fullData$MortgageMostRecentDate), format="%Y%m%d"),"%d-%m-%Y")
dateDF$mortgagePurchaseDate <- format(as.Date(as.character(fullData$MortgagePurchaseDate), format="%Y%m%d"),"%d-%m-%Y")
dateDF$fileDate <- format(as.Date(as.yearmon(as.character(fullData$FileDate), "%Y%m"), frac = 1),"%d-%m-%Y")

#' MailDate preprocessing
train$mailDateYear <- format(as.yearmon(strptime(dateDF$mailDate, format = "%d-%m-%Y")) , "%Y")
train$mailDateMonth <- format(as.yearmon(strptime(dateDF$mailDate, format = "%d-%m-%Y")) , "%m")
train$mailDateDay <- weekdays(as.Date(dateDF$mailDate))
train$mailDateDay[train$mailDateDay == "Sunday"] <- 1
train$mailDateDay[train$mailDateDay == "Monday"] <- 2
train$mailDateDay[train$mailDateDay == "Tuesday"] <- 3
train$mailDateDay[train$mailDateDay == "Wednesday"] <- 4
train$mailDateDay[train$mailDateDay == "Thursday"] <- 5
train$mailDateDay[train$mailDateDay == "Friday"] <- 6
train$mailDateDay[train$mailDateDay == "Saturday"] <- 7
#train$mailDateDay <- as.factor(train$mailDateDay)
#train$mailDateYear <- as.factor(train$mailDateYear)
#train$mailDateMonth <- as.factor(train$mailDateMonth)
train$mailDateDay <- as.numeric(train$mailDateDay)
train$mailDateYear <- as.numeric(train$mailDateYear)
train$mailDateMonth <- as.numeric(train$mailDateMonth)


#' Remove mail date
train$mail_date <- NULL

#' Calculate weeks difference from mail date
train$birthWeekDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                           strptime(dateDF$birthDate, format = "%d-%m-%Y"),units="weeks"))
train$baseRecVerWeekDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                                strptime(dateDF$baseRecVerificationDate, format = "%d-%m-%Y"),units="weeks"))
train$homePurWeekDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                             strptime(dateDF$homePurchaseDate, format = "%d-%m-%Y"),units="weeks"))
train$morSecWeekDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                            strptime(dateDF$mortageSecondDate, format = "%d-%m-%Y"),units="weeks"))
train$morRecWeekDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                            strptime(dateDF$mortageRecentDate, format = "%d-%m-%Y"),units="weeks"))
train$morPurWeekDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                            strptime(dateDF$mortgagePurchaseDate, format = "%d-%m-%Y"),units="weeks"))
train$fileDateWeekDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                              strptime(dateDF$fileDate, format = "%d-%m-%Y"),units="weeks"))

#' Calculate hours difference from mail date
train$birthHourDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                           strptime(dateDF$birthDate, format = "%d-%m-%Y"),units="hours"))
train$baseRecVerHourDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                                strptime(dateDF$baseRecVerificationDate, format = "%d-%m-%Y"),units="hours"))
train$homePurHourDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                             strptime(dateDF$homePurchaseDate, format = "%d-%m-%Y"),units="hours"))
train$morSecHourDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                            strptime(dateDF$mortageSecondDate, format = "%d-%m-%Y"),units="hours"))
train$morRecHourDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                            strptime(dateDF$mortageRecentDate, format = "%d-%m-%Y"),units="hours"))
train$morPurHourDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                            strptime(dateDF$mortgagePurchaseDate, format = "%d-%m-%Y"),units="hours"))
train$fileHourDiff <- as.numeric(difftime(strptime(dateDF$mailDate, format = "%d-%m-%Y"),
                                          strptime(dateDF$fileDate, format = "%d-%m-%Y"),units="hours"))

#' Remove unwanted date variables 
train$TU_BIRTHDATE <- NULL
train$BaseRecVerificationDate <- NULL
train$HomePurchaseDate <- NULL
train$Mortgage2ndMostRecentDate <- NULL
train$MortgageMostRecentDate <- NULL
train$MortgagePurchaseDate <- NULL
train$FileDate <- NULL
dim(train)

#' Remove columns has high NA
train <- train[,colSums(is.na(train)) < 1500]
dim(train)
#dropped 52 columns

table(is.na(train))
#' Convert text columns to numeric and impute by mode
for (f in 1:length(colnames(train))) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]]))
    train[[f]] <- factor(train[[f]], levels=levels)
    train[[f]] <- impute(train[[f]])
    train[[f]] <- as.numeric(train[[f]])
  }
}
dim(train)
table(train$Language)
table(is.na(train))

#' Impute continuous variables by mean
train <- apply(train, 2, impute)

#' Data for clustering
clusterData <- train

#' Check for zero variance and remove zero variance columns
zero.var <- nearZeroVar(train, saveMetrics=TRUE)
View(zero.var)
dim(train)
train <- train[, !zero.var$zeroVar]
train <- as.data.frame(train)

#' Check for collinearity
#' Principal Component Analysis
train <- apply(train, 2, function(x) as.numeric(x))
pca <- prcomp(train[,-1])
summary(pca)
plot(pca, type = "l")
pca$sdev

#' Variation Inflation Factor
colVIF <- vif(train[,-1])
train <- as.data.frame(train[, !colVIF$VIF > 11])
dim(train)

#' Preparing train and test
train$NRR90 <- factor(train$NRR90)
flag <- createDataPartition(train$NRR90, p = .8, list = FALSE)
training <- train[flag,]
test <- train[-flag,]
#training <- train[c(1:1500),]
#test <- train[c(1501:2000),]
target <- test$NRR90
test <- test[,c(-1)]

#' Initialize and start H2o server
h2o.server <- h2o.init( nthreads= -1)

#' Convert data to H2o data format
train.hex <- as.h2o(train)

#' Feature selection
features <- colnames(train[,c(-1)])

#' Find important variables using models
#' GBM Model1
gbmF_model_1 <- h2o.gbm( x=features,
                         y = "NRR90",
                         training_frame =train.hex ,
                         max_depth = 3,
                         distribution = "bernoulli",
                         ntrees =500,
                         learn_rate = 0.05,
                         nbins_cats = 5891
)
gbmF_model_1
varImp <- gbmF_model_1@model$variable_importances
View(varImp)
length(varImp$variable[varImp$relative_importance > 0])
h2o.auc(gbmF_model_1)

#' GBM Model2
gbmF_model_2 <- h2o.gbm( x=features,
                         y = "NRR90",
                         training_frame =train.hex ,
                         max_depth = 3,
                         distribution = "bernoulli",
                         ntrees =400,
                         learn_rate = 0.04,
                         nbins_cats = 5400
)
gbmF_model_2
varImp2 <- gbmF_model_2@model$variable_importances
View(varImp2)
length(varImp2$variable[varImp2$relative_importance > 0])

#' Convert data H2o data format for model development
train.hex <- as.h2o(training)
test.hex <- as.h2o(test)

#' Feature selection
features <- colnames(train[,c(-1)])

#' GBM Model1
gbmF_model_1 <- h2o.gbm( x=features,
                         y = "NRR90",
                         training_frame =train.hex ,
                         max_depth = 3,
                         distribution = "bernoulli",
                         ntrees =500,
                         learn_rate = 0.05,
                         nbins_cats = 5891
)
gbmF_model_1
varImp <- gbmF_model_1@model$variable_importances
View(varImp)
length(varImp$variable[varImp$relative_importance > 0])

testgbm_1 <- as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex) )
table(target, testgbm_1$predict)
table(target)

#' GBM Model2
gbmF_model_2 <- h2o.gbm( x=features,
                         y = "NRR90",
                         training_frame =train.hex ,
                         max_depth = 3,
                         distribution = "bernoulli",
                         ntrees =600,
                         learn_rate = 0.04,
                         nbins_cats = 6400
)
gbmF_model_2
varImp2 <- gbmF_model_2@model$variable_importances
View(varImp2)
length(varImp2$variable[varImp2$relative_importance > 0])
testgbm_2 <- as.data.frame(h2o.predict(gbmF_model_2, newdata = test.hex) )
table(target, testgbm_2$predict)
table(target)

#' GLM Model 1
glm_model_1 <- h2o.glm(x = features,
                       y = "NRR90",
                       training_frame = train.hex,
                       solver = "L_BFGS",
                       family = "binomial",
                       link = "logit",
                       alpha = 0,
                       lambda_search = TRUE,
                       max_iterations = 500
)
glm_model_1
h2o.varimp(glm_model_1)
View(h2o.varimp(glm_model_1))
length(varImp$variable[varImp$relative_importance > 0])
testglm_1 <- as.data.frame(h2o.predict(glm_model_1, newdata = test.hex) )
table(target, testglm_1$predict)
table(target)

#' Deep Learning Model 1
dl_model_1 <- h2o.deeplearning( x=features,
                                y = "NRR90",
                                training_frame =train.hex ,
                                activation="Rectifier",
                                hidden=6,
                                epochs=60,
                                adaptive_rate =FALSE
)
dl_model_1
summary(dl_model_1)

testdl_model_1 <- as.data.frame(h2o.predict(dl_model_1, newdata = test.hex) )
table(target, testdl_model_1$predict)

#' Deep Learning Model 2
dl_model_2 <- h2o.deeplearning( x=features,
                                y = "NRR90",
                                training_frame =train.hex ,
                                activation = "Rectifier",
                                hidden=60,
                                epochs=40,
                                adaptive_rate =F
)
dl_model_2
summary(dl_model_2)
varImp4 <- dl_model_1@model$variable_importances
testdl_model_2 <- as.data.frame(h2o.predict(dl_model_2, newdata = test.hex) )
table(target, testdl_model_2$predict)

#' Deep Learning Model 3
dl_model_3 <- h2o.deeplearning( x=features,
                                y = "NRR90",
                                training_frame =train.hex 
                                #validation_frame =testHex ,
)

testdl_model_3 <- as.data.frame(h2o.predict(dl_model_3, newdata = test.hex) )
table(target, testdl_model_3$predict)

#' AUC curve
a <- prediction(testdl_model_3$p1, target)
perf <- performance(a, measure = "tpr", x.measure = "fpr")
auc <- performance(a, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#' Finding potential customer segments
custClusterData <- clusterData[, c("gtntu14_risk_sc", "GTNEQ14_SC", "FZ4.LengthOfResidence.06", 
                                  "mailDateMonth", "FZ4.GenerationsInHousehold.1",
                                  "GTNEQ14_RG", "FZ4.HomeMarketValue.N", "gtntu14_sc",
                                  "GTNEQ14_TM", "Factor10", "G102", "FZ4.EstimatedHouseholdIncome.K",
                                  "FZ4.HomeMarketValue.F", "CENSUS10149", "HIGHCRD_CONSUMERFINANCE",
                                  "NumberOfChildren")]
custClusterData <- as.data.frame(custClusterData)

#' K means
kmeans <- kmeans(custClusterData, 5)
plot(kmeans, col = kmeans$cluster)
library(useful)
plot.kmeans(kmeans, data = custClusterData)

