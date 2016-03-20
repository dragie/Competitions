#' ---------------------------------------------------------------
#' @version 1.0.0
#' @title Capital One Labs Coding Challenge
#' 
#' @description 
#' This script is used to analyze the Capital One Labs' Coding Challenge data Test Part I. 
#' 
#' @author Vijayan Nagarajan
#' ---------------------------------------------------------------

#' Loading libraries
library(usdm)
library(Hmisc)
library(caret)
library(Rtsne)
library(corrplot)
library(h2o)

#' Set working directory
setwd("D:\\Capital One\\")

#' Read train and test data
train <- read.table("codetest_train.txt", header = TRUE, sep = "\t")
test <- read.table("codetest_test.txt", header = TRUE, sep = "\t")


#' Checking missing data and dimension
table(is.na(train))
table(is.na(test))
dim(train)
dim(test)

#' Finding alphabetics (text) columns 
textColumnsTrain <- which(apply(train, 2, function(x) any(grep("[:alpha:]", x))))
textColumnsTest <- which(apply(test, 2, function(x) any(grep("[:alpha:]", x))))

#' Converting text values to dummy values for training data
train$f_61 <- as.character(train$f_61)
train$f_61[train$f_61 == ""] <- "missing"
table(train$f_61 == "")
train$f_61 <- factor(train$f_61)
for(t in unique(train$f_61)) {
  train[paste("f_61",t,sep="")] <- ifelse(train$f_61==t,1,0)
}
train$f_121 <- as.character(train$f_121)
train$f_121[train$f_121 == ""] <- "missing"
table(train$f_121 == "")
train$f_121 <- factor(train$f_121)
for(t in unique(train$f_121)) {
  train[paste("f_121",t,sep="")] <- ifelse(train$f_121==t,1,0)
}
train$f_215 <- as.character(train$f_215)
train$f_215[train$f_215 == ""] <- "missing"
table(train$f_215 == "")
train$f_215 <- factor(train$f_215)
for(t in unique(train$f_215)) {
  train[paste("f_215",t,sep="")] <- ifelse(train$f_215==t,1,0)
}
train$f_237 <- as.character(train$f_237)
train$f_237[train$f_237 == ""] <- "missing"
table(train$f_237 == "")
train$f_237 <- factor(train$f_237)

for(t in unique(train$f_237)) {
  train[paste("f_237",t,sep="")] <- ifelse(train$f_237==t,1,0)
}

#' Removing existing text columns in train data
train$f_61 <- NULL
train$f_121 <- NULL
train$f_215 <- NULL
train$f_237 <- NULL

#' Converting text values to dummy values for testing data
test$f_61 <- as.character(test$f_61)
test$f_61[test$f_61 == ""] <- "missing"
table(test$f_61 == "")
test$f_61 <- factor(test$f_61)
for(t in unique(test$f_61)) {
  test[paste("f_61",t,sep="")] <- ifelse(test$f_61==t,1,0)
}
test$f_121 <- as.character(test$f_121)
test$f_121[test$f_121 == ""] <- "missing"
table(test$f_121 == "")
test$f_121 <- factor(test$f_121)
for(t in unique(test$f_121)) {
  test[paste("f_121",t,sep="")] <- ifelse(test$f_121==t,1,0)
}
test$f_215 <- as.character(test$f_215)
test$f_215[test$f_215 == ""] <- "missing"
table(test$f_215 == "")
test$f_215 <- factor(test$f_215)
for(t in unique(test$f_215)) {
  test[paste("f_215",t,sep="")] <- ifelse(test$f_215==t,1,0)
}
test$f_237 <- as.character(test$f_237)
test$f_237[test$f_237 == ""] <- "missing"
table(test$f_237 == "")
test$f_237 <- factor(test$f_237)

for(t in unique(test$f_237)) {
  test[paste("f_237",t,sep="")] <- ifelse(test$f_237==t,1,0)
}

#' Removing existing text columns in test data
test$f_61 <- NULL
test$f_121 <- NULL
test$f_215 <- NULL
test$f_237 <- NULL

#' Impute missing values by mean
for(i in 1:length(train)){
  #train[[i]] <- impute(train[[i]])
  train[[i]][is.na(train[[i]])] <- mean(train[[i]], na.rm=TRUE)
}
train <- as.data.frame(train)
table(is.na(train))

for(i in 1:length(test)){
  #test[[i]] <- impute(test[[i]])
  test[[i]][is.na(test[[i]])] <- mean(test[[i]], na.rm=TRUE)
}
test <- as.data.frame(test)
table(is.na(test))

#' Variance and collinearity analysis
zero.var <- nearZeroVar(train, saveMetrics=TRUE)
zero.var
correlations <- cor(train)
colVIF <- vif(train[,-1])

#Removing highly correlated features
for(i in 252:273){
  train[[i]] <- NULL
}
for(i in 251:272){
  test[[i]] <- NULL
}
# t-Distributed Stochastic Neighbor Embedding
tsne <- Rtsne(as.matrix(train), check_duplicates=FALSE, pca=TRUE, 
             perplexity=30, theta=0.5, dims=2)
embedding <- as.data.frame(tsne$Y)
g <- ggplot(embedding, aes(x=V1, y=V2)) +
  geom_point(size=1.25) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE 2D Embedding") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
print(g)

#corrplot.mixed(cor(apply(train, 2, as.numeric)), lower="circle", upper="color", 
#               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")

#' Removing existing and initializing new H2o high peroformance clusters 
#h2o.removeAll()
h2o.server <- h2o.init( nthreads= -1)

#' Convert data H2o data format
train.hex <- as.h2o(train)
test.hex <- as.h2o(test)

#' Feature selection
features <- colnames(train[,-1])

#' GBM Model1
gbmF_model_1 <- h2o.gbm( x=features,
                        y = "target",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =500,
                        learn_rate = 0.05,
                        nbins_cats = 5891
)
gbmF_model_1
varImp <- gbmF_model_1@model$variable_importances
View(varImp)

#' GBM Model2
gbmF_model_2 <- h2o.gbm( x=features,
                        y = "target",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =600,
                        learn_rate = 0.06,
                        nbins_cats = 5891
)

gbmF_model_2
varImp2 <- gbmF_model_2@model$variable_importances

#' Deep Learning Model 1
dl_model_1 <- h2o.deeplearning( x=features,
                               y = "target",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=60,
                               adaptive_rate =FALSE
)

dl_model_1
summary(dl_model_1)
varImp3 <- dl_model_1@model$variable_importances

#' Deep Learning Model 2
dl_model_2 <- h2o.deeplearning( x=features,
                      y = "target",
                      training_frame =train.hex ,
                      activation = "Rectifier",
                      hidden=60,
                      epochs=40,
                      adaptive_rate =F
)

dl_model_2
summary(dl_model_2)
varImp4 <- dl_model_1@model$variable_importances

#' Deep Learning Model 3
dl_model_3 <- h2o.deeplearning( x=features,
                      y = "target",
                      training_frame =train.hex 
                      #validation_frame =testHex ,
)

dl_model_3
summary(dl_model_3)
varImp5 <- dl_model_3@model$variable_importances

#' Predictions
testgbm_1 <- as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex) )
testgbm_2 <- as.data.frame(h2o.predict(gbmF_model_2, newdata = test.hex) )

testdl_model_1 <- as.data.frame(h2o.predict(dl_model_1, newdata = test.hex) )
testdl_model_2 <- as.data.frame(h2o.predict(dl_model_2, newdata = test.hex) )
testdl_model_3 <- as.data.frame(h2o.predict(dl_model_3, newdata = test.hex) )

testgbm_1$predict <- ifelse(testgbm_1$predict<0,0,testgbm_1$predict)
testgbm_2$predict <- ifelse(testgbm_2$predict<0,0,testgbm_2$predict)

testdl_model_1$predict <- ifelse(testdl_model_1$predict<0,0,testdl_model_1$predict)
testdl_model_2$predict <- ifelse(testdl_model_2$predict<0,0,testdl_model_2$predict)
testdl_model_3$predict <- ifelse(testdl_model_3$predict<0,0,testdl_model_3$predict)

#' Ensembling the predicted outputs
finalPredict <- 0.15*(testdl_model_1$predict)+
  0.3*(testdl_model_2$predict)+
  0.25*(testdl_model_3$predict)+
  0.15*(testgbm_1$predict)+
  0.15*(testgbm_2$predict)

#' Output file creation
write.csv(finalPredict, "TestPrediction.csv", row.names = FALSE) 


