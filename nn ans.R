library(neuralnet)
library(caret)
library(gmodels)
install.packages("nnet")
library(nnet)


forest <- read.csv(file.choose())
View(forest)
attach(forest)
str(forest)
forest <- forest[,-c(1,2)]
View(forest)
summary(forest)
names(forest)

forest_norm <- scale(forest[,-29])
forest_norm <- cbind(forest_norm,forest$size_category)
colnames(forest_norm)[29] <-"size_category"
forest_norm <- as.data.frame(forest_norm)



forest_norm$size_category[forest_norm$size_category==2] <-0
forest_norm$size_category[forest_norm$size_category==1] <-1
forest_norm$size_category <- as.factor (forest_norm$size_category) 

forest_train <- forest_norm[1:350,]
forest_test <- forest_norm[351517,]

prop.table(table(forest_train$size_category))
prop.table(table(forest_test$size_category))



install.packages("neuralnet")
library(neuralnet)
#.........model building.........#
set.seed(101)
for_model <- neuralnet(size_category~.,hidden=2,act.fct = "logistic",
                       linear.output = FALSE,data = forest_train)
require(neuralnet)
require(MASS)
require(grid)
require(nnet)

sapply(forest,class)
forest$size_category <-as.numeric(as.character(forest$size_category))

formula_nn <- paste("size_category",paste(colnames(forest[-28]),collapse ="+"),sep="~")

model_size <- neuralnet(size_category~.,data = forest_train)
str(model_size) 
plot(model_size) 
model_result <- compute(model_size,forest_test[,1:28]) 
str(model_result) #structure#
predict_size<- model_result$net.result
predict_size <- as.data.frame(predict_size)
predict_size <- ifelse(predict_size$V1>0.5,0,1) 
predict_prob<- as.data.frame(predict_size) 
forest_test<-cbind(forest_test,predict_prob) 
