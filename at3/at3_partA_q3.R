# At3 Part A Q3
library(car)
library(tidyverse)
library(VGAM)
library(dummies)
library(rpart)
library(nnet)
# load and eda
skull_data = read.csv('egyptskull.csv')
head(skull_data)
str(skull_data)
unique(skull_data$Epoch)
aggregate(skull_data$Epoch, by=list(skull_data$Epoch), FUN=length)




scatterplotMatrix(
   ~ MB+BH+BL+NH  | as.factor(Epoch),
   skull_data,
  smooth = FALSE, regLine = FALSE)


pairs(skull_data)

plot(skull_data$MB, skull_data$BH, col=skull_data$Epoch)



# sample from each epoch 
train_data_index <- skull_data %>% 
  mutate(index = row_number()) %>% 
  select(Epoch, index) %>%
  group_by(Epoch) %>% 
  slice_sample(n=25) # %>% count()

skull_data['Train'] <- FALSE 
skull_data[train_data_index$index,]$Train <- TRUE

skull_data %>% filter(Train == FALSE) %>% group_by(Epoch) %>% count()
skull_data %>% filter(Train == TRUE) %>% group_by(Epoch) %>% count()

skull_train <- skull_data %>% filter(Train == TRUE)
skull_test <- skull_data %>% filter(Train == FALSE)

#### LDA ####

skulls_lda<-lda(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_train)
skulls_lda
plot(skulls_lda)

skull_test$lda_predict<-predict(skulls_lda,skull_test[,1:4])$class
skull_lda_confusion <- table(skull_test$Epoch,skull_test$lda_predict)
skull_lda_confusion
aggregate(skull_test$lda_predict, by=list(skull_test$lda_predict), FUN=length)
confusionMatrix(skull_lda_confusion)

aper <- sum(skull_test$Epoch != skull_test$lda_predict) / length(skull_test$Epoch) # APER (missclassification rate)
print('accuracy')
1 - aper # same accuracy we get from confusion Matrix function 

#### qda ####
skulls_qda<-qda(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_train)
skulls_qda
skull_test$qda_predict<-predict(skulls_qda,skull_test[,1:4])$class
skull_qda_confusion <- table(skull_test$Epoch,skull_test$qda_predict)
skull_qda_confusion
aggregate(skull_test$qda_predict, by=list(skull_test$qda_predict), FUN=length)
confusionMatrix(skull_qda_confusion)
aper <- sum(skull_test$Epoch != skull_test$qda_predict) / length(skull_test$Epoch) 
print('accuracy')
1 - aper


#### multinomial logistc regression ####

epoch <- skull_train$Epoch
epoch_dummies <- dummy(epoch, sep="_" )
skull_train <- cbind(skull_train, epoch_dummies)

skulls_vglm <- vglm(cbind(epoch_150,epoch_200,epoch_1850,epoch_3300,epoch_4000) ~ MB+BH+BL+NH,data=skull_train, family = 'multinomial')
skulls_vglm

vglm_predictions <- predict(skulls_vglm, newdata=skull_test[,1:4], type='response')
class_names <- colnames(vglm_predictions)


skull_test$vglm_predict <- apply(vglm_predictions, 1, function(p) as.integer(strsplit(class_names[which.max(p)],'_')[[1]][2]))
skull_vglm_confusion <- table(skull_test$Epoch,skull_test$vglm_predict)
aggregate(skull_test$vglm_predict, by=list(skull_test$vglm_predict), FUN=length)
confusionMatrix(skull_vglm_confusion)
aper <- sum(skull_test$Epoch != skull_test$qda_predict) / length(skull_test$Epoch) 
print('accuracy')
1 - aper


#### rpart -- recursive partitioning and regression trees ####
skulls_rpart<-rpart(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_train, 
                    method='class',
                    control = rpart.control(minsplit = 10),
                    model = TRUE)
skulls_rpart
skull_test$rpart_predict<-predict(skulls_rpart,skull_test[,1:4],type='class')
skull_rpart_confusion <- table(skull_test$Epoch,skull_test$rpart_predict)
skull_rpart_confusion
aggregate(skull_test$rpart_predict, by=list(skull_test$rpart_predict), FUN=length)
confusionMatrix(skull_rpart_confusion)
aper <- sum(skull_test$Epoch != skull_test$rpart_predict) / length(skull_test$Epoch) 
print('accuracy')
1 - aper

plot(skulls_rpart)
text(skulls_rpart,use.n = TRUE, all=TRUE, cex=.7)
plotcp(skulls_rpart)
#prune()


#### NNET ####
skulls_nnet<-nnet(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_train, 
                  size = 50, rang = 0.1,
                  decay = 5e-4, maxit = 200)

skulls_nnet
skull_test$nnet_predict<-as.integer(predict(skulls_nnet,skull_test[,1:4],type='class'))
skull_nnet_confusion <- table(skull_test$Epoch,skull_test$nnet_predict)
skull_nnet_confusion
aggregate(skull_test$nnet_predict, by=list(skull_test$nnet_predict), FUN=length)
confusionMatrix(skull_nnet_confusion)
aper <- sum(skull_test$Epoch != skull_test$rpart_predict) / length(skull_test$Epoch) 
print('accuracy')
1 - aper

plot(skulls_rpart)
text(skulls_rpart,use.n = TRUE, all=TRUE, cex=.7)
plotcp(skulls_rpart)
#prune()


#### comparing confusion matricies and error ####
print('accuracy and APER')  # actual error rate or actual error rate
print('lda')
confusionMatrix(skull_lda_confusion)$overall['Accuracy']
1 - confusionMatrix(skull_lda_confusion)$overall[['Accuracy']]
print('qda')
confusionMatrix(skull_qda_confusion)$overall['Accuracy']
1 - confusionMatrix(skull_qda_confusion)$overall[['Accuracy']]
print('vglm')
confusionMatrix(skull_vglm_confusion)$overall['Accuracy']
1 - confusionMatrix(skull_vglm_confusion)$overall[['Accuracy']]

print('rpart')
confusionMatrix(skull_rpart_confusion)$overall['Accuracy']
1 - confusionMatrix(skull_rpart_confusion)$overall[['Accuracy']]




#### partimat ####

partimat(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_data,method="lda") 
partimat(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_data,method="qda") 
partimat(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_data,method="rpart") 
partimat(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_data,method="rda") 
partimat(as.factor(Epoch) ~ MB+BH+BL+NH,data=skull_data,method="sknn") 



#### validation data #### 

eval_data <- data.frame(rbind(
  c(128,143,103,50), 
  c(129,126,91,50), 
  c(130,127,99,45), 
  c(130,131,98,53), 
  c(134,124,91,55), 
  c(130,130,104,49), 
  c(134,139,101,49), 
  c(136,133,91,49)))
colnames(eval_data)<- c('MB','BH','BL','NH')

eval_data$predicted_epoch <- predict(skulls_lda, eval_data)$class

eval_data
