# At3 Part A Q3
library(car)
library(tidyverse)

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
