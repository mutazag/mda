
install.packages("MASS")
install.packages("klaR")

library(MASS)
library(klaR)
library(car)
library(caret)



notes_data<-read.csv("Notes.csv")
notes_data$Group<-substr(notes_data$Status,1,1)

count_by_class = aggregate(notes_data$Group, by=list(notes_data$Group), FUN=length)
count_by_class$prob = count_by_class$x / length(notes_data$Group)

count_by_class ## this will be the Prior probabilities og groups in the lda result 


scatterplotMatrix(
  ~ Length + Height + Height.1 + Inner.Frame + Inner.Frame.1 + Diagonal| as.factor(Group),
  notes_data,
  smooth = FALSE, regLine = FALSE)


### perform LDA 
notes_lda<-lda(Group ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
               +Diagonal,data=notes_data)
notes_lda
plot(notes_lda)

notes_data$lda_predict<-predict(notes_lda,notes_data[,1:6])$class


#confusion matrix, counting actual on rows and predicted on coln
notes_lda_confusion <- table(notes_data$Group,notes_data$lda_predict)
notes_lda_confusion
aggregate(notes_data$lda_predict, by=list(notes_data$lda_predict), FUN=length)

confusionMatrix(notes_lda_confusion)






## display partition matrix
partimat(as.factor(Group) ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
         +Diagonal,data=notes_data,method="lda") 




#### perform QDA 
notes_qda<-qda(Group ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
               +Diagonal,data=notes_data)
notes_qda



notes_data$qda_predict<-predict(notes_qda,notes_data[,1:6])$class
table(notes_data$Group,notes_data$qda_predict)

partimat(as.factor(Group) ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
         +Diagonal,data=notes_data,method="qda") 



notes_data_training<-notes_data[c(1:80,101:180),]
notes_data_test<-notes_data[c(81:100,181:200),]
notes_lda<-lda(Group ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
               +Diagonal,data=notes_data_training)
notes_lda

notes_data_test$lda_predict<-predict(notes_lda,notes_data_test[,1:6])$class
table(notes_data_test$Group,notes_data_test$lda_predict)

partimat(as.factor(Group) ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
         +Diagonal,data=notes_data_test,method="lda") 



notes_qda<-qda(Group ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
               +Diagonal,data=notes_data_training)
notes_qda

notes_data_test$qda_predict<-predict(notes_qda,notes_data_test[,1:6])$class
table(notes_data_test$Group,notes_data_test$qda_predict)

partimat(as.factor(Group) ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
         +Diagonal,data=notes_data_test,method="qda") 




notes_lda<-lda(Group ~ Length+Height+Height.1+Inner.Frame+Inner.Frame.1
               +Diagonal,data=notes_data_training,prior=c(0.95,0.05))
notes_lda

notes_data_test$lda_predict<-predict(notes_lda,notes_data_test[,1:6])$class
table(notes_data_test$Group,notes_data_test$lda_predict)
