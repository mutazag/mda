install.packages("MASS")
install.packages("klaR")

library(MASS)
library(klaR)



creditcard_data<-read.csv("C:/Documents/creditcard.csv")

test_rows<-sample(1:nrow(creditcard_data), round(nrow(creditcard_data)*0.2) ,replace=FALSE)

creditcard_data_training<-creditcard_data[-test_rows,]
creditcard_data_test<-creditcard_data[test_rows,]

creditcard_lda<-lda(A15~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14,data=creditcard_data_training)
creditcard_lda

creditcard_data_test$lda_predict<-predict(creditcard_lda,creditcard_data_test[,1:14])$class
table(creditcard_data_test$A15,creditcard_data_test$lda_predict)

partimat(as.factor(A15)~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14,data=creditcard_data_test,method="lda") 



creditcard_qda<-qda(A15~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14,data=creditcard_data_training)
creditcard_qda

creditcard_data_test$qda_predict<-predict(creditcard_qda,creditcard_data_test[,1:14])$class
table(creditcard_data_test$A15,creditcard_data_test$qda_predict)

partimat(as.factor(A15)~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14,data=creditcard_data_test,method="qda") 




wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
colnames(wine_data)<-c("Cult","Alc","MalAcid","Ash","AshAlk","Mag","TotPhen","Flav","NonFlav","Proant","Color","Hue","OD280OD315","Proline")            


test_rows<-sample(1:nrow(wine_data), round(nrow(wine_data)*0.2) ,replace=FALSE)

wine_data_training<-wine_data[-test_rows,]
wine_data_test<-wine_data[test_rows,]

wine_lda<-lda(Cult~Alc+MalAcid+Ash+AshAlk+Mag+TotPhen+Flav+NonFlav+Proant+Color+Hue+OD280OD315+Proline,data=wine_data_training)
wine_lda

wine_data_test$lda_predict<-predict(wine_lda,wine_data_test[,2:14])$class
table(wine_data_test$Cult,wine_data_test$lda_predict)

partimat(as.factor(Cult) ~ Alc+MalAcid+Ash+AshAlk+Mag+TotPhen+Flav+NonFlav+Proant+Color+Hue+OD280OD315+Proline,data=wine_data_test,method="lda") 



wine_qda<-qda(Cult~Alc+MalAcid+Ash+AshAlk+Mag+TotPhen+Flav+NonFlav+Proant+Color+Hue+OD280OD315+Proline,data=wine_data_training)
wine_qda

wine_data_test$qda_predict<-predict(wine_qda,wine_data_test[,2:14])$class
table(wine_data_test$Cult,wine_data_test$qda_predict)

partimat(as.factor(Cult) ~ Alc+MalAcid+Ash+AshAlk+Mag+TotPhen+Flav+NonFlav+Proant+Color+Hue+OD280OD315+Proline,data=wine_data_test,method="qda") 

