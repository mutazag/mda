# Week 5 In class exercises

notes_data<-read.csv("C:/Documents/Notes.csv")

library(biotools)
boxM(notes_data[,1:6],notes_data[,7])

notes_data_R<-notes_data[notes_data$Status=="Real",1:6]
notes_data_F<-notes_data[notes_data$Status=="Counterfeit",1:6]

library(ICSNP)
HotellingsT2(notes_data_R,notes_data_F)

Xbar_1<-colMeans(notes_data_R)
S_1<-cov(notes_data_R)
n_1<-nrow(notes_data_R)
p<-ncol(notes_data_R)
n<-ncol(notes_data_R)

Xbar_2<-colMeans(notes_data_F)
S_2<-cov(notes_data_F)
n_2<-nrow(notes_data_F)

d_bar<-Xbar_1-Xbar_2
Sd<-(1)/(n_1)*S_1 + (1)/(n_2)*S_2

for(i in 1:p){
  lower<-round(d_bar[i]-sqrt(qchisq(0.95,p))*sqrt(Sd[i,i]),3)
  upper<-round(d_bar[i]+sqrt(qchisq(0.95,p))*sqrt(Sd[i,i]),3)
  cat(paste("The 95% CI for ",colnames(notes_data_F)[i]," is: (",lower,", ",upper,")\n"))
}


#MANOVA

car_data <- read.csv("C:/Documents/car.csv")

MakeModel,Price,mpg,rep78,rep77,hroom,rseat,trunk,weight,length,turn,displa,gratio	Origin
car_mod<-manova(cbind(MakeModel,Price,mpg,rep78,rep77,hroom,rseat,trunk,weight,length,turn,displa,gratio)~Origin,data=car_data)
print(car_mod)

summary.aov(car_mod)
summary(car_mod,intercept=TRUE,test="Wilks")




