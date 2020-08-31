install.packages("car")
library(car)

##Swiss Notes data

notes_data<-read.csv("C:/Documents/Notes.csv")
notes_data$Group<-substr(notes_data$Status,1,1)
scatterplotMatrix(~Length+Height+Height.1+Inner.Frame+Inner.Frame.1+Diagonal | 
                    Group,data=notes_data)
cov(notes_data[,1:6])
notes_pc<-princomp(notes_data[,1:6])
summary(notes_pc, loadings = TRUE)
screeplot(notes_pc, npcs = 7, type = "lines")
biplot(notes_pc,xlabs=notes_data[,8],cex=c(0.8,1.4),expand=0.9,col=c("lightblue","black"))
biplot(notes_pc,choices=2:3,xlabs=notes_data[,8],cex=c(0.8,1.4),expand=0.9,col=c("lightblue","black"))
biplot(notes_pc,choices=c(1,3),xlabs=notes_data[,8],cex=c(0.8,1.4),expand=0.9,col=c("lightblue","black"))



notes_pc_sc<-princomp(notes_data[,1:6],scale=TRUE)
cor(notes_data[,1:6])
summary(notes_pc_sc, loadings = TRUE)
screeplot(notes_pc_sc, npcs = 7, type = "lines")
biplot(notes_pc_sc,xlabs=notes_data[,8],cex=c(0.8,1.4),expand=0.9,col=c("lightblue","black"))
biplot(notes_pc_sc,choices=2:3,xlabs=notes_data[,8],cex=c(0.8,1.4),expand=0.9,col=c("lightblue","black"))
biplot(notes_pc_sc,choices=c(1,3),xlabs=notes_data[,8],cex=c(0.8,1.4),expand=0.9,col=c("lightblue","black"))



##Boston Data

boston <- read.csv("C:/Documents/boston.csv")
boston<-boston[,-4]
boston$CRIM<-log(boston$CRIM)
boston$ZN<-boston$ZN/10
boston$INDUS<-log(boston$INDUS)
boston$NOX<-log(boston$NOX)
boston$RM<-log(boston$RM)
boston$AGE<-(boston$AGE)^2.5/10000
boston$DIS<-log(boston$DIS)
boston$RAD<-log(boston$RAD)
boston$TAX<-log(boston$TAX)
boston$PTRATIO<-exp(0.4*boston$PTRATIO)/1000
boston$B<-boston$B/1000
boston$LSTAT<-sqrt(boston$LSTAT)
boston$MEDV<-log(boston$MEDV)

boston_pc<-princomp(boston,scale=TRUE)
cor(boston)
summary(boston_pc, loadings = TRUE)
screeplot(boston_pc, type = "lines")
biplot(boston_pc,cex=c(0.8,1.4),
expand=0.9,col=c("lightblue","black"))
biplot(boston_pc,choices=2:3,cex=c(0.8,1.4),
expand=0.9,col=c("lightblue","black"))
biplot(boston_pc,choices=c(1,3),cex=c(0.8,1.4),
expand=0.9,col=c("lightblue","black"))

