turtle <- read.csv("C:/Documents/Turtles.csv")
turtle_pc<-princomp(turtle[,1:3],scale=TRUE)
cor(turtle[,1:3])
summary(turtle_pc, loadings = TRUE)
screeplot(turtle_pc, type = "lines")
biplot(turtle_pc,xlabs=turtle[,4],cex=c(0.8,1.4),expand=0.9,col=c("lightblue","black"))

activities <- read.csv("C:/Documents/Activities.csv")
activities_pc<-princomp(activities,scale=TRUE)
cor(activities)
summary(activities_pc, loadings = TRUE)
screeplot(activities_pc, type = "lines")
biplot(activities_pc,cex=c(0.8,1.4),expand=0.9,col=c("lightblue","black"))

