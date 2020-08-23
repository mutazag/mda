install.packages("car")
install.packages("MVN")
install.packages("ICSNP")
install.packages("biotools")
install.packages("mvnormtest")

# Wastewater treatment plants are requires to monitor their discharges on a regular basis. 
# 11 samples were taken to a commercial lab and to the Wisconcin state lab of hygiene, and 
# measurements of biochemical oxygen demand and suspended solids were taken. Were the measurements 
# different? Find a confidence interval for the difference.

BOD_C<-c(6, 6, 18, 8, 11, 34, 28, 71, 43, 33, 20)
SS_C<-c(27, 23, 64, 44, 30, 75, 26, 124, 54, 30, 14)
BOD_S<-c(25, 28, 36, 35, 15, 44, 42, 54, 34, 29, 39)
SS_S<-c(15, 13, 22, 29, 31, 64, 30, 64, 56, 20, 21)

Comm<-cbind(BOD_C,SS_C)
State<-cbind(BOD_S,SS_S)

Diff<-Comm-State
colnames(Diff)<-c("BOD","SS")


library(car)
scatterplotMatrix(~ BOD+SS, data=Diff, 
                  smooth=FALSE, reg.line=FALSE, ellipse=TRUE, by.groups=FALSE, 
                  diagonal="none")


library(MVN)
mvtest<-roystonTest(Diff,qqplot=TRUE)
mvtest
mvnPlot(mvtest,type="contour")


library(ICSNP)
wastetest<-HotellingsT2(Diff, mu=c(0,0))
wastetest

#Individually
t.test(Diff[,1],mu=0)
t.test(Diff[,2],mu=0)


mean_D<-colMeans(Diff)
S_D<-cov(Diff)
p<-ncol(Diff)
n<-nrow(Diff)

for(i in 1:p){
  lower<-round(mean_D[i]-sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p))*sqrt(S_D[i,i]/n),3)
  upper<-round(mean_D[i]+sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p))*sqrt(S_D[i,i]/n),3)
  cat(paste("The 95% CI for ",colnames(Diff)[i]," is: (",lower,", ",upper,")\n"))
}



# Skulls Data

Length<-c(190.5, 172.5, 167., 169.5, 175., 177.5, 179.5, 179.5, 173.5, 
            162.5, 178.5, 171.5, 180.5, 183., 169.5, 172., 170., 182.5, 
            179.5, 191., 184.5, 181., 173.5, 188.5, 175., 196., 200., 185., 
            174.5, 195.5, 197., 182.5)
Breadth<- c(152.5, 132., 130., 150.5, 138.5, 142.5, 142.5, 138., 135.5, 
            139.,135., 148.5, 139., 149., 130., 140., 126.5, 136., 135., 
            140.5, 141.5, 142., 136.5, 130., 153., 142.5, 139.5, 134.5, 
            143.5, 144., 131.5, 131.)
Height <- c(145., 125.5, 125.5, 133.5, 126., 142.5, 127.5, 133.5, 130.5, 
            131., 136., 132.5, 132., 121.5, 131., 136., 134.5, 138.5, 
            128.5, 140.5, 134.5, 132.5, 126., 143., 130., 123.5, 143.5, 
            140., 132.5, 138.5, 135., 135.)
Fheight <- c(73.5, 63., 69.5, 64.5, 77.5, 71.5, 70.5, 73.5, 70., 62., 71., 
             65., 74.5, 76.5, 68., 70.5, 66., 76., 74., 72.5, 76.5, 79., 
             71.5, 79.5, 76.5, 76., 82.5, 81.5, 74., 78.5, 80.5, 68.5)
Fbreadth <- c(136.5, 121., 119.5, 128., 135.5, 131., 134.5, 132.5, 133.5, 
              126., 124., 146.5, 134.5, 142., 119., 133.5, 118.5, 134., 
              132., 131.5, 141.5, 136.5, 136.5, 136., 142., 134., 146., 
              137., 136.5, 144., 139., 136.)
Type<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
Tibet <- data.frame(Length,Breadth,Height,Fheight,Fbreadth,Type) 



library(biotools)

boxM(Tibet[,1:5],Tibet[,6])

Tibet_S<-Tibet[Tibet$Type==1,1:5]
Tibet_L<-Tibet[Tibet$Type==2,1:5]

HotellingsT2(Tibet_S,Tibet_L)

Xbar_S<-colMeans(Tibet_S)
S_S<-cov(Tibet_S)
n_S<-nrow(Tibet_S)
p<-ncol(Tibet_S)
n<-ncol(Tibet_S)

Xbar_L<-colMeans(Tibet_L)
S_L<-cov(Tibet_L)
n_L<-nrow(Tibet_L)

d_bar<-Xbar_S-Xbar_L
Sd<-(n_S - 1)/(n_S + n_L - 2)*S_S + (n_L - 1)/(n_S + n_L - 2)*S_L

for(i in 1:p){
  lower<-round(d_bar[i]-sqrt(p*(n_S+n_L-2)/(n_S+n_L-p-1)*qf(0.95,p,n_S+n_L-p-1))
               *sqrt(Sd[i,i]*(1/n_S+1/n_L)),3)
  upper<-round(d_bar[i]+sqrt(p*(n_S+n_L-2)/(n_S+n_L-p-1)*qf(0.95,p,n_S+n_L-p-1))
               *sqrt(Sd[i,i]*(1/n_S+1/n_L)),3)
  cat(paste("The 95% CI for ",colnames(Tibet_L)[i]," is: (",lower,", ",upper,")\n"))
}



# Wine data - 2 samples

wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
colnames(wine)<-c("Cult","Alc","MalAcid","Ash","AshAlk","Mag","TotPhen","Flav","NonFlav","Proant","Color","Hue","OD280OD315","Proline")            

trans<-powerTransform(wine[2:14])
wine2<-wine
wine2[2:14]<-bcPower(wine[2:14],trans$lambda)
wine3<-wine2[wine2$Cult==1|wine2$Cult==2,]

boxM(wine3[,2:14],wine3[,1])

wine31<-wine3[wine3$Cult==1,2:14]
wine32<-wine3[wine3$Cult==2,2:14]

HotellingsT2(wine31,wine32)

Xbar_1<-colMeans(wine31)
S_1<-cov(wine31)
n_1<-nrow(wine31)
p<-ncol(wine31)
n<-ncol(wine31)

Xbar_2<-colMeans(wine32)
S_2<-cov(wine32)
n_2<-nrow(wine32)

d_bar<-Xbar_1-Xbar_2
Sd<-(1)/(n_1)*S_1 + (1)/(n_2)*S_2

for(i in 1:p){
  lower<-round(d_bar[i]-sqrt(qchisq(0.95,p))*sqrt(Sd[i,i]),3)
  upper<-round(d_bar[i]+sqrt(qchisq(0.95,p))*sqrt(Sd[i,i]),3)
  cat(paste("The 95% CI for ",colnames(wine32)[i]," is: (",lower,", ",upper,")\n"))
}


#MANOVA

apply(wine2,2,function(x) by(x,wine$Cult,summary, basic=FALSE))
wine2$Cult<-as.factor(wine2$Cult)
mmod<-manova(cbind(Alc,MalAcid,Ash,AshAlk,Mag,TotPhen,Flav,NonFlav,Proant,Color,Hue,OD280OD315,Proline)~Cult,data=wine2)
print(mmod)

summary.aov(mmod)
summary(mmod,intercept=TRUE,test="Wilks")

resid<-residuals(mmod)
roystonTest(resid,qqplot=TRUE)

fitIII <- lm(cbind(Alc,MalAcid,Ash,AshAlk,Mag,TotPhen,Flav,NonFlav,
                   Proant,Color,Hue,OD280OD315,Proline) ~ Cult, data=wine2,
             contrasts=list(Cult="contr.sum"))
mmod <- Manova(fitIII, type="III")
mmod_sum<-summary(mmod, multivariate=TRUE)

mmod_sum


meanvec<-t(apply(wine2[2:14],2,function(x) by(x,wine$Cult,mean)))
wmat<-mmod_sum$SSPE
p<-13
g<-3
nvec<-as.matrix(table(wine$Cult))
ntot<-sum(nvec)
alpha<-0.05

# tau_2-tau_1
ind1<-2
ind2<-1
for(i in 1:p){
  lower<-round(meanvec[i,ind1]-meanvec[i,ind2]-qt(1-alpha/(p*g*(g-1)),ntot-g)*sqrt(wmat[i,i]/(ntot-g)*(1/nvec[ind1,1]+1/nvec[ind2,1])),3)
  upper<-round(meanvec[i,ind1]-meanvec[i,ind2]+qt(1-alpha/(p*g*(g-1)),ntot-g)*sqrt(wmat[i,i]/(ntot-g)*(1/nvec[ind1,1]+1/nvec[ind2,1])),3)
  cat(paste("The 95% CI for ",colnames(wine)[i]," is: (",lower,", ",upper,")\n"))
}

lower<-round(meanvec[1,ind1]-meanvec[1,ind2]-qt(1-alpha/(p*g*(g-1)),ntot-g)*sqrt(wmat[1,1]/(ntot-g)*(1/nvec[ind1,1]+1/nvec[ind2,1])),3)
upper<-round(meanvec[1,ind1]-meanvec[1,ind2]+qt(1-alpha/(p*g*(g-1)),ntot-g)*sqrt(wmat[1,1]/(ntot-g)*(1/nvec[ind1,1]+1/nvec[ind2,1])),3)


# tau_3-tau_1
ind1<-3
ind2<-1
for(i in 1:p){
  lower<-round(meanvec[i,ind1]-meanvec[i,ind2]-qt(1-alpha/(p*g*(g-1)),ntot-g)*sqrt(wmat[i,i]/(ntot-g)*(1/nvec[ind1,1]+1/nvec[ind2,1])),3)
  upper<-round(meanvec[i,ind1]-meanvec[i,ind2]+qt(1-alpha/(p*g*(g-1)),ntot-g)*sqrt(wmat[i,i]/(ntot-g)*(1/nvec[ind1,1]+1/nvec[ind2,1])),3)
  cat(paste("The 95% CI for ",colnames(wine)[i]," is: (",lower,", ",upper,")\n"))
}

# tau_3-tau_2
ind1<-3
ind2<-2
for(i in 1:p){
  lower<-round(meanvec[i,ind1]-meanvec[i,ind2]-qt(1-alpha/(p*g*(g-1)),ntot-g)*sqrt(wmat[i,i]/(ntot-g)*(1/nvec[ind1,1]+1/nvec[ind2,1])),3)
  upper<-round(meanvec[i,ind1]-meanvec[i,ind2]+qt(1-alpha/(p*g*(g-1)),ntot-g)*sqrt(wmat[i,i]/(ntot-g)*(1/nvec[ind1,1]+1/nvec[ind2,1])),3)
  cat(paste("The 95% CI for ",colnames(wine)[i]," is: (",lower,", ",upper,")\n"))
}

