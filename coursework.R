protein <- read.csv('protein.csv', header = T)
attach(protein)
##(a)##
summary(protein)
tapply(RedMeat,Country,mean)
tapply(WhiteMeat,Country,median)
tapply(Eggs,Country,max)
tapply(Milk,Country,min)
tapply(Fish,Country,range)
tapply(Cereals,Country,quantile)
IQR(Starch)  
sd(Nuts) 
quantile(Fr.Veg,probs = 0.25)  
#sd(RedMeat)  #3.347078
#var(WhiteMeat)   #13.6423
#range(Eggs)  #0.5 4.7
#IQR(Milk)   #12.2
#min(Fish)  #0.2
#max(Cereals) #56.7

##(b)## 
plot(protein,col=Country)
par(family=("Heiti TC Light"))
par(mfrow=c(3,1))
## RedMeat, WhiteMeat, Eggs ##
for (i in 2:4){
  barplot(protein[,i], main = names(protein)[i])
  countryNames <- as.vector(Country)
  axis(1,at=1:25,labels=countryNames, las=2)}
## Milk, Fish, Cereals ##
for (i in 5:7){
  barplot(protein[,i], main = names(protein)[i])
  countryNames <- as.vector(Country)
  axis(1,at=1:25,labels=countryNames, las=2)}
## Starch, Nuts ##
for (i in 8:9){
  barplot(protein[,i], main = names(protein)[i])
  countryNames <- as.vector(Country)
  axis(1,at=1:25,labels=countryNames, las=2)}

##(c)##
cor(Fr.Veg,protein[,2:9])

##(d)##
par(mfrow=c(4,2))
fd_cat <- as.vector(2:9)
for (k in fd_cat){
  plot(Fr.Veg, protein[,k],ylab = names(protein)[k])}

##(e)##
for (i in fd_cat){
  print(paste("95% Confidence interval for", names(protein)[i]))
  print(t.test(protein[,i],conf.level = 0.95)$conf.int)
}

##(f)##
t.test(Starch,Nuts,var.equal = T)

###Question 2###
DP <- read.csv('DartPoints.csv', header = T)
#DP <- na.omit(DP)
attach(DP)
##(a)##
summary(DP)
z <- scale(DP[,3:9])
summary(z)

##(b)##
par(mfrow=c(3,2))
#DP[,3]
for (j in 4:9){
  plot(Length, DP[,j], ylab =names(DP)[j])
  abline(lm(DP[,j]~Length))}

##(c)##
for (p in 4:9){
  print(names(DP)[p])
  print(cor(Length,DP[,p],use = "complete.obs"))}

##(d)##
Str <- which(Blade.Sh=='S')
Exc <- which(Blade.Sh=='E')
Inc <- which(Blade.Sh=='I')
Rec <- which(Blade.Sh=='R')

S <- Weight[Str]/length(Str)
E <- Weight[Exc]/length(Exc)
I <- Weight[Inc]/length(Inc)
R <- Weight[Rec]/length(Rec)

boxplot(S,E,I,R,xlab='types of Blade Shape', ylab='freq'
        , main='Relative Frequency Distribution of Weight in Different Blade Shape'
        , names = c('Straight','Excurvate','Incurvate','Recurvate'))
sh <- c(S,E,I,R)
barplot(sh)

data.frame('X'=Str,'Weight'=Weight[Str],'Straight'=S)
data.frame('X'=Exc, 'Weight'=Weight[Exc],'Excurvate'=E)
data.frame('X'=Inc, 'Weight'=Weight[Inc], 'Incurvate'=I)
data.frame('X'=Rec, 'Weight'=Weight[Rec], 'Recurvate'=R)
#############################################################################
str <- data.frame('Weight'=Weight[Str])
exc <- data.frame('Weight'=Weight[Exc])
inc <- data.frame('Weight'=Weight[Inc])
rec <- data.frame('Weight'=Weight[Rec])

quantile(Weight,0.25)   #4.55
quantile(Weight,0.5)   #6.8
quantile(Weight,0.75)  #10.05

weight_class<-rep(NA,dim(str)[1])
par(mfrow=c(2,2))
weight_class[which(str<4.55)]<-"<25%"
weight_class[which(str>=4.55 & str<6.8)]<-"25%~50%"
weight_class[which(str>=6.8 & str<10.05)]<-"50%~75%"
weight_class[which(str>=10.05)]<-"75% ~ "
f <- table(weight_class)/length(weight_class)
barplot(f, main = 'relative freq distribution of Weight in Straight')
weight_class

weight_class<-rep(NA,dim(exc)[1])
weight_class[which(exc<4.55)]<-"< 25%"
weight_class[which(exc>=4.55 & exc<6.8)]<-"25%~50%"
weight_class[which(exc>=6.8 & exc<10.05)]<-"50%~75%"
weight_class[which(exc>=10.05)]<-"75% ~"
f <- table(weight_class)/length(weight_class)
barplot(f, main = 'relative freq distribution of Weight in Excurvate')

weight_class<-rep(NA,dim(inc)[1])
weight_class[which(inc<4.55)]<-"< 25%"
weight_class[which(inc>=4.55 & inc<6.8)]<-"25%~50%"
weight_class[which(inc>=6.8 & inc<10.05)]<-"50%~75%"
weight_class[which(inc>=10.05)]<-"75% ~"
f <- table(weight_class)/length(weight_class)
barplot(f, main = 'relative freq distribution of Weight in Incurvate')

weight_class<-rep(NA,dim(rec)[1])
weight_class[which(rec<4.55)]<-"< 25%"
weight_class[which(rec>=4.55 & rec<6.8)]<-"25%~50%"
weight_class[which(rec>=6.8 & rec<10.05)]<-"50%~75%"
weight_class[which(rec>=10.05)]<-"75% ~"
f <- table(weight_class)/length(weight_class)
barplot(f, main = 'relative freq distribution of Weight in Recurvate')

###Question 03###
# 1-Length 0.879953, 2-Width 0.8263948, 3-Thickness 0.6001054, 
# 4-J.Width 0.5025996, 5-H.Length 0.486397
for (p in 3:8){
  print(names(DP)[p])
  print(cor(Weight,DP[,p],use = "complete.obs"))}
#cor(DP[,3:9],use = "complete.obs")

par(mfrow=c(3,2))
for (j in 3:8){
  plot(Weight, DP[,j], ylab =names(DP)[j])
  abline(lm(DP[,j]~Weight))}
## variable selection ##  
summary(lm(Weight~., data = DP))   #Weight 與全部變數：R-Square 0.9147, p:3.226e-12
#############################################################################
#當我們建立出一個線性回歸時，必須要確認其殘差(residual)是否符合下面三個假設：
#常態性(Normality)、獨立性(Independence)、變異數同質性(Homogeneity of Variance)
#############################################################################
## linear regression ## 不要加入thickness ##

lm6 <- lm(Weight~Length+J.Width)  #bad
summary(lm6)  #R-squared:  0.7877, p-value: < 2.2e-16, rse:1.971
anova(lm6)

lm7 <- lm(Weight~Length+H.Length)  #bad: big rse, low R^2
summary(lm7)  #R-squared:  0.7763, p-value: < 2.2e-16, rse:2.012
anova(lm7)
## --------------------------------------------------------------
lm1 <- lm(Weight~Length)   #highly relevant
summary(lm1) #R-Square:0.7743, p:<2.2e-16 rse:2.01, 
anova(lm1)

lm2 <- lm(Weight~Width)  #worse than lm1
summary(lm2)  #R-squared: 0.6829, p: <2.2e-16, rse:2.382
anova(lm2)

lm3 <- lm(Weight~Thickness)   #worse
summary(lm3)   #R-squared: 0.3601, p-value:3.242e-10, se:3.384
anova(lm3)

lm4 <- lm(Weight~Length+Width)  #better than lm1
summary(lm4) #R-squared: 0.8292  p-value: < 2.2e-16, se:1.759
anova(lm4)

lm5 <- lm(Weight~Length+Thickness)  #delete thickness
summary(lm5) #R-squared:  0.7845, p-value: < 2.2e-16, rse:1.975
anova(lm5)

lm8 <- lm(Weight~Length+Width+Thickness)
summary(lm8)  #R-squared:  0.8328, p-value: < 2.2e-16, rse:1.75
anova(lm8)  #F:0.1747
## --------------------------------------------------------------
#summary(lm(Weight~Thickness))  #R-squared:  0.3601, p: 3.242e-10        #  #
#summary(lm(Weight~J.Width))  #R-squared:  0.2526, p: 4.468e-07           #
#summary(lm(Weight~H.Length))  #R-squared:  0.2366, p-value: 1.018e-06  #   #
#############################################################################
# One-way Anova(單因子變異數分析)是只有一個類別變數當作independent variable，
# 檢驗此類別變數與其它連續變數(continuous variable)之間的關係。
# Two-way Anova(雙因子變異數分析)是有兩個以上的類別變數作為independent variables。
#############################################################################
## 7種類別
Str <- which(Blade.Sh=='S')
Exc <- which(Blade.Sh=='E')
Inc <- which(Blade.Sh=='I')
Rec <- which(Blade.Sh=='R')
Blade.Sh[Str]
data<-read.csv("plants.csv",header=TRUE)
attach(data)
Name<-as.factor(Name)
Blade.Sh<-as.factor(Blade.Sh)
Base.Sh<-as.factor(Base.Sh)
Should.Sh<-as.factor(Should.Sh)
Should.Or<-as.factor(Should.Or)
Haft.Sh<-as.factor(Haft.Sh)
Haft.Or<-as.factor(Haft.Or)

atw1<-lm(Weight~Name*Blade.Sh) #3NamePedernales 2NameTravis 
summary(atw1)  #R-squared:  0.4823, p-value: 9.983e-07, error: 3.304

atw2 <- lm(Weight~Name*Base.Sh)  #3NamePedernales  1Travis .Wells
summary(atw2)  #R-squared:  0.466, p-value: 2.676e-06, error: 3.355

atw3<-lm(Weight~Name*Should.Sh)  #3Wells 1Pedernales Travis
summary(atw3)    #R-squared:  0.4801, p-value: 1.145e-06, error: 3.311
 
atw4<-lm(Weight~Name*Should.Or)   ##highest!
summary(atw4)

atw5<-lm(Weight~Name*Haft.Sh)    #3Pedernales 2Travis Wells 
summary(atw5)   #R-squared:  0.4468   p-value: 8.136e-06  error: 3.415

atw6<-lm(Weight~Name*Haft.Or)   #1Pedernales .Wells
summary(atw6)   #R-squared:  0.4542, p-value: 5.341e-06, error: 3.393

atw7<-lm(Weight~Blade.Sh*Base.Sh)  ##worst! 0.1187
summary(atw7)

atw8<-lm(Weight~Blade.Sh*Should.Sh)   ## bad! 0.1497
summary(atw8)

atw9<-lm(Weight~Blade.Sh*Should.Or)   ## bad! 0.2356
summary(atw9)

atw10<-lm(Weight~Blade.Sh*Haft.Sh)   ## bad! 0.1525
summary(atw10)

atw11<-lm(Weight~Blade.Sh*Haft.Or)   #1Blade.shS .HaftOrE Blade.ShR:Haft.OrP Blade.ShS:Haft.OrT
summary(atw11)   # 0.3482, 0.001072, 3.707

atw12<-lm(Weight~Base.Sh*Should.Sh)  ## bad! 0.14
summary(atw12)

atw13<-lm(Weight~Base.Sh*Should.Or)   #3Base.ShI:Should.OrT 2Base.ShI Base.ShI:Should.OrH Base.ShI:Should.OrX 
summary(atw13)   #0.3222, 0.0004286, 3.707

atw14<-lm(Weight~Base.Sh*Haft.Sh)    ## bad! 0.1008
summary(atw14)

atw15<-lm(Weight~Base.Sh*Haft.Or)   ##1Base.ShI  Haft.OrT Base.ShI:Haft.OrE Base.ShI:Haft.OrT Base.ShS:Haft.OrT
summary(atw15)   #0.3349, 0.001877, 3.745

atw16<-lm(Weight~Should.Sh*Should.Or)  #2Should.ShS Should.OrH Should.ShI:Should.OrH Should.ShS:Should.OrT
summary(atw16)  #0.4078, 7.389e-07, 3.422   多！

atw17<-lm(Weight~Should.Sh*Haft.Sh)   ## bad! 0.1232
summary(atw17)

atw18<-lm(Weight~Should.Sh*Haft.Or)   #. Should.ShI Should.ShI:Haft.OrT
summary(atw18)  #0.3899, 0.0001587, 3.587

atw19<-lm(Weight~Should.Or*Haft.Sh)    #3Should.OrT  2Should.OrH 1Should.OrX .Haft.ShR
summary(atw19)  #0.2007, 0.04945, 4.026

atw20<-lm(Weight~Should.Or*Haft.Or)  #good good
summary(atw20)  #0.483, 3.793e-07, 3.28

atw21<-lm(Weight~Haft.Sh*Haft.Or)  #not that good
summary(atw21)

#############################################################################
atw4<-lm(Weight~Name*Should.Or)   ##highest!
summary(atw4)  #R-squared:  0.6088, p-value: 2.835e-11, se: 2.853

atw20<-lm(Weight~Should.Or*Haft.Or)  #good good
summary(atw20)  #R-squared: 0.483, p-value: 3.793e-07, se: 3.28

atw1<-lm(Weight~Name*Blade.Sh) #3NamePedernales 2NameTravis 
summary(atw1)  #R-squared:  0.4823, p-value: 9.983e-07, error: 3.304

atw3<-lm(Weight~Name*Should.Sh)  #3Wells 1Pedernales Travis
summary(atw3)    #R-squared:  0.4801, p-value: 1.145e-06, error: 3.311

atw2 <- lm(Weight~Name*Base.Sh)  #3NamePedernales  1Travis .Wells
summary(atw2)  #R-squared:  0.466, p-value: 2.676e-06, error: 3.355

atw6<-lm(Weight~Name*Haft.Or)   #1Pedernales .Wells
summary(atw6)   #R-squared:  0.4542, p-value: 5.341e-06, error: 3.393

atw5<-lm(Weight~Name*Haft.Sh)    #3Pedernales 2Travis Wells 
summary(atw5)   #R-squared:  0.4468   p-value: 8.136e-06  error: 3.415

NameTravis <- as.double(Name == 'Travis')   
Should.OrH <- as.double(Should.Or == 'H')
Should.OrT <- as.double(Should.Or == 'T')
d1 <- lm(Weight~Length+Width+Should.OrH*NameTravis+Should.OrT*NameTravis)
summary(d1)  ## 0.8577, 1.657

Haft.OrE <- as.double(Haft.Or == 'E')
Should.OrH <- as.double(Should.Or=='H')
Should.OrT <- as.double(Should.Or=='T')
d2 <- lm(Weight~Length+Width+Should.OrH*NameTravis+Should.OrT*NameTravis
         +Should.OrT*Haft.OrE+Should.OrH*Haft.OrE)
summary(d2)  #0.8604, 1.672

NameEnsor <- as.double(Name =='Ensor')
NameTravis <- as.double(Name == 'Travis')  
Should.OrT <- as.double(Should.Or == 'T')
d3 <- lm(Weight~Length+Width+Should.OrH*NameTravis+Should.OrT*NameTravis
         +Should.OrT*Haft.OrE+Should.OrH*Haft.OrE
         +Name.E*Should.OrT+Name.T*Should.OrT)   #0.8632
summary(d3)

Should.OrX <- as.double(Should.Or=='X')
NameWells <- as.double(Name == 'Wells')
d4 <- lm(Weight~Length+Width+Should.OrH*NameTravis+Should.OrT*NameTravis
         +Should.OrT*Haft.OrE+Should.OrH*Haft.OrE
         +NameEnsor*Should.OrT+NameTravis*Should.OrT
         +NameWells*Should.OrT+NameWells*Should.OrX)   #0.8634
summary(d4)
#############################################################################
d_final <- lm(Weight~Length+Width+Should.OrH*NameTravis+Should.OrT*NameTravis
              +Should.OrT*Haft.OrE+Should.OrH*Haft.OrE
              +NameEnsor*Should.OrT+NameTravis*Should.OrT
              +NameWells*Should.OrT+NameWells*Should.OrX)
summary(d_final)  #0.8634, 1.709

plot(d_final$fitted.values,d_final$residuals)
names(d_final)
res <- d_final$residuals
fit <- d_final$fitted.values
cbind(Weight, res, fit)
plot(fit, res)

par(mfrow=c(2,2))
plot(d_final)

d_final$coefficients

yhat <- d_final$coefficients[1]+d_final$coefficients[2]*70+d_final$coefficients[3]*60
yhat  # 26.264 

CI <- yhat+c(-1,1)*1.709*qt(0.975,df=74)
CI  #22.85874 29.66925

newdata<-data.frame(Length=70,Width=60,NameTravis=1,Should.OrH=0,Should.OrT=0,Haft.OrE=0,NameEnsor=0,NameWells=0,Should.OrX=0)
predict(d_final,newdata,df=74,interval = "confidence",level=0.95)





