


#################################################
#########SECTION 1: Exploratory Analysis#########
#################################################
rm(list=ls())
FYGPA=read.csv(file.choose(), header =T)
summary(FYGPA)
head(FYGPA)
str(FYGPA)
attach(FYGPA)
boxplot(GPA,HSGPA)
pairs(FYGPA[2:3])
qqnorm(GPA)
qqnorm(HSGPA)
t.test(FYGPA[,2],FYGPA[,3],paired=TRUE)

boxplot(GPA~White,data=FYGPA)
t.test(GPA~White,data=FYGPA)

boxplot(GPA~Male,data=FYGPA)
t.test(GPA~Male,data=FYGPA,var.equal=TRUE)

boxplot(GPA~FirstGen,data=FYGPA)
t.test(GPA~FirstGen,data=FYGPA,var.equal=TRUE)

boxplot(GPA~CollegeBound,data=FYGPA)
t.test(GPA~CollegeBound,data=FYGPA)

myAnova<- aov(GPA~White+Male+FirstGen+CollegeBound, data = FYGPA)
summary(myAnova)
#################################################
###SECTION 2: Bootstrapping###
#################################################
# The seed is set to 1
set.seed(1)
n=219 # No.of students
nboot=150
student_gpa=matrix(NA,nrow=nboot, ncol= n)

for (i in 1:nboot){
  GPAsample = sample(1:n, n, replace = T)
  student_gpa[i,] = GPA[GPAsample]
  
}
gpaAvg=apply(student_gpa, 1, mean)
hist(gpaAvg, xlab="Bootstrap mean", main="Histogram of bootstrap means\nFirstYearGPA")
abline(v=mean(gpaAvg), col="red",lwd=2)

mean(GPA)
SE=sd(gpaAvg)      #Predicted population standard error
sd(GPA)/sqrt(219) # Sample Standard Error
alpha = 0.05
tMult<-qt(1-alpha/2,df=218)
# 95% confidence interval
mean(GPA)-tMult*SE
mean(GPA)+tMult*SE

#################################################
#########SECTION 3: Regression Analysis#########
#################################################

qqnorm(GPA)

# Linear Regression
dataset <- FYGPA[,c(-1,-2)]
modelAll <- lm(GPA ~ ., data=dataset)  #Model containing all variables
summary(modelAll)
par(mfrow=c(2,2))
plot(modelAll)
par(mfrow=c(1,1))
modelBasic <- lm(GPA ~ 1) #Model containing only intercept
summary(modelBasic)


step(modelAll,scope=list(lower=modelBasic,upper=modelAll),direction = "backward") #To step backward
step(modelBasic,direction="forward",scope=formula(modelAll)) #To step forward

modelFit<-lm(formula = GPA ~ HSGPA + SATV + HU + SS + White, data = dataset)
summary(modelFit)
par(mfrow=c(2,2))
plot(modelFit)
par(mfrow=c(1,1))

# Non-parametric regression
plot(HSGPA,GPA)
modelBadFit<-lm(formula = GPA ~ HSGPA, data = FYGPA)
summary(modelBadFit)
par(mfrow=c(2,2))
plot(modelBadFit)
par(mfrow=c(1,1))

# K-neighbours

#install.packages("FNN") // Uncomment the line to install the package
library(FNN) # need this library for kNN
plot(HSGPA, GPA,xlab="High-school GPA", ylab="College GPA", main="k nearest neighbor regression")
m=knn.reg(HSGPA,y=GPA,k=10) #k=10
x=HSGPA
fit=m$pred
lines(x[order(x)], fit[order(x)], col="blue",lwd=2)
m=knn.reg(HSGPA,y=GPA,k=50) #k=50
x=HSGPA
fit=m$pred
lines(x[order(x)], fit[order(x)], col="red",lwd=2)

#Kernel Regression
plot(SATV,GPA)
modelBadFit<-lm(formula = GPA ~ SATV, data = FYGPA)
summary(modelBadFit)
lines(ksmooth(x=SATV, y=GPA,kernel="normal", bandwidth=25), lwd=2, col="red")


