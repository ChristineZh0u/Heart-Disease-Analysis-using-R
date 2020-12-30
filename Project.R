install.packages("ggcorrplot")
library(ggcorrplot)


###import data set
data <- read.csv(file = 'C:\\Users\\xinyi\\Dropbox\\School\\College 2\\STAT 355\\Project\\Heart.csv')
data
age = ï..age #fix import error
attach(data)

###overall information
dim(data)
summary(data)

###single variable info
#histogram & Q-Q ploy of variables
hist(age)
boxplot(age)
hist(sex,breaks =2)
hist(trestbps)
boxplot(trestbps)
hist(chol)
boxplot(chol)
hist(thalach)
boxplot(thalach)
hist(oldpeak)
boxplot(oldpeak)
hist(target, breaks=2)

###correlation
pairs(~age+trestbps+chol+thalach+oldpeak,data=data, main = "Corrolations of Pairs")
pairs(~age+trestbps+chol+thalach+oldpeak,data=data, main = "Corrolations of Pairs", col = ifelse(target < 1,'red','green'))
pairs(~age+trestbps+chol+thalach+oldpeak+sex,data=data, main = "Corrolations of Pairs", col = ifelse(target < 1,'red','green'))
#correlation matrix
corr <- round(cor(data), 1)
ggcorrplot(corr, lab = TRUE)

###Question 1
hd = thalach[which(target==0)] #select max heart rate of patients who have heart disease
nhd = thalach[which(target==1)] #select max heart rate of patients without heart disease
t.test(hd, nhd, alternative='two.sided')
t.test(hd, nhd, alternative='less')
wilcox.test(hd, nhd,  alternative='less')

###Question 2
boxplot(age~cp)
anova = aov(age~cp)
summary(anova)
qqnorm(anova$residuals) #check assumptions of ANOVA


###Question 3
mod1=lm(target~cp+thalach+exang+oldpeak+ca)
mod2=lm(target~cp*thalach*exang*oldpeak*ca)
anova(mod1, mod2) #compare with/without interaction
summary(mod2)
step(mod2)


###Question 4
m1=glm(target~thalach, family=binomial(link=logit))
plot(target~thalach)
allthalach = seq(min(thalach),max(thalach), len=1000)
phats = predict(m1, data.frame(thalach=allthalach), type='res')
lines(phats ~ allthalach, col=2, lwd=2)
exp(m1$coef[2])
#separating male and female
female = data[data$sex == 0,]  #select female patients
male = data[sex == 1,] #select male patients
#male
attach(male)
m1=glm(target~thalach, family=binomial(link=logit))
plot(target~thalach, main='male')
allthalach = seq(min(thalach),max(thalach), len=1000)
phats = predict(m1, data.frame(thalach=allthalach), type='res')
lines(phats ~ allthalach, col=2, lwd=2)
exp(m1$coef[2])
#female
attach(female)
m1=glm(target~thalach, family=binomial(link=logit))
plot(target~thalach, main='female')
allthalach = seq(min(thalach),max(thalach), len=1000)
phats = predict(m1, data.frame(thalach=allthalach), type='res')
lines(phats ~ allthalach, col=2, lwd=2)
exp(m1$coef[2])
