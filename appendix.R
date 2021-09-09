data <- read_csv("C:/Users/gonul/Desktop/veri364 - Çalışma Sayfası1.csv")
View(data)
data1=round(data[,3:20],digits = 4)
data=cbind(data[,1:2],data1)

#Analysis of Human Scores by Regions Codes
ggplot(data,aes(region,Human_Freedom_score))+geom_boxplot()+labs(title ="Human Freedom Scores by Region",y="Human Freedom Score")+scale_fill_gradient2()+ theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Analysis of Highest and Lowest Human Freedom Scores of Countries Codes
newdata <- data[order(Human_Freedom_score,countries),]
newdata=newdata[,c(1,19)]
newdata[,2]=round(newdata[,2],digits = 1)
newdata=newdata[1:10,]
Analysis of Legal System and Property Right Scores by Regions Codes
keeps <- c("Legal_system_and_property_rights","region") #independence
master = master_all[keeps]
str(master)
summary(master)
mytable <- table(master)
chi2 <- chisq.test(mytable, correct=TRUE)
shapiro.test(Legal_system_and_property_rights) # normality check
bartlett.test(Legal_system_and_property_rights ~ region, data=data) #variance check
tukey.test=TukeyHSD(aov.out,conf.level = 0.99)
aov.out = aov(Legal_system_and_property_rights ~ region, data=data)

#Analysis of Correlation Matrix Codes
res2 <- rcorr(as.matrix(data))
corrplot(res2$r, type="upper", order="hclust",p.mat = res2$P, sig.level = 0.01, insig =
           "blank", tl.cex = 0.55/par("cex"),cl.cex = 1/par("cex"))

#Multiple Linear Regression Codes
T_tuk = transformTukey(Human_Freedom_score,plotit=FALSE) #transformation
shapiro.test(Human_Freedom_score^(1.5)) #normality check
fit=lm((Human_Freedom_score^1.5)~Rule_of_Law+Security_and_Safety+Free-
         27
       dom_of_movement+Religious_freedom+Association+Freedom_of_expression+Identity_and_relationships+Size_of_government+Legal_system_and_property_rights+Sound_money+Freedom_to_trade_internationally+Regulation,data=data)
summary(fit)
mse=(test.set$Human_Freedom_score-pred)^2
pred=predict(model,test.set)
summary(model)
model=lm(Human_Freedom_score~Rule_of_Law+Security_and_Safety+Freedom_of_movement+
           Religious_freedom+Association+
           Freedom_of_expression+Size_of_government+Legal_system_and_property_rights+Sound_money+Fr
         eedom_to_trade_internationally+Regulation,data=train.set)
fit=lm(Human_Freedom_score^(1.5)~Rule_of_Law+Security_and_Safety+Freedom_of_movement+
         Religious_freedom+Association+
         Freedom_of_expression+Identity_and_relationships+Personal_Freedom_score+Size_of_government
       +Legal_system_and_property_rights+Sound_money+Freedom_to_trade_internationally+Regulation+
         Economic_Freedom_score,data=data)
summary(fit)
fit2=lm(Human_Freedom_score^(1.5)~Rule_of_Law+Security_and_Safety+Freedom_of_movement+
          Religious_freedom+Association+
          Freedom_of_expression+Identity_and_relationships+Size_of_government+Legal_system_and_proper
        ty_rights+Sound_money+Freedom_to_trade_internationally+Regulation,data=data)
summary(fit2) after transformation
plot(fit2)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
x=residuals(fit2)
lines(xfit, yfit, col="red", lwd=2)
yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
xfit<-seq(min(x),max(x),length=100)
main="Histogram of residuals")
h<-hist(x, breaks=9, col="brown",)
ols_step_best_subset(fit2)
ols_step_both_p(fit2)
ols_step_forward_p(fit2)
fit3=lm(Human_Freedom_score^(1.5)~Rule_of_Law+Security_and_Safety+Freedom_of_movement+
          Religious_freedom+Association+
          Freedom_of_expression+Size_of_government+Legal_system_and_property_rights+Sound_money+Fr
        eedom_to_trade_internationally+Regulation,data=data) #final model after selection
summary(fit3)

#cross validation
test.set=data[trainingRowindex,]
train.set=data[trainingRowindex,]
trainingRow=sample(1:nrow(data),0.7*nrow(data))
data$Human_Freedom_score=data$Human_Freedom_score^(1.5)
createDataPartition(p = 0.8, list = FALSE)
training.samples <- data$Human_Freedom_score^(1.5)
training.sample=data$Human_Freedom_score^(1.5)
MSE=mean(mse, na.rm = T)