#Statistical Inference
# Lab 4
# Abdul Rahman Vakili
# 810196646

1. 
library(boot)
set.seed(1)
dat<-data.frame(airquality)
dat[is.na(dat)]<- 0
bot_medi<- function(dat,resam_vect){median(dat[resam_vect])}
medi_result<- boot(dat$Solar.R,bot_medi,R=1000)

2. 
ci<- boot.ci(medi_result,0.9, type="perc")
ci


3. 
inter<- median(medi_result$t) + c(-1,1)*qnorm(1-(1-0.90)/2)*sterr
inter

4. 
hist(medi_result$t, col="green", main="Percentile_median of CI")
abline(v=quantile(medi_result$t, c(0.05,0.95)),col="brown")
hist(medi_result$t, col="green", main="SError of CI")
abline(v=quantile(inter, c(0.05, 0.95), col= "blue"))

5. 
source("inference.R")
inference(dat$Solar.R,type="ci", method="simulation", conflevel = 0.90, est = "median", boot_method = "perc", seed =1, nsim = 1000)
inference(dat$Solar.R,type="ci", method="simulation", conflevel = 0.90, est = "median", boot_method = "se", seed =1, nsim = 1000)

6. 
library(COUNT)
theship$Risk <- ifelse(theship$incidents > 10, c("High"), c("Low"))

7. 
inference(high_rsk$Risk,type="ht", method="simulation", success = "High", conflevel = 0.95, est = "proportion", alternative = "greater", null= 0.5)

8. 
#In document

9. 
ggplot(Galton, aes(x=father, y = height))+ geom_point(color="red") + ylab("Height of Child")+ xlab("Height of father")+ geom_smooth(data= Galton, method = "lm", se = FALSE, col= "green")

10. 
# In document
11. 
mymodel<- lm(height~father, data=Galton)
mymodel<- lm(height~father, data=Galton)

12. 
#In document

13. 
Nmodel<- data.frame(father=c(70,75,80))
predict(mymodel, newdata = Nmodel, interval= "predict")

14. 
mymodel2 <- lm(height~father+mother, data = Galton)
summary(mymodel2)

15. 
hist(mymodel2$residuals, col= "green", main = "Modle2 Histogram")


16. 
qqnorm(mymodel2$residuals)
qqline(mymodel2$residuals, col = "green")

17. 
plot(mymodel2$residuals~Gal$father, main = "Residula Plot")
abline(0,0)
plot(mymodel2$residuals~Galton$mother, main = "Residula Plot")
abline(0,0)
plot(mymodel2$residuals, main = "Residula Plot")
abline(0,0)

18. 
plot(mymodel2$residuals~mymodel2$fitted.values, main = "fitted plot")
abline(0,0)

19. 
mymodel3<- lm(height~father+mother+sex, data =Galton)
summary(mymodel3)

20. 
# In document

21. 
mymodel_4th<-lm(height~father+ mother+sex+nkids+family, data=Galton)
step(mymodel_4th, direction= "backward", test= "F")

22. 
tt <-data.frame(titanic)
mymodel_glm<- glm(survived~class+sex+age,data =tt, family = binomial)
summary(mymodel_glm)

23. 
predict(mymodel_glm, newdata = mydat, type = "response", interval = "predict")

24. 
pr<- data.frame()
for (a in levels(tt$age)){
 for (b in levels(tt$sex)){
  for (c in levels(tt$class)){
       if(nrow(subset(tt, class == c))> 0){
       datn<- with(tt, data.frame(sex = b, calss = c, age = a))
       thep<- data.frame(predict(mymodel_glm, newData = datn, type = "response"), a, b, c)
       pr<- rbind(pr, thep)
      }
    }
  }
}
names(pr) <-c("probability", "age", "sex", "class")
pr[with(pr, order(probability)),]

25.
 logpr<- function(logi){
   theodds<- exp(logi)
   theprob<- theodds / (1 + theodds)
   return(theprob)
 }
 logpr(predict(mymodel3, interval = "predict"))

26. 
# In document
