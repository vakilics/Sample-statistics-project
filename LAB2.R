###Question 1####
x <- seq(-3, 3 , length=100);
y<-dnorm(x);
plot(x,y,xlab = "Value of X" ,ylab = "Value of Y" ,type="l");
#============================================================

###Question 2####
x <- rnorm(5);
y<-dnorm(x);
plot(x,y,xlab = "X" ,ylab = "Y");

x <- rnorm(10);
y<-dnorm(x);
plot(x,y,xlab = "X" ,ylab = "Y");

x <- rnorm(100);
y<-dnorm(x);
plot(x,y,xlab = "X" ,ylab = "Y");

x <- rnorm(1000);
y<-dnorm(x);
plot(x,y,xlab = "X" ,ylab = "Y");

###Question 3####
x <- seq(-3, 3, length=100)
Y<-dnorm(x)
dfree <- c(1, 2, 5, 10)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=2", "df=5", "df=10", "normal")
plot(x, y, type="l",xlab="x value",ylab="Density")
for (i in 1:4){
if(i==1){
lines(x, dt(x,dfree[1]), lwd=4, col=colors[1])
}
else{
lines(x, dt(x,dfree[i]),lwd=2 , col=colors[i])
}
}

legend("topleft", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

###Question 4####

qqnorm(precip)
qqline(precip)

###Question 5####

par(mfrow=c(2, 2))
hist(x, breaks=7, col="green", border="white")
hist(x, breaks=13, col="green", border="white")
hist(x, breaks=30, col="green", border="white")
hist(x, breaks=13 ,prob=TRUE, col="gray", border="white")

###Question 6####
hist(x ,breaks = 7 ,prob=TRUE ,col = "brown" ,border = "yellow",main = "A Sample Histogram");
lines(density(x,bw=5),col="green")
labels <- c("simple curve", "density histogram");
colors <- c("yellow", "brown")
legend("topright", inset=.05,labels, lwd=2, lty=c(1, 1), col=colors)

###Question 7####

boxplot(rivers, main="Rivers Data")

###Question 8####
med<-median(rivers);
x<-sort(rivers)
x
#Finding Q1
Q1<-median(x[1 : match(med , x)])

#Finding Q2
Q2<-median(rivers)

Q1#Finding Q3
Q3<-median(x[match(med , x) :length(rivers)])

#Finding IQR
IQR = Q3-Q1

upperWhisker = min(max(rivers), (Q3 + (1.5 * IQR) ));
lowerWhisker = max(min(rivers), (Q1-(1.5 * IQR) ));

#Finding Outlier
for (out in rivers) {
if(out>upperWhisker){
print(out)
}
}
###Question 9####
x<-rivers
x[rivers<500] <-"tiny"
tiny<-x[which(x=="tiny")]
rivers<-rivers[which(x!="tiny")]

x<-rivers
x[rivers<1500] <-"short"
short<-x[which(x=="short")]
rivers<-rivers[which(x!="short")]

x<-rivers
x[rivers<3000] <-"medium"
medium<-x[which(x=="medium")]
rivers<-rivers[which(x!="medium")]

x<-rivers
x[rivers>=3000] <-"long"
long<-x[which(x=="long")]
rivers<-rivers[which(x!="long")]

x<-c(length(tiny) , length(short) ,length(medium) , length(long))
pie(x , labels =paste(round((x/sum(x)*100) , 1),"%"),main="Pie Chart" , col = rainbow(length(x)) );
lbls <- c("TINY", "SHORT", "MEDIUM", "LONG");
legend("topright", lbls, cex = 0.8 ,col = colors,   fill = rainbow(length(x)))

###Question 10####
#Some packages are need to insatll first
install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
boxplot(mpg$displ ~ mpg$class)

F <- x ~y

###Question 11####

library(ggplot2)
ggplot(Orange, aes(x=Age, y=Circumference))+geom_point(size=2)
geom_smooth()

###Question 12####

#In the R code below, point shapes, colors are controlled by the levels of the factor variable Tree :
ggplot(Orange, aes(x=age, y=circumference , shape=Tree ,color=Tree))+geom_point(size=2)

###Question 13####
#With method loess
ggplot(Orange, aes(x=age, y=circumference))+geom_point(size=2)+ geom_smooth(method="loess" ,se="TRUE", fullrange=TRUE ,level=0.80)

###Question 14####


x<-Orange
x$age[Orange$age <250]<-"Young"
x$age[Orange$age>250 & 900 > x$age ]<-"Adult"
x$age[ 900<Orange$age ]<-"Old"

p<-ggplot(data=x, aes(x=age, y=circumference,fill=
                        ges)) +
   geom_bar(stat="identity" ,position=position_dodge())+
   coord_flip() +
   scale_fill_brewer(palette="Paired") +
   scale_x_discrete(limits=c("Young", "Adult", "Old"))

###Question 15####

centers_df=data.frame(latitude=state.center$y , longitude=state.center$x)

###Question 16####

install.packages('ggmap')
library(ggmap)
library(tidyverse)

get_map("United States" ,zoom = 6) %>% ggmap() +geom_point(data = centers_df, aes(x = centers_df$longitude,
y = centers_df$latitude), color = 'red', size = 4)