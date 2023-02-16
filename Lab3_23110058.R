setwd("C:/Users/Hannan/Downloads")
LifeExpectancy<-read.csv("Life Expectancy.csv")
#Question 1
par(mar=c(5,4,2,1))
par(mfrow=c(1,2))
with(subset(LifeExpectancy,Sex=="Male" & Race=="All Races"),plot(Year,Average.Life.Expectancy,main="Male Life Expectancy",pch=17,ylab="Life Expectancy",xlab="Year",col="red",ylim=c(30,80)))
with(subset(LifeExpectancy,Sex=="Female" & Race=="All Races"),plot(Year,Average.Life.Expectancy,main="Female Life Expectancy",pch=17,ylab="Life Expectancy",xlab="Year",col="blue",ylim=c(30,80)))

#Question 2
with(subset(LifeExpectancy,Sex=="Male" & Race=="All Races"),plot(Year,Average.Life.Expectancy,main="Male Life Expectancy",pch=17,ylab="Life Expectancy",xlab="Year",col="red",ylim=c(30,80)))
abline(v=c(1914,1918,1939,1945),lty="dotted")
with(subset(LifeExpectancy,Sex=="Female" & Race=="All Races"),plot(Year,Average.Life.Expectancy,main="Female Life Expectancy",pch=17,ylab="Life Expectancy",xlab="Year",col="blue",ylim=c(30,80)))
abline(v=c(1914,1918,1939,1945),lty="dotted")

#Question 3
par(mfcol=c(2,1))
par(oma=c(0,0,2,0))
par(mar=c(5,5,2,1))
hist(subset(LifeExpectancy,Sex=="Female" & Race == "Black")$Average.Life.Expectancy,xlab="Life Expectancy",main= "Life Expectancy in Black Females")
hist(subset(LifeExpectancy,Sex=="Female" & Race == "White")$Average.Life.Expectancy,xlab="Life Expectancy",main="Life expectancy in White Females")
mtext(("Life Expectancy across Races in Females"),outer=TRUE,font=2)

#Question 4
install.packages("gridExtra")
library("gridExtra")
library("lattice")

plot1<-xyplot(Average.Life.Expectancy~Year | Race,data=subset(LifeExpectancy,Sex=="Female"),ylab="Life Expectancy",xlab="Year",as.table=T,layout=c(3,1),main="Female Life Expectancy Across all Races",col="black",pch=3,cex=0.35)
plot2<-xyplot(Average.Life.Expectancy~Year | Race,data=subset(LifeExpectancy,Sex=="Male"),ylab="Life Expectancy",xlab="Year",as.table=T,layout=c(3,1),main="Male Life Expectancy Across all Races",col="black",pch=3,cex=0.35)

grid.arrange(plot1,plot2)

#Question 5
library(ggplot2)
ggplot(subset(LifeExpectancy,Sex!="Both Sexes"),aes(x=Sex,y=Average.Life.Expectancy))+
  geom_boxplot()+
  labs(y="Average Life Expectancy",x="Sex",title="Average Life Expectancy by Gender")+
  theme_bw()

#Question 6
library(ggplot2)
ggplot(subset(LifeExpectancy,Sex!="Both Sexes" & Race!="All Races"),aes(x=Average.Life.Expectancy))+
  facet_wrap(Sex~Race)+
  geom_density(alpha=0.7,color="black",fill="light blue")+
  labs(y="density",x="Life Expectancy",title="Life Expectancy by Race and Sex")

  

