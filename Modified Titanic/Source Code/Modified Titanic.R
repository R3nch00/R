data=read.csv("C:/Users/O M A R/Downloads/Document/Data Science/PomPom/TITANIC MOD.csv")
print(data)

str(data)

names(data)

head(data)

summary(data$age)

sapply(data,function(x) which(is.na(x)))

data$age[is.na(data$age)]<-mean(data$age,na.rm= TRUE)
print(data)

data1<-data
for(i in 1:ncol(data)){
  data1[,i][is.na(data1[ ,i])]<-mean(data1[ ,i],na.rm= TRUE)
}
data1

boxplot(data$age,col="maroon",main="Boxplot for descriptive analysis of Age")
boxplot(data$gender,col="blue",main="Boxplot for descriptive analysis of gender")
boxplot(data$sibsp,col="olivedrab1",main="Boxplot for descriptive analysis of sibsp")
boxplot(data$parch,col="red4",main="Boxplot for descriptive analysis of parch")
boxplot(data$fare,col="green",main="Boxplot for descriptive analysis of fare")
boxplot(data$survived,col="skyblue",main="Boxplot for descriptive analysis of survived")

hist(data$fare,
           col="palevioletred1",
           main="Histogram for FARE",
           xlab="fare",
           ylab="frequency",
           labels=TRUE)


a=table(data$who)
barplot(a,main="Using BarPlot to display Man,Woman,Child",
        ylab="Count",
        xlab="who",
        col=rainbow(3),
        legend=rownames(a))

sd(data$age,na.rm=FALSE)


a=table(data$class)
lbs=paste(c("First","Second","Third")," ",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,main="Pie Chart Depicting Ratio of Class")


install.packages("zoo")
library("zoo")

data2<-na.aggregate(data)
data2

data2$age=as.numeric(format(round(data2$age,0)))
data2$fare=as.numeric(format(round(data2$fare,0)))
print(data2)
data2$gender=as.numeric(format(round(data2$gender,0)))
print(data2)


