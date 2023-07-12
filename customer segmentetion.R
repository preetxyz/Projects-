customer_data=read.csv("C:\\Users\\Preety\\Downloads\\_customers.csv")
View(customer_data)
str(customer_data)
names(customer_data)
head(customer_data)
tail(customer_data)
summary(customer_data$Age)
sd(customer_data$Age)
summary(customer_data$Annual.Income)
sd(customer_data$Annual.Income)
summary(customer_data$Spending.Score)
sd(customer_data$Spending.Score)
#customer gender visualization 
#1
a=table(customer_data$Gender)
print(a)
barplot(a,main="Gender Visualisation",
        xlab="Gender",ylab = "Count",
        col=rainbow(2),
        legend.text = c(round(100*a/sum(a)))) 
#2
pie_per=round(100*a/sum(a))
lb=paste(c("female","male")," ",pie_per,"%",sep="")
library(plotrix)
pie3D(a,labels= lb,main="Gender Visualisation")
legend("topleft",c("female","male"), cex=0.8,fill=rainbow(2),bg="pink")
#analysis of the annual income of the customers
#1.
summary(customer_data$Annual.Income)
h=table(customer_data$Annual.Income)
hist(customer_data$Annual.Income,
     main="Histrogram for Annual Income",
    xlab="annual income class",
    ylab="frequency",
    col="brown",
    border="pink",
    labels=TRUE) 
#analyzing spending score of the customers
summary(customer_data$Spending.Score)
#1.
boxplot(customer_data$Spending.Score,
        horizontal=TRUE,
        main="Boxplot for analysis of spending score",
        col="pink",
        ylab="frequency",
        xlab="spending score")
#2.
hist(customer_data$Spending.Score,
     main="histogram for spending score",
     xlab="spending score",
     ylab="frequency",
     labels=TRUE,
     col="blue",
     border="pink")

#comparison of age and income of customers
age=customer_data$Age     
income=customer_data$Annual.Income

library(ggplot2)
ggplot(customer_data,aes(x=age,y=income))+
  ggtitle("Comparison between Age and Annual Income")+
    geom_tile(color="pink",fill="blue")
#or 

ggplot(customer_data,aes(x=age,y=income))+
  ggtitle("Comparison between Age and Annual Income")+
  geom_text(label=income,color="blue")


#comparison  between age and spending score 
age=customer_data$Age
spending_score=customer_data$Spending.Score

ggplot(customer_data,aes(x=age,y=spending_score))+
  ggtitle("Comparison between Age and Spending Score")+
  geom_tile(color="orange",fill="pink")
#or
ggplot(customer_data,aes(x=age,y=spending_score))+
  ggtitle("Comparison between Age and Spending Score")+
  geom_text(label=spending_score,color="orange")



#comparison between Income and spending score
income=customer_data$Annual.Income
spending_score=customer_data$Spending.Score

ggplot(customer_data,aes(x=income,y=spending_score))+
  ggtitle("Comparison between Income and Spending Score")+
  geom_tile(color="green",fill="pink")
#or
ggplot(customer_data,aes(x=income,y=spending_score),xlim=c(20,200),ylim=c(15,100))+
  ggtitle("Comparison between Income and Spending Score")+
  geom_text(label=income,color="green")

#comparison between gender and spending score 
gender=customer_data$Gender
spending_score=customer_data$Spending.Score

ggplot(customer_data,aes(x=gender,y=spending_score))+
  ggtitle("Comparison between Gender and Spending Score")+
  geom_tile(color="blue",fill="white")
#or
ggplot(customer_data,aes(x=gender,y=spending_score))+
  ggtitle("Comparison between Gender and Spending Score")+
  geom_bin2d(color="white")

#comparison of income,age and spending score
scatterplot=c(customer_data$Annual.Income,customer_data$Age,customer_data$Spending.Score)
plot(customer_data$Spending.Score,type="l",main="comparison beteen income, age, spending score"
     ,col="green",ylab="spending score")
lines(customer_data$Age,type="l",col="red")
lines(customer_data$Annual.Income,type="l",col="blue")

