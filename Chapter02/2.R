data()
str(airquality)
head(airquality)
mydata <- airquality
mydata$Month = factor(mydat$Month)
str(mydata)
is.na(mydata$Ozone)
sum(is.na(mydata$Ozone)==TRUE) 
sum(is.na(mydata$Ozone)==TRUE) / length(mydata$Ozone)* 100
sapply(mydata, function(df){
  +		sum(is.na(df)==TRUE)/length(df) *100
   })
install.packages("Amelia")
require(Amelia)
missmap(mydata, main="Missing Map")
AmeliaView()
mydata$Ozone
max(table((factor(mydata$Ozone)),useNA="always"))
max(table((factor(mydata$Ozone))))
mydata$Ozone[which(is.na(mydata$Ozone))]=23
table((factor(mydata$Ozone)),useNA="always")
table((factor(mydata$Solar.R)),useNA="always")
max(table((factor(mydata$Solar.R))))
mydata$Solar.R[which(is.na(mydata$Solar.R))]=259
table((factor(mydata$Solar.R)),useNA="always")
barplot(table(mydata$Ozone), main="Ozone Observations", xlab="O bservations", ylab="Frequency")
barplot(table(mydata$Temp), main="Temperature Observations", xlab="Temprature", ylab="Frequency")
hist(mydata$Temp,  main="Temperature", xlab = " Temperature ")
hist(mydata$Temp,  main="Temperature", xlab = " Temperature ", breaks= 5)
hist(mydata$Temp,  main="Temperature", xlab = " Temperature ", prob=TRUE)
summary(mydata)
boxplot(mydata)
summary(mydata$Temp)
boxplot(mydata$Temp)
boxplot(mydata$Temp ~ mydata$Month, main="Month Wise Temperature", xlab="Month", ylab="Temperature")
plot(mydata$Temp ~ mydata$Day + mydata$Solar.R + mydata$Wind + my-data$Ozone, col="blue")
install.packages("corrplot")
require(corrplot)
mydata$Month = airquality$Month # Removing factors, using origi-nal data
corrplot(cor(mydata),method="number")
attach(mydata)
reg <- lm(Temp~Ozone)
summary(reg)
predict(reg, data.frame(Ozone=c(80)))
Solar.R = 190
Wind = 10
Ozone = 45
Month = 9 
new_data = data.frame(Solar.R,Wind,Ozone,Month)
new_data
reg<-lm(Temp~Ozone+Solar.R+Month,mydata)
predict(reg,newdata = new_data)
corrplot(cor(mydata),method="color")
corrplot(cor(mydata),method="circle")
corrplot(cor(mydata),method="pie")

