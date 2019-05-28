# TASK 1
View(mtcars)
#1
#p(Z>2.64)
pnorm(2.4,lower.tail = F)
pnorm(1.39,lower.tail =F)
#2TRUE
-qnorm(.99)
rownames(UCBAdmissions)
View(UCBAdmissions)
xtabs(Freq ~ Admit, data = UCBAdmissions)
ph<-1755/(1755+2771)
ph
nn<-(ph-.4)/sqrt((.4*(1-.4))/(1755+2771))
nn
#as test statistic value is higher than the z score we cant 
#reject the null hypothesis
#3)
n_of_sam<-sum(table(rownames(mtcars)))
qt(.95,df=(n_of_sam -1))
table(mtcars$am)
ph1<-13/(13+19)
ph1
nn1<-(ph1-.4)/sqrt((.4*(1-.4))/(13+19))
nn1
#as test statistic value is lower than the z score we can 
#reject the null hypothesis

#task 2
library(xlsx)
setwd("C:/Users/Owner/Documents/newwd")
dataa<-read.xlsx(unzip("AirQualityUCI.zip","AirQualityUCI.xlsx"),1)
View(dataa)
table(is.na(dataa))
library(DMwR)
manyNAs(dataa,0.20)
dataa1<-dataa[,c(-16,-17)]#it has all omitted last coloumns which are irrelevant
dataa2<-na.omit(dataa1) #it has omitted nas in all coloumns
table(is.na(dataa2))
#we dont do imputation since the na's are in all rows of last lines
#univariate for all coloumns
lapply(dataa2[,3:15], mean)
lapply(dataa2[,3:15], median)
lapply(dataa2[,3:15], max)
lapply(dataa2[,3:15], min)
lapply(dataa2[,3:15], sd)
lapply(dataa2[,3:15], range)
View(BostonHousing)
library(corrplot)
Date_numeric<-as.numeric(dataa2$Date)
Time_numeric<-as.numeric(dataa2$Time)
new_data_datetimenumeric<-data.frame(Date_numeric,Time_numeric,dataa2[,3:15])
head(new_data_datetimenumeric)
corrplot(cor(new_data_datetimenumeric),type = "lower","number")
corll<-cor(new_data_datetimenumeric)
write.csv(corll,"correlationnn.csv")

for(i in 3:15)
{  
fit<-lm(new_data_datetimenumeric[,i]~dataa2$Time)
print(summary(fit))
}
for(i in 3:15)
{  
  fit<-lm(new_data_datetimenumeric[,i]~dataa2$Date)
  print(summary(fit))
}
#to findout most polluted time of the day and most polluted component
#second way after adding
date1<-new_polluted$dataa2.Date
num_time<-as.numeric(new_polluted$dataa2.Time)
new_polluted1<-data.frame(date1,num_time,new_polluted$pollution_content)
View(new_polluted1)
jj=-2209161600 #first hour of the day
time_24hours<-data.frame()
for(i in 1:24)
{
  time_24hours[i,1]<-sum(new_polluted1[which(new_polluted1$num_time==jj),3])
  jj=jj+3600
}
time_24hours
time_24hours$id<-1:24
cpp<-max(time_24hours)
time_24hours[which(time_24hours$V1==cpp),]
#so on average 1899-12-30 20:00:00 of any day has high pollution
date2<-new_polluted$dataa2.Date
num_time1<-as.numeric(new_polluted$dataa2.Time)
no_neg<-data.frame(date2,num_time1,new_data_datetimenumeric[,3:15])
View(no_neg)
for(l in 3:15)
{
  for(j in 1:nrow(no_neg))
  {
    if(no_neg[,l][j]<0)
    {
      no_neg[,l][j]=0
    }
  }
}
time_24hours1<-data.frame()
jj=-2209161600
for(j in 1:13)
{
  jj=-2209161600
  for(i in 1:24)
  {
    time_24hours1[i,j]<-
      sum(no_neg[which(no_neg$num_time1==jj),j+2])
    jj=jj+3600
  }
}
time_24hours2<-data.frame()
for(i in 1:13)
{
  time_24hours2[i,1]<-max(time_24hours1[,i])
}
time_24hours2$id<-1:13
max(time_24hours2)
time_24hours2[which(time_24hours2$V1==max(time_24hours2)),]
# so from above analysis i conclude that PT08.S4.NO2 has high content of pollution






