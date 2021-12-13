data <- read.csv(file.choose())

data$age_g <- ifelse(data$EAGE<=35, 0,
                     ifelse(data$EAGE<=40, 1,
                            ifelse(data$EAGE<=45, 2,
                                   ifelse(data$EAGE<=50, 3,
                                          ifelse(data$EAGE<=55, 4,
                                                 ifelse(data$EAGE<=60, 5,
                                                        ifelse(data$EAGE<=65, 6, 7)))))))


data$mutation <- ifelse(data$DTIME<=data$INTERVIEW2 & data$DTIME >= data$INTERVIEW1,paste0(data$MUT1,data$MUT1),
                        paste0(data$MUT1,data$MUT2))
data$mutation <- ifelse(is.na(data$mutation), paste0(data$MUT1,data$MUT2), data$mutation)

# NEW AGE GROUP FOR 4 LEVELS
data$age_g_4 <- ifelse(data$EAGE<=40, 0,
                     ifelse(data$EAGE<=50, 1,
                            ifelse(data$EAGE<=60, 2, 3)))
# new age group for 2 levels
data$age_g_2 <- ifelse(data$EAGE<=50, 0, 1)


# new mut group for 2 levels
data$mut <- ifelse(data$mutation == "01" | data$mutation == "10", 1, 0)


# 把發病在研究開始前的資料刪掉
data <- data[-which(data$DTIME<data$INTERVIEW1),]

# calculate futime
data$dtimeyyyymmdd <- paste0(data$DYYYY, "-", data$DMM, "-", data$DDD)
data$followyyyymmdd <- paste0(data$YYYY1, "-", data$MM1, "-", data$DD1)
data$futime[which(data$dtimeyyyymmdd != "NA-NA-NA")] <- unclass(as.POSIXct(data$dtimeyyyymmdd[which(data$dtimeyyyymmdd != "NA-NA-NA")])) / (60*60*24) - unclass(as.POSIXct(data$followyyyymmdd[which(data$dtimeyyyymmdd != "NA-NA-NA")])) / (60*60*24)
# if not died
data$futime[which(data$dtimeyyyymmdd == "NA-NA-NA")] <- unclass(as.POSIXct("2004-12-31")) / (60*60*24) - unclass(as.POSIXct(data$followyyyymmdd[which(data$dtimeyyyymmdd == "NA-NA-NA")])) / (60*60*24)

write.table(data,file="D:/OneDrive - 國立台灣大學/109-1/流行病學實例討論(no exam)/3. cohort/data.csv",sep=",",row.names=F, na = "NA")


library(ggplot2)



age <- c(0.00,4.81,4.92,9.26,8.96,24.00,30.77,12.50)
x <- factor(c("31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70"))
data1 <- data.frame(x,count=age)

ggplot(data = data1) +  
    geom_bar(stat = 'identity', aes(x=x, y=count), size=1, group=1, fill="steelblue", color="steelblue") + 
    geom_text(aes(x = x, y = count, label = count), vjust = -1, size = 6)+
    
    xlab("age level") + ylab("case proportion(%)") + ggtitle("Case proportion of\n each age level") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20),
          axis.text.y = element_text(vjust = 0.5, size = 20),
          axis.title = element_text(hjust = 0.5, size = 20),
          plot.title = element_text(hjust = 0.5, size = 25), # 將title置中
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)
    ) 

age <- c(3.61,6.53,14.66,27.12)
x <- factor(c("31-40", "41-50", "51-60", "61-70"))
data1 <- data.frame(x,count=age)

ggplot(data = data1) +  
    geom_bar(stat = 'identity', aes(x=x, y=count), size=1, group=1, fill="steelblue", color="steelblue") + 
    geom_text(aes(x = x, y = count, label = count), vjust = -1, size = 6)+
    
    xlab("age level") + ylab("case proportion(%)") + ggtitle("Case proportion of\n each age level") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20),
          axis.text.y = element_text(vjust = 0.5, size = 20),
          axis.title = element_text(hjust = 0.5, size = 20),
          plot.title = element_text(hjust = 0.5, size = 25), # 將title置中
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)
    ) 

sex <- c(8.56,9.01)
x <- factor(c("Female", "Male"))
data1 <- data.frame(x,count=sex)

ggplot(data = data1) +  
    geom_bar(stat = 'identity', aes(x=x, y=count), size=1, group=1, fill="orange", color="orange") + 
    geom_text(aes(x = x, y = count, label = count), vjust = -1, size = 6)+
    
    xlab("sex") + ylab("case proportion(%)") + ggtitle("Case proportion of each sex") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20),
          axis.text.y = element_text(vjust = 0.5, size = 20),
          axis.title = element_text(hjust = 0.5, size = 20),
          plot.title = element_text(hjust = 0.5, size = 25), # 將title置中
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)
    ) 



mut <- c(1.19,2.56,7.03,11.53)
x <- factor(c("10","01","11","00"), levels = c("10","01","11","00"))
data1 <- data.frame(x,count=mut)
levels(data1$x)

ggplot(data = data1) +  
    geom_bar(stat = 'identity', aes(x=x, y=count), size=1, group=1, fill="salmon", color="salmon") + 
    geom_text(aes(x = x, y = count, label = count), vjust = -1, size = 6)+
    
    xlab("mutaion process") + ylab("case proportion(%)") + ggtitle("Case proportion of \neach mutation process") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20),
          axis.text.y = element_text(vjust = 0.5, size = 20),
          axis.title = element_text(hjust = 0.5, size = 20),
          plot.title = element_text(hjust = 0.5, size = 25), # 將title置中
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)
    ) 


mut <- c(10.49,1.85)
x <- factor(c("11 and 00", "10 or 01"), levels = c("11 and 00", "10 or 01"))
data1 <- data.frame(x,count=mut)
levels(data1$x)

ggplot(data = data1) +  
    geom_bar(stat = 'identity', aes(x=x, y=count), size=1, group=1, fill="salmon", color="salmon") + 
    geom_text(aes(x = x, y = count, label = count), vjust = -1, size = 6)+
    
    xlab("mutaion process") + ylab("case proportion(%)") + ggtitle("Case proportion of \neach mutation process") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20),
          axis.text.y = element_text(vjust = 0.5, size = 20),
          axis.title = element_text(hjust = 0.5, size = 20),
          plot.title = element_text(hjust = 0.5, size = 25), # 將title置中
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)
    ) 


## Fit Cox PH model

library(survival)

data(data,package="survival")

force(data)

attach(data)

data$fustat<-ifelse(data$DISEASE=="death",1,0)

## Create a survival object

s<-Surv(data$futime,data$fustat)

## Cox regression

coxmodel<-coxph(s~strata(factor(mut)),ties="breslow",data=data)

summary(coxmodel)

# strata(variable) 以variable分層

## plot mean response

coxfit<-survfit(coxmodel)

# newdata= 以newdata=data.frame()分層

plot(coxfit,mark.time=F,conf.int = F, 
     
     main="survival curves",
     
     xlab="Follow-up time (days)",
     
     ylab="Survival probability",lty=c(1,2,3,4),lwd=c(2,2,2,2),col=c(1,2,3,4))

legend("bottomleft",c("","","",""),lty=c(1,2,3,4),lwd=c(2,2,2,2),col=c(1,2,3,4))



######## km method
s<-Surv(data$futime, data$DISEASE==1)
#######################################


fit<-survfit(s~1) #rx+resid.ds分層  

summary(fit)

## plot Kaplan-Meier survival curves

plot(fit,mark.time =T,conf.int = T,     col=c(1,2,3),lty=c(1,2,2),lwd=c(2,2,2), 
     
     main="Survival curves",
     
     xlab="Follow-up time (days)",
     
     ylab="Survival probability"
     
)


######## km method
fit<-survfit(s~factor(data$age_g_4)) #rx+resid.ds分層  

summary(fit)

## plot Kaplan-Meier survival curves

plot(fit,mark.time =T,conf.int = F,     col=c(6,2,3,4),lwd=c(2,2,2,2), 
     
     main="Survival curves strafied by age",
     
     xlab="Follow-up time (days)",
     
     ylab="Survival probability"
)
legend("bottomleft",c("31-40","41-50","51-60","61-70"),lwd=c(2,2,2,2),col=c(6,2,3,4))


######## km method
fit<-survfit(s~factor(data$SEX)) #rx+resid.ds分層  

summary(fit)

## plot Kaplan-Meier survival curves

plot(fit,mark.time =T,conf.int = F,     col=c(5,2,3,4),lwd=c(2,2,2,2), 
     
     main="Survival curves strafied by sex",
     
     xlab="Follow-up time (days)",
     
     ylab="Survival probability"
)
legend("bottomleft",c("Female", "Male"),lwd=c(2,2,2,2),col=c(5,2,3,4))



######## km method
fit<-survfit(s~factor(data$mutation)) #rx+resid.ds分層  
levels(factor(data$mutation))
summary(fit)

## plot Kaplan-Meier survival curves

plot(fit,mark.time =T,conf.int = F,     col=c(6,2,3,4),lwd=c(2,2,2,2), 
     
     main="Survival curves strafied by mutation process",
     
     xlab="Follow-up time (days)",
     
     ylab="Survival probability"
)
legend("bottomleft",levels(factor(data$mutation)),lwd=c(2,2,2,2),col=c(6,2,3,4))


######## km method

levels(factor(data$mut))
fit<-survfit(s~factor(data$mut)) #rx+resid.ds分層  
summary(fit)

## plot Kaplan-Meier survival curves

plot(fit,mark.time =T,conf.int = F, col=c(6,2,3,4),lwd=c(2,2,2,2), 
     
     main="Survival curves strafied by mutation process",
     
     xlab="Follow-up time (days)",
     
     ylab="Survival probability"
)
legend("bottomleft",c("00 or 11", "01 or 10"),lwd=c(2,2,2,2),col=c(6,2,3,4))


######## km method
cut <- subset(data, age_g_2==1)
s<-Surv(cut$futime, cut$DISEASE==1)

levels(factor(cut$mutation))
fit<-survfit(s~factor(cut$mutation)) #rx+resid.ds分層  
summary(fit)

## plot Kaplan-Meier survival curves

plot(fit,mark.time =T,conf.int = F, col=c(6,2,3,4),lwd=c(2,2,2,2), 
     
     main="Survival curves strafied by mutation process \n for age 51-70",
     
     xlab="Follow-up time (days)",
     
     ylab="Survival probability"
)
legend("bottomleft",c("00","01","10","11") ,lwd=c(2,2,2,2),col=c(6,2,3,4))

# log rank test
survdiff(s~factor(data$mutation))
survdiff(s~factor(data$age_g_4))
survdiff(s~factor(data$SEX))
survdiff(s~factor(data$mut))
survdiff(s~factor(cut$mutation))

######## km method
datacut <- subset(data, data$age_g_4 == 2 | data$age_g_4 == 3)
s<-Surv(datacut$futime, datacut$DISEASE == 1)
fit<-survfit(s~factor(datacut$mut)) #rx+resid.ds分層  

## plot Kaplan-Meier survival curves

plot(fit,mark.time =T,conf.int = F, col=c(6,2),lwd=c(2,2), 
     
     main="Survival curves strafied by \n mutation process for age 51-70",
     
     xlab="Follow-up time (days)",
     
     ylab="Survival probability"
)
legend("bottomleft",c("00 or 11", "01 or 10"),lwd=c(2,2),col=c(6,2))
survdiff(s~factor(datacut$mut))


# INTERACTION


# 畫交互作用
data1 <- subset(data, data$DISEASE == 0)
table(data1$age_g_2, data1$SEX)


###################
install.packages("fmsb")
library(fmsb)

### 算分層後的RR和PVALUE
data1 <- subset(data, data$age_g_2 == 1)
a <- table(data1$mut, data1$DISEASE)
a
riskratio(a[4], a[3] , a[2]+a[4],a[1] + a[3])


### 看交互作用MH METHOD
install.packages("epiR")
library(epiR)

# MH RR
a <- table(data$age, data$DISEASE)
epi.mh(a[4],a[2]+a[4],a[3],a[1] + a[3],names, method = "risk.ratio")

# B test

cc <- table(data$mut,data$DISEASE, data$age_g_2)
epi.2by2(dat=cc, method = "cohort.count",
         conf.level = 0.95, units = 100, outcome = "as.columns")

### crude RR
d <- table(data$age_g_2, data$DISEASE)
d
riskratio(d[4], d[3] , d[2]+d[4],d[1] + d[3])
