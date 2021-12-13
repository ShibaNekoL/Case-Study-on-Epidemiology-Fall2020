install.packages("foreign")
library("foreign")
esocancer <- read.csv(file.choose(), header =T)
View(esocancer)
install.packages("questionr")
library( questionr )
eso <- esocancer
esocancer$AGE <- ifelse(eso$AGE <= 1, "0", "1")
esocancer$SMK <- ifelse(eso$SMK <= 1, "0", "1")
esocancer$ALC <- ifelse(eso$ALC <= 3, "0", "1")

# FINAL MODEL
esocancer$SMK1 <- ifelse(esocancer$SMK == 1, 1, 0)
esocancer$SMK2 <- ifelse(esocancer$SMK == 2, 1, 0)
esocancer$SMK3 <- ifelse(esocancer$SMK == 3, 1, 0)
esocancer$SMK4 <- ifelse(esocancer$SMK == 4, 1, 0)

esocancer$ALC1 <- ifelse(esocancer$ALC == 1, 1, 0)
esocancer$ALC2 <- ifelse(esocancer$ALC == 2, 1, 0)
esocancer$ALC3 <- ifelse(esocancer$ALC == 3, 1, 0)
esocancer$ALC4 <- ifelse(esocancer$ALC == 4, 1, 0)


model.int<-glm(CASE~factor(AGE)+
                   factor(SMK2)+factor(SMK3)+factor(SMK4)+factor(SMK1)+
                   factor(ALC2)+factor(ALC3)+factor(ALC4)+factor(ALC1)+
                   factor(SMK4)*factor(ALC4),family=binomial,data=esocancer)
summary(model.int)

coe <- c()
for(i in 1:15){
    coe <- append(coe, model.int$coefficients[[i]])
}

or <- exp(coe)

odds.ratio(model.int)


## ODDS RATIO
## fit model model.AGE
model<-glm(CASE~factor(AGE)+factor(ALC),family=binomial,data=eso)
summary(model)

odds.ratio(model)


## fit model model.AGE
model.AGE<-glm(CASE~AGE,family=binomial,data=esocancer)
summary(model.AGE)

odds.ratio(model.AGE)

## fit model model.SMK
model.SMK<-glm(CASE~SMK,family=binomial,data=esocancer)
summary(model.SMK)

odds.ratio(model.SMK)

## fit model model.ALC
model.ALC<-glm(CASE~ALC,family=binomial,data=esocancer)
summary(model.ALC)

odds.ratio(model.ALC)



## fit model model.ALC
model.ALC<-glm(CASE~SMK*AGE,family=binomial,data=esocancer)
summary(model.ALC)

odds.ratio(model.ALC)

## fit model model.int
model.int<-glm(CASE~AGE+SMK+ALC+SMK*ALC,family=binomial,data=esocancer)
summary(model.int)

odds.ratio(model.int)


## fit model model.int
model.int<-glm(CASE~factor(AGE)+factor(SMK)+factor(ALC)+factor(SMK)*factor(AGE),family=binomial,data=esocancer)
summary(model.int)

odds.ratio(model.int)



## fit model model.int
model.int<-glm(CASE~factor(AGE)+factor(SMK)+factor(ALC),family=binomial,data=esocancer)
summary(model.int)

odds.ratio(model.int)


## fit model model.int

eso <- subset(esocancer, esocancer$AGE=="1")

model.int<-glm(CASE~factor(SMK),family=binomial,data=eso)
summary(model.int)

odds.ratio(model.int)

# smk and alcohol interaction
model.int<-glm(CASE~factor(ALC)+factor(SMK)+factor(ALC)*factor(SMK),family=binomial,data=esocancer)
summary(model.int)

odds.ratio(model.int)

# age and alc interaction
model.int<-glm(CASE~factor(AGE)+factor(ALC)+factor(ALC)*factor(AGE),family=binomial,data=esocancer)
summary(model.int)

odds.ratio(model.int)


## fit model model.int

model.int<-glm(CASE~factor(AGE)+factor(SMK)+factor(ALC)+factor(AGE)*factor(ALC),family=binomial,data=esocancer)
summary(model.int)

odds.ratio(model.int)



## fit model model.int


model.int<-glm(CASE~factor(AGE)+factor(SMK)+factor(ALC)+factor(AGE)*factor(ALC),family=binomial,data=esocancer)
summary(model.int)

odds.ratio(model.int)

# trend

install.packages("ggplot2", type = "source")
library(ggplot2)





## age
x <- factor(c(1:6))
AGE <- c(0.86, 4.62, 19.35, 31.40, 34.16, 29.55)
data <- data.frame(x, count=AGE)


ggplot(data = data) +  
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


## smk
x <- factor(c(1:4))
smk <- c(14.86, 24.58, 25.00, 32.93)
data <- data.frame(x, count=smk)


ggplot(data = data) +  
    geom_bar(stat = 'identity', aes(x=x, y=count), size=1, group=1, fill="salmon", color="salmon") + 
    geom_text(aes(x = x, y = count, label = count), vjust = -1, size = 6)+
    
    xlab("smoking level") + ylab("case proportion(%)") + ggtitle("Case proportion\n of each smoking level") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20),
          axis.text.y = element_text(vjust = 0.5, size = 20),
          axis.title = element_text(hjust = 0.5, size = 20),
          plot.title = element_text(hjust = 0.5, size = 25), # 將title置中
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)
    ) 




## alc

x <- factor(c(1:4))
alc <- c(6.99, 21.13, 36.96, 61.19)
data <- data.frame(x, count=alc)


ggplot(data = data) +  
    geom_bar(stat = 'identity', aes(x=x, y=count), size=1, group=1, fill="forestgreen", color="forestgreen") + 
    geom_text(aes(x = x, y = count, label = count), vjust = -1, size = 6)+
    
    xlab("alcohol level") + ylab("case proportion(%)") + ggtitle("Case proportion of\n each alcohol level") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20),
          axis.text.y = element_text(vjust = 0.5, size = 20),
          axis.title = element_text(hjust = 0.5, size = 20),
          plot.title = element_text(hjust = 0.5, size = 25), # 將title置中
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)
    ) 
