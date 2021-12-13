data <- read.csv(file.choose())

data$stool <- ifelse(data$watery_stool + data$mucous_stool + data$bloody_stool > 0, 1,0)
data$index <- data$fever + data$stomach_ache + data$vomiting + data$stool + data$diarrhea
data$case <- ifelse(data$index >= 3, 1, 0)

chisq.test(data$cuttlefish, data$case)


datecase <- data.frame(table(data$date,data$case))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

library(ggplot2)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)

ggplot(data = casecount2) +  
  
  geom_bar(stat = 'identity', aes(x=date2, y=count), size=1, group=1, fill="steelblue", color="steelblue") + 
  geom_text(aes(x = date2, y = count, label = count), vjust = -1, size = 5)+
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 15),
        axis.text.y = element_text(vjust = 0.5, size = 15),
        axis.title = element_text(hjust = 0.5, size = 15),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )



###################

data <- read.csv(file.choose())

data$stool <- ifelse(data$watery_stool + data$mucous_stool + data$bloody_stool > 0, 1,0)
data$index <- data$fever + data$stomach_ache + data$vomiting + data$stool + data$diarrhea
data$case <- ifelse(data$index >= 3, 1, 0)

chisq.test(data$cuttlefish, data$case)

library(ggplot2)


##
datecase <- data.frame(table(data$date,data$bloody_stool))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)


## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)

ggplot(data = casecount2) +  
  
  geom_bar(stat = 'identity', aes(x=date2, y=count), size=1, group=1, fill="steelblue", color="steelblue") + 
  geom_text(aes(x = date2, y = count, label = count), vjust = -1, size = 5)+
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for bloody_stool") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 15),
        axis.text.y = element_text(vjust = 0.5, size = 15),
        axis.title = element_text(hjust = 0.5, size = 15),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )




###################

data <- read.csv(file.choose())

data$stool <- ifelse(data$watery_stool + data$mucous_stool + data$bloody_stool > 0, 1,0)
data$index <- data$fever + data$stomach_ache + data$vomiting + data$stool + data$diarrhea
data$case <- ifelse(data$index >= 3, 1, 0)

chisq.test(data$cuttlefish, data$case)

library(ggplot2)


####
datecase <- data.frame(table(data$date,data$fever))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)



#### fever
datecase <- data.frame(table(data$date,data$fever))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)
casecount2$symptom <- "fever"


#### stomach_ache
datecase <- data.frame(table(data$date,data$stomach_ache))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount3 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount3$date2 <- factor(casecount3$date, levels = level)
casecount3$symptom <- "stomach_ache"



#### vomiting
datecase <- data.frame(table(data$date,data$vomiting))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount4 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount4$date2 <- factor(casecount4$date, levels = level)
casecount4$symptom <- "vomiting"



#### diarrhea
datecase <- data.frame(table(data$date,data$diarrhea))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount5 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount5$date2 <- factor(casecount5$date, levels = level)
casecount5$symptom <- "diarrhea"


#### watery_stool
datecase <- data.frame(table(data$date,data$watery_stool))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount6 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount6$date2 <- factor(casecount6$date, levels = level)
casecount6$symptom <- "watery_stool"




#### mucous_stool
datecase <- data.frame(table(data$date,data$mucous_stool))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount7 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount7$date2 <- factor(casecount7$date, levels = level)
casecount7$symptom <- "mucous_stool"



#### bloody_stool
datecase <- data.frame(table(data$date,data$bloody_stool))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount8 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount8$date2 <- factor(casecount8$date, levels = level)
casecount8$symptom <- "bloody_stool"



casecount10 <- rbind(casecount2, casecount3,casecount4, casecount5, casecount6,casecount7, casecount8)

# 固定factor順序
casecount10$symptom <- factor(casecount10$symptom, levels = c("fever", "stomach_ache", "diarrhea", "watery_stool", "mucous_stool", "bloody_stool", "vomiting"))


# bar seperate
ggplot(casecount10, aes(x=date2, y=count, fill=symptom)) + 
  geom_bar(stat = 'identity', position="dodge") + 
  # geom_text(aes(x = date2, y = count, label = count), size = 5) +
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for symptoms") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )


# bar stack
ggplot(casecount10, aes(x=date2, y=count, fill=symptom)) + 
  geom_bar(stat = 'identity', position="stack") + 
  # geom_text(aes(x = date2, y = count, label = count), size = 5) +
  scale_fill_manual(values = c("#FA8072","#00BFFF", "#FF8C00", "#EE82EE", "#32CD32", "#FF6347", "#77DDFF")) +
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for symptoms") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )


# bar stack default color
ggplot(casecount10, aes(x=date2, y=count, fill=symptom)) + 
  geom_bar(stat = 'identity', position="stack") + 
  # geom_text(aes(x = date2, y = count, label = count), size = 5) +

  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for symptoms") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )



# line
ggplot(casecount10, aes(x=date2, y=count, color=symptom, group = symptom)) + 
  geom_line(stat = 'identity',  size = 1) + 
  # geom_text(aes(x = date2, y = count, label = count), size = 5) +
  scale_color_manual(values = c("#FA8072","#00BFFF", "#FF8C00", "#B22222", "#32CD32", "#EE82EE", "#77DDFF")) +
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for symptoms") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )



# 花枝的得病

subdata <- subset(data, cuttlefish == 1)
datecase <- data.frame(table(subdata$date,subdata$case))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)


ggplot(data = casecount2) +  
  
  geom_bar(stat = 'identity', aes(x=date2, y=count), size=1, group=1, fill="steelblue", color="steelblue") + 
  geom_text(aes(x = date2, y = count, label = count), vjust = -1, size = 5)+
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for those who ate cuttlefish") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 15),
        axis.text.y = element_text(vjust = 0.5, size = 15),
        axis.title = element_text(hjust = 0.5, size = 15),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )

table(data$cuttlefish, data$case)



# 喝水的得病

subdata <- subset(data, drinking == 4)
datecase <- data.frame(table(subdata$date,subdata$case))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)


ggplot(data = casecount2) +  
  
  geom_bar(stat = 'identity', aes(x=date2, y=count), size=1, group=1, fill="steelblue", color="steelblue") + 
  geom_text(aes(x = date2, y = count, label = count), vjust = -1, size = 5)+
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for those who drink 4") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 15),
        axis.text.y = element_text(vjust = 0.5, size = 15),
        axis.title = element_text(hjust = 0.5, size = 15),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


## 吃花枝
datecase <- data.frame(table(data$date, data$cuttlefish))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)


ggplot(data = casecount2) +  
  
  geom_bar(stat = 'identity', aes(x=date2, y=count), size=1, group=1, fill="steelblue", color="steelblue") + 
  geom_text(aes(x = date2, y = count, label = count), vjust = -1, size = 5)+
  
  xlab("date") + ylab("count") + ggtitle("those who ate cuttlefish") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 15),
        axis.text.y = element_text(vjust = 0.5, size = 15),
        axis.title = element_text(hjust = 0.5, size = 15),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )



# facewashing

subdata <- subset(data, facewashing == 1)
datecase <- data.frame(table(subdata$date,subdata$case))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)


ggplot(data = casecount2) +  
  
  geom_bar(stat = 'identity', aes(x=date2, y=count), size=1, group=1, fill="steelblue", color="steelblue") + 
  geom_text(aes(x = date2, y = count, label = count), vjust = -1, size = 5)+
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for those who facewashing 1") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 15),
        axis.text.y = element_text(vjust = 0.5, size = 15),
        axis.title = element_text(hjust = 0.5, size = 15),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )













################### every symptoms

data <- read.csv(file.choose())

data$stool <- ifelse(data$watery_stool + data$mucous_stool + data$bloody_stool > 0, 1,0)
data$index <- data$fever + data$stomach_ache + data$vomiting + data$stool + data$diarrhea
data$case <- ifelse(data$index >= 3, 1, 0)

chisq.test(data$cuttlefish, data$case)

library(ggplot2)


#### fever
datecase <- data.frame(table(data$date,data$fever))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount2 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount2$date2 <- factor(casecount2$date, levels = level)
casecount2$symptom <- "fever"


#### stomach_ache
datecase <- data.frame(table(data$date,data$stomach_ache))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount3 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount3$date2 <- factor(casecount3$date, levels = level)
casecount3$symptom <- "stomach_ache"



#### vomiting
datecase <- data.frame(table(data$date,data$vomiting))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount4 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount4$date2 <- factor(casecount4$date, levels = level)
casecount4$symptom <- "vomiting"



#### diarrhea
datecase <- data.frame(table(data$date,data$diarrhea))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount5 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount5$date2 <- factor(casecount5$date, levels = level)
casecount5$symptom <- "diarrhea"


#### watery_stool
datecase <- data.frame(table(data$date,data$watery_stool))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount6 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount6$date2 <- factor(casecount6$date, levels = level)
casecount6$symptom <- "watery_stool"




#### mucous_stool
datecase <- data.frame(table(data$date,data$mucous_stool))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount7 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount7$date2 <- factor(casecount7$date, levels = level)
casecount7$symptom <- "mucous_stool"



#### bloody_stool
datecase <- data.frame(table(data$date,data$bloody_stool))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount8 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount8$date2 <- factor(casecount8$date, levels = level)
casecount8$symptom <- "bloody_stool"



#### abdominal_cramp
datecase <- data.frame(table(data$date,data$abdominal_cramp))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount9 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount9$date2 <- factor(casecount8$date, levels = level)
casecount9$symptom <- "abdominal_cramp"


#### fatigued
datecase <- data.frame(table(data$date,data$fatigued))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount10 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount10$date2 <- factor(casecount8$date, levels = level)
casecount10$symptom <- "fatigued"



#### nausea
datecase <- data.frame(table(data$date,data$nausea))

names(datecase)[1] <- "date"
names(datecase)[2] <- "case"
names(datecase)[3] <- "count"

casecount <- subset(datecase, datecase$case == 1)
casecount <- subset(casecount, casecount$date != 0)

## x軸離散變數依1007-1106排列
b <- data.frame(date=c(5,9,11,12,13,19,21,26,27,28,29,30,31),case=c(0,0,0,0,0,0,0,0,0,0,0,0,0),count=c(0,0,0,0,0,0,0,0,0,0,0,0,0))
casecount11 <- rbind(b, casecount)

level <- c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,1,2,3,4,5,6)
casecount11$date2 <- factor(casecount8$date, levels = level)
casecount11$symptom <- "nausea"


casecount12 <- rbind(casecount2, casecount3,casecount4, casecount5, casecount6,casecount7, casecount8, casecount9,casecount10, casecount11)

# 固定factor順序
# casecount12$symptom <- factor(casecount10$symptom, levels = c("fever", "stomach_ache", "diarrhea", "watery_stool", "mucous_stool", "bloody_stool", "vomiting"))


# bar seperate
ggplot(casecount12, aes(x=date2, y=count, fill=symptom)) + 
  geom_bar(stat = 'identity', position="dodge") + 
  # geom_text(aes(x = date2, y = count, label = count), size = 5) +
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for all symptoms") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )


# bar stack
ggplot(casecount12, aes(x=date2, y=count, fill=symptom)) + 
  geom_bar(stat = 'identity', position="stack") + 
  # geom_text(aes(x = date2, y = count, label = count), size = 5) +
  # scale_fill_manual(values = c("#FA8072","#00BFFF", "#FF8C00", "#EE82EE", "#32CD32", "#FF6347", "#77DDFF")) +
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for symptoms") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )


# bar stack default color
ggplot(casecount12, aes(x=date2, y=count, fill=symptom)) + 
  geom_bar(stat = 'identity', position="stack") + 
  # geom_text(aes(x = date2, y = count, label = count), size = 5) +
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for symptoms") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )



# line
ggplot(casecount12, aes(x=date2, y=count, color=symptom, group = symptom)) + 
  geom_line(stat = 'identity',  size = 1) + 
  # geom_text(aes(x = date2, y = count, label = count), size = 5) +
  scale_color_manual(values = c("#FA8072","#00BFFF", "#FF8C00", "#B22222", "#32CD32", "#EE82EE", "#77DDFF")) +
  
  xlab("date") + ylab("count") + ggtitle("Epidemic Curve for symptoms") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


