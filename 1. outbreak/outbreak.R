data <- read.csv(file.choose())

data$index <- data$diarrhea + data$fever + data$stomach_ache +  data$vomiting +  data$fatigued + data$watery_stool
data$case <- ifelse(data$index >=4, 1, 0)

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

  geom_line(aes(x = date2, y = count), size = 1, group = 1, color="lightblue3") + 
  geom_point(aes(x = date2, y = count), size = 2, color="lightblue3") +
  geom_text(aes(x = date2, y = count, label = count), vjust = -1, size = 5)+
  
  xlab("date") + ylab("count") + ggtitle("流行曲線") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10),
        axis.title = element_text(hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  ) 