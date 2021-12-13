data <- read.csv(file.choose())
names(data)[1] <- "year"

# total pop = 100
standardpop <- read.csv(file.choose())
names(standardpop)[1] <- "agegp"
names(standardpop)[2] <- "standpop"
standardpop <- standardpop[- 19,]

### 年齡標準化死亡率-直接標準化
## 先算出各年齡層原始死亡率
## 再乘到標準人口各年齡層人口數->標準人口各年齡層死亡人數
## 加總得到標準人口總死亡人數
## 除以標準人口總人口數
## 算出年齡標準化死亡率


###################################### 1

####### all

allm <- aggregate(data[,c(5,6)], by=list(year=data$year, agegp=data$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年各年齡層死亡率
allm$m <- allm$death / allm$pop
# 合併原資料與標準人口
allm <- merge(allm, standardpop, by=c("agegp"))
# 計算標準人口死亡數
allm$standdeath <- allm$m * allm$standpop
# 各年加總死亡數
allyearm <- aggregate(allm[,c(7)], by=list(year=allm$year), FUN=sum)
# 計算各年死亡率(每十萬人口)
allyearm$standard_m <- allyearm$x / 100 * 100000


####### male

# 擷取男性資料
maledata <- subset(data, sex == 1)[, c(1, 4, 5, 6)]
# 以年與年齡組分類加總
malem <- aggregate(maledata[,c(3,4)], by=list(year=maledata$year, agegp=maledata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年各年齡層死亡率
malem$m <- malem$death / malem$pop
# 合併原資料與標準人口
malem <- merge(malem, standardpop, by=c("agegp"))
# 計算標準人口死亡數
malem$standdeath <- malem$m * malem$standpop
# 各年加總死亡數
maleyearm <- aggregate(malem[,c(7)], by=list(year=malem$year), FUN=sum)
# 計算各年死亡率(每十萬人口)
maleyearm$standard_m <- maleyearm$x / 100 * 100000

####### female

# 擷取女性資料
femaledata <- subset(data, sex == 2)[, c(1, 4, 5, 6)]
# 以年與年齡組分類加總
femalem <- aggregate(femaledata[,c(3,4)], by=list(year=femaledata$year, agegp=femaledata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年各年齡層死亡率
femalem$m <- femalem$death / femalem$pop
# 合併原資料與標準人口
femalem <- merge(femalem, standardpop, by=c("agegp"))
# 計算標準人口死亡數
femalem$standdeath <- femalem$m * femalem$standpop
# 各年加總死亡數
femaleyearm <- aggregate(femalem[,c(7)], by=list(year=femalem$year), FUN=sum)
# 計算各年死亡率(每十萬人口)
femaleyearm$standard_m <- femaleyearm$x / 100 * 100000


####### plot
library(ggplot2)
windowsFonts(A=windowsFont("微軟正黑體"))

all <- data.frame(X1=allyearm$year, X2=allyearm$standard_m, Category="all")
male <- data.frame(X1=maleyearm$year, X2=maleyearm$standard_m, Category="male")
female <- data.frame(X1=femaleyearm$year, X2=femaleyearm$standard_m, Category="female")

plotframe <- data.frame(rbind(all, male, female))

ggplot(data = plotframe) +  
    
    geom_line(aes(x = X1, y = X2, color=Category, group=Category), size = 1) + 
    geom_point(aes(x = X1, y = X2, color=Category, group=Category), size = 2) +
    xlab("民國年") + ylab("年齡標準化死亡率(每十萬人口)") + ggtitle("癌症年齡標準化死亡率") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20,family = "A"),
          axis.text.y = element_text(vjust = 0.5, size = 20,family = "A"),
          axis.title = element_text(hjust = 0.5, size = 20,family = "A",face="bold"),
          plot.title = element_text(hjust = 0.5, size = 20,family = "A",face="bold"), # 將title置中
          legend.text = element_text(size = 20,family = "A"),
          legend.title = element_text(size = 20,family = "A",face="bold")
    )


###################################### 2
####### plot

# use 1st question's male and female
plotframe <- data.frame(male, female)
plotframe$sexratio <- plotframe$X2 / plotframe$X2.1


ggplot(data = plotframe) +  
    
    geom_line(aes(x = X1, y = sexratio), size = 1.5, color="pink3") + 
    geom_point(aes(x = X1, y = sexratio), size = 3, color="pink3") +
    geom_smooth(aes(x = X1, y = sexratio),method="lm", se=TRUE, fullrange=FALSE, level=0.95, color="steelblue") + 
    xlab("民國年") + ylab("性比例(男/女)") + ggtitle("癌症年齡標準化死亡率性比例") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20,family = "A"),
          axis.text.y = element_text(vjust = 0.5, size = 20,family = "A"),
          axis.title = element_text(hjust = 0.5, size = 20,family = "A",face="bold"),
          plot.title = element_text(hjust = 0.5, size = 20,family = "A",face="bold"), # 將title置中
          legend.text = element_text(size = 20,family = "A"),
          legend.title = element_text(size = 20,family = "A",face="bold")
    )


###################################### 3

# import city chinese name
city_zhdata <- read.csv(file.choose(), fileEncoding="big5")

# 把年分五組
data$yeargp <- ifelse(data$year <= 64, 1, 
                      ifelse(data$year <= 69, 2,
                             ifelse(data$year <= 74, 3,
                                    ifelse(data$year <= 79, 4,
                                           ifelse(data$year <= 84, 5,
                                                  ifelse(data$year <= 89, 6
                                                         )
                                                  )
                                           )
                                    )
                             )
                      )

# 把高雄 台中 台南 合併
# 把高雄縣 高雄市 台中縣 台中市 台南縣 台南市 取出
# 42 2 36 17 41 21
tempdata <- subset(data, address == 42 | 
                      address == 2 | 
                      address == 36 |  
                      address == 17 | 
                      address == 41 | 
                      address == 21
                  )
tempdata$newaddress <- tempdata$address
tempdata$newaddress <- ifelse(tempdata$address == 42 | tempdata$address == 2, 51, tempdata$newaddress)
tempdata$newaddress <- ifelse(tempdata$address == 36 | tempdata$address == 17, 52, tempdata$newaddress)
tempdata$newaddress <- ifelse(tempdata$address == 41 | tempdata$address == 21, 53, tempdata$newaddress)

subdata <- aggregate(tempdata[,c(5,6)], by=list(year=tempdata$year, sex=tempdata$sex, address=tempdata$newaddress, agegp=tempdata$agegp, yeargp=tempdata$yeargp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)

newdata <- data[- which(data$address == 42 | 
                            data$address == 2 | 
                            data$address == 36 |  
                            data$address == 17 | 
                            data$address == 41 | 
                            data$address == 21),]
tempdata <- tempdata[,-3]
names(tempdata)[7] <- "address"
names(newdata)

data2 <- rbind(newdata,tempdata)



####### all

alldata <- data2[,c(1, 3, 4, 5, 6, 7)]
# 以年分組與縣市與年齡組分類加總
allm <- aggregate(alldata[,c(4,5)], by=list(city=alldata$address, yeargp=alldata$yeargp, agegp=alldata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年各年齡層死亡率
allm$m <- allm$death / allm$pop
# 合併原資料與標準人口
allm <- merge(allm, standardpop, by=c("agegp"))
# 計算標準人口死亡數
allm$standdeath <- allm$m * allm$standpop
# 各年加總各年齡層死亡數
allyearm <- aggregate(allm[,8], by=list(city=allm$city, yeargp=allm$year), FUN=sum)
# 計算各年死亡率(每十萬人口)
allyearm$standard_m <- allyearm$x / 100 * 100000

# 合併中文縣市名稱 方便畫圖
allyearm <- merge(allyearm, city_zhdata, by="city")


####### male

# 擷取男性資料
maledata <- subset(data2, sex == 1)[, c(1, 3, 4, 5, 6, 7)]
# 以年分組與縣市與年齡組分類加總
malem <- aggregate(maledata[,c(4,5)], by=list(city=maledata$address, yeargp=maledata$yeargp, agegp=maledata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年各年齡層死亡率
malem$m <- malem$death / malem$pop
# 合併原資料與標準人口
malem <- merge(malem, standardpop, by=c("agegp"))
# 計算標準人口死亡數
malem$standdeath <- malem$m * malem$standpop
# 各年加總各年齡層死亡數
maleyearm <- aggregate(malem[,8], by=list(city=malem$city, yeargp=malem$year), FUN=sum)
# 計算各年死亡率(每十萬人口)
maleyearm$standard_m <- maleyearm$x / 100 * 100000

# 合併中文縣市名稱 方便畫圖
maleyearm <- merge(maleyearm, city_zhdata, by="city")


####### female

# 擷取女性資料
femaledata <- subset(data2, sex == 2)[, c(1, 3, 4, 5, 6, 7)]
# 以年分組與縣市與年齡組分類加總
femalem <- aggregate(femaledata[,c(4,5)], by=list(city=femaledata$address, yeargp=femaledata$yeargp, agegp=femaledata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年各年齡層死亡率
femalem$m <- femalem$death / femalem$pop
# 合併原資料與標準人口
femalem <- merge(femalem, standardpop, by=c("agegp"))
# 計算標準人口死亡數
femalem$standdeath <- femalem$m * femalem$standpop
# 各年加總各年齡層死亡數
femaleyearm <- aggregate(femalem[,8], by=list(city=femalem$city, yeargp=femalem$year), FUN=sum)
# 計算各年死亡率(每十萬人口)
femaleyearm$standard_m <- femaleyearm$x / 100 * 100000

# 合併中文縣市名稱 方便畫圖
femaleyearm <- merge(femaleyearm, city_zhdata, by="city")

############
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) { 
    library(grid) 
    
    # Make a list from the ... arguments and plotlist 
    plots <- c(list(...), plotlist) 
    
    numPlots = length(plots) 
    
    # If layout is NULL, then use 'cols' to determine layout 
    if (is.null(layout)) { 
        # Make the panel 
        # ncol: Number of columns of plots 
        # nrow: Number of rows needed, calculated from # of cols 
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
                         ncol = cols, nrow = ceiling(numPlots/cols)) 
    } 
    
    if (numPlots==1) { 
        print(plots[[1]]) 
        
    } else { 
        # Set up the page 
        grid.newpage() 
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) 
        
        # Make each plot, in the correct location 
        for (i in 1:numPlots) { 
            # Get the i,j matrix positions of the regions that contain this subplot 
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE)) 
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                                            layout.pos.col = matchidx$col)) 
        } 
    } 
}

####### plot
require("dplyr")
require("stringr")
require("data.table")
require("ggplot2")
require("maptools")
require("knitr")
require("kableExtra")

install.packages("sf")
library(sf)

taiwan.map <- st_read("D:\\OneDrive - 國立台灣大學\\109-1\\流行病學實例討論(no exam)\\4. descriptive epidemiology\\hw\\gadm36_TWN_shp\\gadm36_TWN_2.shp")

taiwan.map$NL_NAME_2
# plot(taiwan.map)

my.taiwan.map <- taiwan.map[c("NL_NAME_2", "geometry")]
my.taiwan.map$NL_NAME_2 <- as.character(my.taiwan.map$NL_NAME_2)
# head(my.taiwan.map)

library(dplyr)

######### merge
my.taiwan.map.data <- left_join(my.taiwan.map, allyearm, by= c("NL_NAME_2" = "city_zh"))

yearvector <- c("民國60-64年","民國65-69年","民國70-74年","民國75-79年","民國80-84年","民國85-89年")




######
for(i in 1:6){
    
    title <- yearvector[i]
    
    p <- ggplot(data = subset(my.taiwan.map.data, yeargp == i)) +
        geom_sf(aes(fill = standard_m)) + 
        ggtitle(title)+ 
        scale_fill_continuous(limits=c(2,11),name = "年齡標準化死亡率(每十萬人口)", high= "darkgreen", low= "greenyellow") +
        
        theme(axis.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"),
              plot.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"), # 將title置中
              legend.text = element_text(size = 10,family = "A"),
              legend.title = element_text(size = 10,family = "A",face="bold")
        ) +
        lims(x = c(119, 123), y = c(21, 26))
    np <- paste0("p", i)
    assign(np, p)
}
multiplot(p1, p2, p3, p4, p5, p6, cols = 2) #還有layout參數可以調整圖片放置位置順序


####### plot
require("dplyr")
require("stringr")
require("data.table")
require("ggplot2")
require("maptools")
require("knitr")
require("kableExtra")

install.packages("sf")
library(sf)

taiwan.map <- st_read("D:\\OneDrive - 國立台灣大學\\109-1\\流行病學實例討論(no exam)\\4. descriptive epidemiology\\hw\\gadm36_TWN_shp\\gadm36_TWN_2.shp")

taiwan.map$NL_NAME_2
# plot(taiwan.map)

my.taiwan.map <- taiwan.map[c("NL_NAME_2", "geometry")]
my.taiwan.map$NL_NAME_2 <- as.character(my.taiwan.map$NL_NAME_2)
# head(my.taiwan.map)

library(dplyr)


####################
# merge
my.taiwan.map.data <- left_join(my.taiwan.map, maleyearm, by= c("NL_NAME_2" = "city_zh"))

yearvector <- c("民國60-64年","民國65-69年","民國70-74年","民國75-79年","民國80-84年","民國85-89年")


######
for(i in 1:6){
    
    title <- yearvector[i]
    
    p <- ggplot(data = subset(my.taiwan.map.data, yeargp == i)) +
        geom_sf(aes(fill = standard_m)) + 
        ggtitle(title)+ 
        scale_fill_continuous(limits=c(2, 13), name = "男性年齡標準化死亡率(每十萬人口)", high= "deepskyblue4", low= "lightblue") +
        
        theme(axis.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"),
              plot.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"), # 將title置中
              legend.text = element_text(size = 10,family = "A"),
              legend.title = element_text(size = 10,family = "A",face="bold")
        ) +
        lims(x = c(119, 123), y = c(21, 26))
    np <- paste0("p", i)
    assign(np, p)
}
multiplot(p1, p2, p3, p4, p5, p6, cols = 2) #還有layout參數可以調整圖片放置位置順序

####################
# merge
my.taiwan.map.data <- left_join(my.taiwan.map, femaleyearm, by= c("NL_NAME_2" = "city_zh"))

yearvector <- c("民國60-64年","民國65-69年","民國70-74年","民國75-79年","民國80-84年","民國85-89年")


######
for(i in 1:6){
    
    title <- yearvector[i]
    
    p <- ggplot(data = subset(my.taiwan.map.data, yeargp == i)) +
        geom_sf(aes(fill = standard_m)) + 
        ggtitle(title)+ 
        scale_fill_continuous(limits=c(2, 11), name = "女性年齡標準化死亡率(每十萬人口)", high= "brown3", low= "pink2") +
        
        theme(axis.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"),
              plot.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"), # 將title置中
              legend.text = element_text(size = 10,family = "A"),
              legend.title = element_text(size = 10,family = "A",face="bold")
        ) +
        lims(x = c(119, 123), y = c(21, 26))
    np <- paste0("p", i)
    assign(np, p)
}
multiplot(p1, p2, p3, p4, p5, p6, cols = 2) #還有layout參數可以調整圖片放置位置順序


###################################### 4

# 沿用第三題資料

####### 台灣平均各年齡層預期死亡率
alldata <- data2[,c(1, 3, 4, 5, 6, 7)]
# 以年分組與縣市與年齡組分類加總
allm <- aggregate(alldata[,c(4,5)], by=list(yeargp=alldata$yeargp, agegp=alldata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年各年齡層死亡率
allm$m <- allm$death / allm$pop

####### 各縣市各年齡層人口數

citydata <- data2[,c(1, 3, 4, 5, 6, 7)] 
citym <- aggregate(citydata$pop, by=list(city=citydata$address, yeargp=citydata$yeargp, agegp=citydata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)

mergedata <- merge(citym, allm, by=c("yeargp", "agegp"))

mergedata$expecteddeath <- mergedata$x * mergedata$m
exp <- aggregate(mergedata$expecteddeath, by=list(city=mergedata$city, yeargp=mergedata$yeargp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(exp)[3] <- "exp"

####### 總觀察死亡人數
## 加總各年組各縣市死亡人數
obs <- aggregate(data2$death, by=list(city=data2$address, yeargp=data2$yeargp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
names(obs)[3] <- "obs"

smr <- merge(obs, exp, by=c("city", "yeargp"))
smr$SMR <- smr$obs / smr$exp

# 合併中文縣市名稱 方便畫圖
smr <- merge(smr, city_zhdata, by="city")

####################
# merge
my.taiwan.map.data <- left_join(my.taiwan.map, smr, by= c("NL_NAME_2" = "city_zh"))

yearvector <- c("民國60-64年","民國65-69年","民國70-74年","民國75-79年","民國80-84年","民國85-89年")

######
mid <- 1
for(i in 1:6){
    
    title <- yearvector[i]
    
    p <- ggplot(data = subset(my.taiwan.map.data, yeargp == i)) +
        geom_sf(aes(fill = SMR)) + 
        ggtitle(title)+ 
        scale_fill_continuous(name="SMR") +
        scale_fill_gradient2(limits=c(0,1.5), midpoint = mid, low = "blue4", mid = "white", high = "red4") + 
        theme(axis.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"),
              plot.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"), # 將title置中
              legend.text = element_text(size = 10,family = "A"),
              legend.title = element_text(size = 10,family = "A",face="bold")
        ) +
        lims(x = c(119, 123), y = c(21, 26))
    np <- paste0("p", i)
    assign(np, p)
}
multiplot(p1, p2, p3, p4, p5, p6, cols = 2) #還有layout參數可以調整圖片放置位置順序



################## 全部平均SMR

####### 台灣平均各年齡層預期死亡率
alldata <- data2[,c(1, 3, 4, 5, 6, 7)]
# 以年分組與縣市與年齡組分類加總
allm <- aggregate(alldata[,c(4,5)], by=list(agegp=alldata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年各年齡層死亡率
allm$m <- allm$death / allm$pop

####### 各縣市各年齡層人口數

citydata <- data2[,c(3, 4, 5, 6)] 
citym <- aggregate(citydata$pop, by=list(city=citydata$address,agegp=citydata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)

mergedata <- merge(citym, allm, by=c("agegp"))

mergedata$expecteddeath <- mergedata$x * mergedata$m
exp <- aggregate(mergedata$expecteddeath, by=list(city=mergedata$city), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(exp)[2] <- "exp"

####### 總觀察死亡人數
## 加總各年組各縣市死亡人數
obs <- aggregate(data2$death, by=list(city=data2$address), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
names(obs)[2] <- "obs"

smr <- merge(obs, exp, by=c("city"))
smr$SMR <- smr$obs / smr$exp

# 合併中文縣市名稱 方便畫圖
smr <- merge(smr, city_zhdata, by="city")

####################
# merge
my.taiwan.map.data <- left_join(my.taiwan.map, smr, by= c("NL_NAME_2" = "city_zh"))

######
mid <- 1
ggplot(data = subset(my.taiwan.map.data)) +
    geom_sf(aes(fill = SMR)) + 
    ggtitle("民國60-89年平均標準化死亡比")+ 
    scale_fill_continuous(name="SMR") +
    scale_fill_gradient2(midpoint = mid, low = "blue4", mid = "white",
                            high = "red4") + 
    
    theme(axis.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"),
          plot.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"), # 將title置中
          legend.text = element_text(size = 10,family = "A"),
          legend.title = element_text(size = 10,family = "A",face="bold")
    ) +
    lims(x = c(119, 123), y = c(21, 26))


########################################## 8

######
stand_incidence <- read.csv(file.choose())
names(stand_incidence)[1] <- "year"



all <- data.frame(X1=allyearm$year, X2=allyearm$standard_m, Category="mortality")
male <- data.frame(X1=maleyearm$year, X2=maleyearm$standard_m, Category="male")
female <- data.frame(X1=femaleyearm$year, X2=femaleyearm$standard_m, Category="female")
inc <- data.frame(X1=stand_incidence$year, X2=stand_incidence$stand_incidence, Category="incidence")

plotframe <- data.frame(rbind(all, inc))

ggplot(data = plotframe) +  
    
    geom_line(aes(x = X1, y = X2, color=Category, group=Category), size = 1) + 
    geom_point(aes(x = X1, y = X2, color=Category, group=Category), size = 2) +
    xlab("民國年") + ylab("年齡標準化死亡率與發生率(每十萬人口)") + ggtitle("癌症年齡標準化死亡率與發生率") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20,family = "A"),
          axis.text.y = element_text(vjust = 0.5, size = 20,family = "A"),
          axis.title = element_text(hjust = 0.5, size = 20,family = "A",face="bold"),
          plot.title = element_text(hjust = 0.5, size = 20,family = "A",face="bold"), # 將title置中
          legend.text = element_text(size = 20,family = "A"),
          legend.title = element_text(size = 20,family = "A",face="bold")
    )
######
stand_incidence_mf <- read.csv(file.choose())
names(stand_incidence_mf)[1] <- "year"
dim(stand_incidence_mf)
male_inc <- data.frame(X1=stand_incidence_mf$year[1:22], X2=stand_incidence_mf$stand_incidence[1:22], Category="incidence_male")
femle_inc <- data.frame(X1=stand_incidence_mf$year[23:44], X2=stand_incidence_mf$stand_incidence[23:44], Category="incidence_female")
male <- data.frame(X1=maleyearm$year, X2=maleyearm$standard_m, Category="mortality_male")
female <- data.frame(X1=femaleyearm$year, X2=femaleyearm$standard_m, Category="mortality_female")

plotframe <- data.frame(rbind(male_inc, femle_inc,male,female))

library(ggplot2)

ggplot(data = plotframe) +  
    
    geom_line(aes(x = X1, y = X2, color=Category, group=Category), size = 1) + 
    geom_point(aes(x = X1, y = X2, color=Category, group=Category), size = 2) +
    xlab("民國年") + ylab("年齡標準化死亡率與發生率(每十萬人口)") + ggtitle("癌症年齡標準化死亡率與發生率") +
    
    theme(axis.text.x = element_text(vjust = 0.5, size = 20,family = "A"),
          axis.text.y = element_text(vjust = 0.5, size = 20,family = "A"),
          axis.title = element_text(hjust = 0.5, size = 20,family = "A",face="bold"),
          plot.title = element_text(hjust = 0.5, size = 20,family = "A",face="bold"), # 將title置中
          legend.text = element_text(size = 20,family = "A"),
          legend.title = element_text(size = 20,family = "A",face="bold")
    )



#################################### 8

stand_inc_city <- read.csv(file.choose(), fileEncoding="UTF-8")
stand_inc_city <- subset(stand_inc_city, year == 89)
inc <- aggregate(stand_inc_city$stand_incidence, by=list(city_zh=stand_inc_city$city_zh), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)

# merge
my.taiwan.map.data <- left_join(my.taiwan.map, inc, by= c("NL_NAME_2" = "city_zh"))

######

ggplot(data = subset(my.taiwan.map.data)) +
    geom_sf(aes(fill = x)) + 
    ggtitle("民國89年平均年齡標準化發生率")+ 
    scale_fill_continuous(name = "年齡標準化發生率(每十萬人口)", high= "darkorange3", low= "lightyellow") +
    
    theme(axis.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"),
          plot.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"), # 將title置中
          legend.text = element_text(size = 10,family = "A"),
          legend.title = element_text(size = 10,family = "A",face="bold")
    ) +
    lims(x = c(119, 123), y = c(21, 26))



###################
####### all

alldata <- data2[,c(1, 3, 4, 5, 6, 7)]
alldata <- subset(alldata, year == 89)
# 以年分組與縣市與年齡組分類加總
allm <- aggregate(alldata[,c(4,5)], by=list(city=alldata$address, agegp=alldata$agegp), FUN=sum, simplify = TRUE, na.action = NULL, na.rm = TRUE)
# 計算各年齡層死亡率
allm$m <- allm$death / allm$pop
# 合併原資料與標準人口
allm <- merge(allm, standardpop, by=c("agegp"))
# 計算標準人口死亡數
allm$standdeath <- allm$m * allm$standpop
# 各年加總各年齡層死亡數
allyearm <- aggregate(allm[,7], by=list(city=allm$city), FUN=sum)
# 計算各年死亡率(每十萬人口)
allyearm$standard_m <- allyearm$x / 100 * 100000

# 合併中文縣市名稱 方便畫圖
allyearm <- merge(allyearm, city_zhdata, by="city")
# merge
my.taiwan.map.data <- left_join(my.taiwan.map, allyearm, by= c("NL_NAME_2" = "city_zh"))

######

ggplot(data = subset(my.taiwan.map.data)) +
    geom_sf(aes(fill = standard_m)) + 
    ggtitle("民國89年平均年齡標準化死亡率")+ 
    scale_fill_continuous(name = "年齡標準化死亡率(每十萬人口)", high= "steelblue4", low= "lightblue") +
    
    theme(axis.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"),
          plot.title = element_text(hjust = 0.5, size = 15,family = "A",face="bold"), # 將title置中
          legend.text = element_text(size = 10,family = "A"),
          legend.title = element_text(size = 10,family = "A",face="bold")
    ) +
    lims(x = c(119, 123), y = c(21, 26))