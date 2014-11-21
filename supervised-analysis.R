# 處理 2010 台北市議員得票明細 資料
tmp2010 <- read.csv("2010台北市議員選舉得票明細.csv", encoding="big5", stringsAsFactors=FALSE)
tmp2010$推薦政黨 <- sub("民主進步黨","民進黨", tmp2010$推薦政黨)
tmp2010$推薦政黨 <- sub("中國國民黨","國民黨", tmp2010$推薦政黨)
tmp2010$推薦政黨 <- sub("無黨籍及未經政黨推薦","無", tmp2010$推薦政黨)

dat2010 <- subset(tmp2010, 當選註記=="*" & (推薦政黨=="民進黨" | 推薦政黨=="國民黨"))[,c(1,2,6,8)]
dat2010$得票率 <- as.numeric(sub("%","",dat2010$得票率))


# 匯入 2014 台北市議員候選人資料
dat2014 <- read.csv("台北市議員候選人資料.csv", encoding="big5", stringsAsFactors=FALSE)

# 加入 候選人文宣對應的色碼資訊
library(jpeg)
filename <- dir("main-color/")
name <- do.call(rbind,strsplit(dir("main-color//"), split="-"))[,2]
col <- t(sapply(filename, function(f) apply(readJPEG(paste("main-color/", f, sep="")), 3, mean)))
rownames(col) <- do.call(rbind,strsplit(rownames(col), split="-"))[,2]
colnames(col) <- c("r", "g", "b")
col <- data.frame(姓名=rownames(col), col)

dat2014_rgb <- merge(dat2014, col, sort=FALSE)
dat2014_rgb <- dat2014_rgb[,c(2,1,3,4,5,7,8,9,10,6)]

dat2014_merged <- merge(dat2014_rgb[,-10], subdat2010)
colnames(dat2014_merged)[10] <- "rate"

out <- data.frame(dat2014_merged[,c("r","g","b","rate")])
library(rpart)
library(rpart.plot)
fit <- rpart(rate~r+g+b ,data=out)
prp(fit, extra = 1)
x1 <- subset(out, b>=0.84)
x2 <- subset(out, b<0.84 & g <0.57)
x3 <- subset(out, b<0.84 & g >=0.57)

x1 <- x1[order(x1[2],x1[1],x1[3]),]
x2 <- x2[order(x2[1],x2[3],x2[2]),]
x3 <- x3[order(x3[1],x3[3],x3[2]),]

col1 <- apply(x1, 1, function(x) rgb(x[1],x[2],x[3]))
col2 <- apply(x2, 1, function(x) rgb(x[1],x[2],x[3]))
col3 <- apply(x3, 1, function(x) rgb(x[1],x[2],x[3]))


a1 <- data.frame(x=c(1,rep(1:2, each = 4)), y1=c(0:4, seq(0,5-5/4,l=4)), y2=c(1:5, seq(0,5-5/4,l=4)+5/4))
a2 <- data.frame(x=rep(3:4, each = 6)+0.2, y1=rep(seq(0,5-5/6,l=6),2), y2=rep(seq(0,5-5/6,l=6)+5/6,2))
a3 <- data.frame(x=rep(5:6, each = 7)+0.4, y1=rep(seq(0,5-5/7,l=7),2), y2=rep(seq(0,5-5/7,l=7)+5/7,2))
df <- data.frame(rbind(a1,a2,a3), col=c(col1, col2, col3))

library(ggplot2)                       
theme_layz <- function(){
  theme(axis.line=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(), 
        axis.title=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
}

ggplot(df) + scale_fill_identity() + theme_layz() +
  geom_rect(aes(xmin=x,xmax=x+1,ymin=y1,ymax=y2, fill=col))
ggsave("output/rgb-grouping.png",dpi=150)
