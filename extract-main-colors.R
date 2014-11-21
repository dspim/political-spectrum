#---------------------------------------
# 收集候選人的文宣照片，並萃取出主要用色
#---------------------------------------
library(EBImage)
library(jpeg)

Div <- function(x, k){
  km <- kmeans(x, k)
  p <- km$size/sum(km$size)
  exp(sum(-p*log(p)))/k
}


dat <- read.csv("台北市議員候選人資.csv", encoding="big5", stringsAsFactors=FALSE)
head(dat)
dat.dpp <- subset(dat, 推薦之政黨=="民主進步黨")
dat.kmt <- subset(dat, 推薦之政黨=="中國國民黨")
dat1 <- rbind(dat.dpp, dat.kmt)


for(i in 1:length(dat1$姓名)){
  if(dat1$圖片網址[i]=="") stop
  else{
    filename <- paste("images/", paste(dat1[i,-6], collapse = "-"),".jpg", sep="")
    download.file(dat1$圖片網址[i], filename, mode = 'wb')
    
    if(length(grep(".png", dat1$圖片網址[i])) > 1){
      im <- readImage(filename, type = "png")
      writeImage(im, filename, type = "jpeg") 
    }
  }
}


for(i in 29:length(dat1$姓名)){
  ptm <- proc.time()
  if(dat.kmt$圖片網址[i]=="") stop
  else{
    #dat1 <- dat.kmt
    filename <- paste("images/", paste(dat1[i,-6], collapse = "-"),".jpg", sep="")
    raw <- readJPEG(filename)
    x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
    test <- sapply(3:8, function(k) Div(x, k))
    # plot(test, type="l")
    
    fit <- kmeans(x, centers = which.min(test)+2, iter.max = 20)
    out <- data.frame(fit$centers, size=fit$size)
    out <- out[order(-out$size), ]
    col <- c(out[1, -4])
    img <- array(data = 0, dim = c(300,300,3))
    img[,,1] <- col$r
    img[,,2] <- col$g
    img[,,3] <- col$b
    writeJPEG(img, paste("images/", paste(dat1[i,-6], collapse = "-"),"-c1.jpg", sep=""))
    
    
    p <- sweep(out[,-4], 1, rowSums(out[,-4]), "/")
    id0 <- apply(out[,-4], 1, function(x) sqrt(sum((1-x)^2)) < 0.2) # 保留白色
    id1 <- apply(p, 1, function(p) 1/sum(p^2)) < 2.8 # 扣除灰色
    id2 <- apply(out[,-4], 1, function(x) sqrt(sum(x^2)) > 0.2) # 扣除黑色
    id3 <- apply(out[,-4], 1, function(x) sqrt(sum((x-c(0.95,0.8,0.75))^2)) > 0.2) # 扣除膚色  
    id <- (id0 | id1) & id2 & id3
    tmp <- c(out[id,-4][1,])
    if(identical(col, tmp)){
      if(sum(id)>1){
        col <- c(out[id,-4][2,])
      }else{
        col <- c(out[2, -4])    
      }
    }else{
      col <- tmp
    }
    img[,,1] <- col$r
    img[,,2] <- col$g
    img[,,3] <- col$b
    writeJPEG(img, paste("images/", paste(dat1[i,-6], collapse = "-"),"-c2.jpg", sep=""))   
  }
  cat(i, dat1[i,2], (proc.time() - ptm)[3], "\n")
}



