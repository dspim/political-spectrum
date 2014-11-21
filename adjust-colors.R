library(jpeg)

dat <- read.csv("political-spectrum/台北市議員候選人資料.csv", encoding="big5", stringsAsFactors=FALSE)
head(dat)
dat.dpp <- subset(dat, 推薦之政黨=="民進黨")
dat.kmt <- subset(dat, 推薦之政黨=="國民黨")
dat1 <- rbind(dat.dpp, dat.kmt)

#-------------------------------------
# 手動校正
# 2-王孝維-男-民主進步黨-6
#-------------------------------------
dat2 <- dat.dpp
i <- which(dat2[,2]=="王孝維")
filename <- paste("images/", paste(dat2[i,-6], collapse = "-"),".jpg", sep="")
raw <- readJPEG(filename)
x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
fit <- kmeans(x, centers = 8, iter.max = 20)
out <- data.frame(fit$centers, size=fit$size)
out <- out[order(-out$size), ]
img <- array(data = 0, dim = c(300,300,3))

col <- c(out[1, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c1.jpg", sep=""))

col <- c(out[8, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c2.jpg", sep="")) 


#-------------------------------------
# 手動校正
# 2-江志銘-男-民主進步黨-11
#-------------------------------------
dat2 <- dat.dpp
i <- which(dat2[,2]=="江志銘")
filename <- paste("images/", paste(dat2[i,-6], collapse = "-"),".jpg", sep="")
raw <- readJPEG(filename)
x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
fit <- kmeans(x, centers = 8, iter.max = 20)
out <- data.frame(fit$centers, size=fit$size)
out <- out[order(-out$size), ]
img <- array(data = 0, dim = c(300,300,3))

col <- c(out[2, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c1.jpg", sep=""))

col <- c(out[3, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c2.jpg", sep="")) 


#-------------------------------------
# 手動校正
# 5-周威佑-男-民主進步黨-7
#-------------------------------------
dat2 <- dat.dpp
i <- which(dat2[,2]=="周威佑")
filename <- paste("images/", paste(dat2[i,-6], collapse = "-"),".jpg", sep="")
raw <- readJPEG(filename)
x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
fit <- kmeans(x, centers = 8, iter.max = 20)
out <- data.frame(fit$centers, size=fit$size)
out <- out[order(-out$size), ]
img <- array(data = 0, dim = c(300,300,3))

col <- c(out[1, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c1.jpg", sep=""))

col <- c(out[3, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c2.jpg", sep=""))   

#-------------------------------------
# 手動校正
# 6-簡舒培-女-民主進步黨-21
#-------------------------------------
i <- 27
dat2 <- dat.dpp
filename <- paste("images/", paste(dat2[i,-6], collapse = "-"),".jpg", sep="")
raw <- readJPEG(filename)
x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
fit <- kmeans(x, centers = 8, iter.max = 20)
out <- data.frame(fit$centers, size=fit$size)
out <- out[order(-out$size), ]
img <- array(data = 0, dim = c(300,300,3))

col <- c(out[1, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c1.jpg", sep=""))

col <- c(out[6, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c2.jpg", sep=""))   



#-------------------------------------
# 手動校正
# 1-汪志冰-女-中國國民黨-13
#-------------------------------------
i <- 3
dat2 <- dat.kmt
filename <- paste("images/", paste(dat2[i,-6], collapse = "-"),".jpg", sep="")
raw <- readJPEG(filename)
x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
fit <- kmeans(x, centers = 8, iter.max = 20)
out <- data.frame(fit$centers, size=fit$size)
out <- out[order(-out$size), ]
img <- array(data = 0, dim = c(300,300,3))

col <- c(out[1, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c1.jpg", sep=""))

col <- c(out[3, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c2.jpg", sep=""))   



#-------------------------------------
# 手動校正
# 3-王正德-男-中國國民黨-17
#-------------------------------------
dat2 <- dat.kmt
i <- which(dat2[,2]=="王正德")
filename <- paste("images/", paste(dat2[i,-6], collapse = "-"),".jpg", sep="")
raw <- readJPEG(filename)
x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
fit <- kmeans(x, centers = 8, iter.max = 20)
out <- data.frame(fit$centers, size=fit$size)
out <- out[order(-out$size), ]
img <- array(data = 0, dim = c(300,300,3))

col <- c(out[1, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c1.jpg", sep=""))

col <- c(out[3, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c2.jpg", sep=""))   



#-------------------------------------
# 手動校正
# 6-李慶元-男-中國國民黨-22
#-------------------------------------
i <- 32
dat2 <- dat.kmt
filename <- paste("images/", paste(dat2[i,-6], collapse = "-"),".jpg", sep="")
raw <- readJPEG(filename)
x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
fit <- kmeans(x, centers = 8, iter.max = 20)
out <- data.frame(fit$centers, size=fit$size)
out <- out[order(-out$size), ]
img <- array(data = 0, dim = c(300,300,3))

col <- c(out[1, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c1.jpg", sep=""))

col <- c(out[4, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c2.jpg", sep=""))   



#-------------------------------------
# 手動校正
# 6-厲耿桂芳-女-中國國民黨-3
#-------------------------------------
i <- 25
dat2 <- dat.kmt
filename <- paste("images/", paste(dat2[i,-6], collapse = "-"),".jpg", sep="")
raw <- readJPEG(filename)
x <- data.frame(r=c(raw[,,1]), g=c(raw[,,2]), b=c(raw[,,3]))
fit <- kmeans(x, centers = 8, iter.max = 20)
out <- data.frame(fit$centers, size=fit$size)
out <- out[order(-out$size), ]
img <- array(data = 0, dim = c(300,300,3))

col <- c(out[1, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c1.jpg", sep=""))

col <- c(out[3, -4])
img[,,1] <- col$r
img[,,2] <- col$g
img[,,3] <- col$b
writeJPEG(img, paste("images/", paste(dat2[i,-6], collapse = "-"),"-c2.jpg", sep=""))   







