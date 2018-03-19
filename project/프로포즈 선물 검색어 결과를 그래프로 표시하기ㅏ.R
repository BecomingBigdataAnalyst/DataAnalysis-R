setwd("c:\\r_temp")  
install.packages("KoNLP") 
library(KoNLP)  
txt <- readLines("propose.txt") 
pro <- sapply(txt,extractNoun,USE.NAMES=F)
pro 
c <- unlist(pro)
pro2 <- gsub("\\.","",c)
pro2 <- gsub("\\n","",pro2) 
pro2 <- gsub("\\d+","",pro2) 
txt <- readLines("프로포즈gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt
for( i in 1:cnt_txt) {
  pro2 <-gsub((txt[i]),"",pro2)     
}
pro2 <- Filter(function(x) {nchar(x) >= 2} ,pro2) 
pro2 <- Filter(function(x) {nchar(x) <= 6} ,pro2) 
head(unlist(pro2), 20)
write(unlist(pro2),"pro_3.txt") 
rev <- read.table("pro_3.txt")
nrow(rev) 
wordcount <- table(rev)
head(sort(wordcount, decreasing=T),20)
a <- head(sort(wordcount, decreasing=T),10)

bp <- barplot(a,  main = "프로포즈 선물 TOP 10 ", col = rainbow(10), cex.names=0.7, 
              las = 2,ylim=c(0,60))
pct <- round(a/sum(a) * 100 ,1)
text(x = bp, y = a*1.05, labels = paste(a,"건"), col = "black", cex = 0.7)
text(x = bp, y = a*0.85, labels = paste("(",pct,"%",")"), col = "black", cex = 0.7)




#[ Pie chart 소스 코드 ]

setwd("c:\\r_temp")  
install.packages("KoNLP") 
library(KoNLP)  
txt <- readLines("propose.txt") 
pro <- sapply(txt,extractNoun,USE.NAMES=F)
pro 
c <- unlist(pro) 
pro2 <- gsub("\\.","",c)
pro2 <- gsub("\\n","",pro2) 
pro2 <- gsub("\\d+","",pro2) 
txt <- readLines("프로포즈gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt
for( i in 1:cnt_txt) {
  pro2 <-gsub((txt[i]),"",pro2)     
}
pro2 <- Filter(function(x) {nchar(x) >= 2} ,pro2) 
pro2 <- Filter(function(x) {nchar(x) <= 6} ,pro2) 
head(unlist(pro2), 20)
write(unlist(pro2),"pro_3.txt") 
rev <- read.table("pro_3.txt")
nrow(rev) 
wordcount <- table(rev)
head(sort(wordcount, decreasing=T),20)
a <- head(sort(wordcount, decreasing=T),10)
pct <- round(a/sum(a) * 100 ,1)
names(a)
lab <- paste(names(a),"\n",pct,"%")
pie(a,main="프로포즈 선물 TOP 10",col=rainbow(10), cex=0.8,labels = lab)

