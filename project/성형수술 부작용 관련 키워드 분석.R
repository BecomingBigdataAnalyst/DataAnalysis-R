setwd("c:\\r_temp")  
install.packages("KoNLP") 
install.packages("wordcloud") 
library(KoNLP)  
library(wordcloud)
useSejongDic() 
data1 <- readLines("remake2.txt") 
data1
data2 <- sapply(data1,extractNoun,USE.NAMES=F)
data2
head(unlist(data2), 30) 
data3 <- unlist(data2) 
data3 <- Filter(function(x) {nchar(x) <= 10} ,data3)
head(unlist(data3), 30)
data3 <- gsub("\\d+","", data3) ##  <--- 모든 숫자 없애기
data3 <- gsub("\\.","",data3)
data3 <- gsub(" ","",data3)
data3 <- gsub("\\'","",data3)

data3
write(unlist(data3),"remake_2.txt") 
data4 <- read.table("remake_2.txt")
data4
nrow(data4) 
wordcount <- table(data4) 
wordcount
head(sort(wordcount, decreasing=T),20)
txt <- readLines("성형부작용gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt
for( i in 1:cnt_txt) {
  data3 <-gsub((txt[i]),"",data3)     
}
data3
data3 <- Filter(function(x) {nchar(x) >= 2} ,data3)
write(unlist(data3),"remake_2.txt") 
data4 <- read.table("remake_2.txt")
data4
nrow(data4) 
wordcount <- table(data4) 
wordcount
head(sort(wordcount, decreasing=T),20)
library(RColorBrewer) 
palete <- brewer.pal(9,"Set3") 
wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq=2,
          random.order=F,random.color=T,colors=palete)
legend(0.3,1 ,"성형수술 부작용 키워드 분석  ",cex=0.8,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")
