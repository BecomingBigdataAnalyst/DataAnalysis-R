setwd("c://r_temp")
install.packages("KoNLP")
install.packages("wordcloud")
library(KoNLP)
library(wordcloud)
useSejongDic()
mergeUserDic(data.frame("서진수","ncn"))
data1<-readLines("remake.txt")
data1
data2<-sapply(data1,extractNoun,USE.NAMES = F)
data2
data3<-unlist(data2)
data3<-Filter(function(x){nchar(x)<=10},data3)
head(unlist(data3),30)
data3<-gsub("\\d+","",data3)
data3 <- gsub("쌍수","쌍꺼풀",data3)
data3 <- gsub("쌍커풀","쌍꺼풀",data3)
data3 <- gsub("메부리코","매부리코",data3)
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
head(sort(wordcount, decreasing=T),20) # 가장 많이 언급된 상위 20개만 확인합니다
txt <- readLines("성형gsub.txt") # 제거하고 싶은 단어 목록을 불러옵니다
txt
cnt_txt <- length(txt)
cnt_txt
i <- 1
for( i in 1:cnt_txt) {
  data3 <-gsub((txt[i]),"",data3)     
}
data3  # ?-- 제거되어 있을 거예요~
data3 <- Filter(function(x) {nchar(x) >= 2} ,data3)
write(unlist(data3),"remake_2.txt") 
data4 <- read.table("remake_2.txt")
data4
nrow(data4) 
wordcount <- table(data4) 
wordcount
head(sort(wordcount, decreasing=T),30)

library(RColorBrewer) 
palete <- brewer.pal(9,"Set3") 
wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq=2,
          random.order=F,random.color=T,colors=palete)
legend(0.3,1 ,"여고생들이 선호하는 성형수술 부위",cex=0.8,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")

