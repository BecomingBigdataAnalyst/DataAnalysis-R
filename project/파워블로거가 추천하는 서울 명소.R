setwd("c:\\r_temp") 
install.packages("KoNLP")
install.packages("wordcloud")
install.packages("stringr")

library(KoNLP)  
library(wordcloud)
library(stringr)
useSejongDic( ) 
mergeUserDic(data.frame(readLines("서울명소merge.txt"), "ncn"))
txt <- readLines("seoul_go.txt")
place <- sapply(txt,extractNoun,USE.NAMES=F)
head(place,10) 
head(unlist(place), 30)
c <- unlist(place) 
res <- str_replace_all(c, "[^[:alpha:]]", "") 
txt <- readLines("서울명소gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt
for( i in 1:cnt_txt) {
  res <-gsub((txt[i]),"",res)     
}
res2 <- Filter(function(x) {nchar(x) >= 2} ,res)
NROW(res2) 
write(res2,"seoul_go2.txt") 
res3 <- read.table("seoul_go2.txt") 
wordcount <- table(res3) 
head(sort(wordcount, decreasing=T),30)
library(RColorBrewer) 
palete <- brewer.pal(8,"Set2") 
wordcloud(names(wordcount),freq=wordcount,scale=c(3,1),rot.per=0.25,min.freq=5,
          random.order=F,random.color=T,colors=palete)
legend(0.3,1 ,"블로거 추천 서울 명소 분석   ",cex=0.8,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")

