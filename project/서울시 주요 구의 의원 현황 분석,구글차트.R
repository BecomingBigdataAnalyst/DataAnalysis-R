#서울시 주요 구의 의원 현황 분석하기
setwd("c:\\r_temp")
install.packages("KoNLP")
library(KoNLP)
data1 <- read.csv("2013년_서울_주요구별_병원현황.csv" , header=T)
data1

barplot(as.matrix(data1[1:9,2:11]),
        main=paste("서울시 주요 구별 과목별 병원현황-2013년","\n",
                   "출처[국민건강보험공단]") , ylab="병원수", beside=T ,
        col=rainbow(8),ylim=c(0,350))
abline(h=seq(0,350,10),lty=3,lwd=0.2)
name <- data1$표시과목
name
legend(75,350,name,cex=0.8,fill=rainbow(8),bg="white")
savePlot("hospital.png",type="png")


#####서울시 주요 구의 의원 현황 한꺼번에 보기
v1 <- data1[1:9,2]*0.1 # 강남구 값 
v2 <- data1[1:9,3]*0.1 # 강동구 값
v3 <- data1[1:9,4]*0.1 # 강서구 값
v4 <- data1[1:9,5]*0.1 # 관악구 값
v5 <- data1[1:9,6]*0.1 # 구로구 값
v6 <- data1[1:9,7]*0.1 # 도봉구 값
v7 <- data1[1:9,8]*0.1 # 동대문구 값
v8 <- data1[1:9,9]*0.1 # 동작구 값
v9 <- data1[1:9,10]*0.1 # 마포구 값
v10 <- data1[1:9,11]*0.1 # 서대문구 값

par(mfrow=c(2,5))  # 2행 5열 로 그래프 배치하기
name <- data1$표시과목
name

# 강남구 그래프 그리기
gangnam <- barplot(as.matrix(v1),main="강남구 병원현황",
                   beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
                   ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

# 강동구 그래프 그리기
gangdong <- barplot(as.matrix(v2),main="강동구 병원현황",
                    beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
                    ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

# 강서구 그래프 그리기
gangseo <- barplot(as.matrix(v3),main="강서구 병원현황",
                   beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.8,las=2,
                   ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

# 관악구 그래프 그리기
gwanak <- barplot(as.matrix(v4),main="관악구 병원현황",
                  beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
                  ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)


# 구로구 그래프 그리기
guro<- barplot(as.matrix(v5),main="구로구 병원현황",
               beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
               ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

# 도봉구 그래프 그리기
dobong <- barplot(as.matrix(v6),main="도봉구 병원현황",
                  beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
                  ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

# 동대문구 그래프 그리기
dongademun <- barplot(as.matrix(v7),main="동대문구 병원현황",
                      beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
                      ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

# 동작구 그래프 그리기
dongjak <- barplot(as.matrix(v8),main="동작구 병원현황",
                   beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
                   ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

# 마포구 그래프 그리기
mapo <- barplot(as.matrix(v9),main="마포구 병원현황",
                beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
                ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

# 서대문구 그래프 그리기
seodaemun <- barplot(as.matrix(v10),main="서대문구 병원현황",
                     beside=T,axes=F,ylab="병원수(단위:10개)",xlab="", cex.names=0.85,las=2,
                     ylim=c(0,40), col=rainbow(8), border="white",names.arg=name)

axis(2,ylim=seq(0,35,10))
abline(h=seq(0,35,5),lty=2)

savePlot("hospital2.png",type="png")


#서울시 주요 구의 의원 현황을 구글 차트로 보여주기
install.packages("googleVis")
library(googleVis) 
data <- read.csv("2013년_서울_구별_주요과목별병원현황_구글용.csv",header=T)
data
hos <- gvisColumnChart(data,options=list(title="지역별 병원현황",height=400,weight=500))
plot(hos)

