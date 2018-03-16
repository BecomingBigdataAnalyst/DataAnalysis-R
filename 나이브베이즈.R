#나이브베이즈 

#iris 데이터 집합을 이용해서 베이즈 정리로 분류 방식
#데이터를 학습/평가 데이터로 분리 
install.packages('caret')
library(caret)                  #ggplot2, lattice 필요
idx<-createDataPartition(iris$Species,p=0.7,list=F)
#7:3 비율로 데이터를 학습/평가로 분리 

train<-iris[idx,]
test<-iris[-idx,]

table(train$Species)
table(test$Species)

#베이즈 이론으로 조건부 확률 계산후 적용
install.packages('e1071')
library(e1071)
result<-naiveBayes(train,train$Species,laplace=1)
pred<-predict(result,test,type='class')
table(pred,test$Species)

result
pred

#베이즈 이론 적용 예측값 평가 
confusionMatrix(pred,test$Species)
#영화마케팅 문제를 베이즈 정리로 해결
#영화관객의 성향을 설문조사로 정리 
#관객의 속성으로 영화 취향을 파악해보자

# 사전확률 : 20대,여,디자이너,NO,YES,로맨틱
# P(B):20대, 여, IT,미혼,애인없음
# P(A) : 공포 영화를 선택할 확률
# 주변확률 : P(B|A)
# 조건부확률 : P(A|B)

#P(B|A) = P(20대,여,IT,미혼,애인없음 | 공포 )=
#P(20대|공포)*P(여|공포)*P(IT|공포)*P(미혼|공포)*P(애인없음|공포)

movie<-read.csv('c:/JAVA/movie.csv',header=T)
nbmovie<-naiveBayes(movie[1:5], movie$장르, laplace=0)
head(nbmovie)

result<-predict(nbmovie,movie[1:5])
sum(movie$장르 != result)

result 

table(result,movie$장르)

confusionMatrix(result,movie$장르)  #혼돈행렬 출력


 #사내메일중 스팸메일 가려내기
#메일에 특정 키워드가 있는 경우 스팸으로 처리 

#사전확률
#P(B) : '특강'이라는 단어가 포함될 확률
#P(A) : 메일이 스팸일 확률
#주변확률 : P(B|A)
#조건부확률:P(A|B)

mail<-read.csv('c:/JAVA/spam.csv',header=T)
nbmail<-naiveBayes(mail[2:13],mail$메일종류,laplace=1)

nbmail

result2<-predict(nbmail,mail[2:13])
sum(mail$메일종류 != result2)  #비일치 항목수 출력
table(result2,mail$메일종류)

confusionMatrix(result2,mail$메일종류)
result2

#입사지원시 조건에 따른 합격여부 판별
#P(B):나이, 장래희망 유무, 인터뷰태도,고교성적 
#P(A):합격여부 확률
#주변확률 : P(B|A)
#조건부확률:P(A|B)

#테스트 데이터: 적음,없음,매우좋음,보통, ??? 

P(합격여부=합격) : 13/20
P(합격여부=불합격) : 7/20
P(나이=적음|합격):4/13
P(나이=적음|불합격) : 3/7
P(장래희망유무=없음|불합격) : 1/13
P(장래희망유무=없음|불합격) : 7/7
P(인터뷰태도=매우좋음|합격) : 3/13
P(인터뷰태도=매우좋음|불합격) : 1/7
P(고교성적=보통|합격) : 2/13
P(고교성적=보통|불합격) : 3/7

P(적음,없음,매우좋음,보통|합격)
(13/20) *( 4/13) * (1/13) * (3/13) * (2/13) #/P(?)
=(0.0005+0.0092)/0.0005

P(적음,없음,매우좋음,보통|불합격)
(7/20)*(3/7)*(7/7)*(1/7)*(3/7)    #/P(?)
=(0.0005+0.0092)/0.0092


P(나이=적음)
P(장래희망=없음)
P(태도=매우좋음)
P(고교성적=보통)






