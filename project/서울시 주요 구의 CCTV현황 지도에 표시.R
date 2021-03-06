#[ 강북구 소스코드 ]

setwd("c:\\r_temp")
install.packages("ggmap")
library(ggmap)
gangbuk <- read.csv("project_gangbuk_data.csv", header=T)
g_m <- get_map("gangbukgu", zoom=13, maptype="roadmap")
gang.map <- ggmap(g_m) + geom_point(data=gangbuk, aes(x=LON, y=LAT), size=2, alpha=0.7, color="#980000")
gang.map

#[ 강북구 CCTV 현황 + 관할 지구대 범위 ]
g_m <- get_map("gangbukgu", zoom=13, maptype="roadmap")
gang.map <- ggmap(g_m) + geom_point(data=gangbuk, aes(x=LON,y=LAT), size=2, alpha=0.7, color="#980000") +
  geom_point(aes(x=127.0273396, y=37.6374119), size=30, alpha=0.1, color="#003399") +
  geom_point(aes(x=127.0481630, y=37.6236052), size = 30,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0192530, y=37.6353036), size = 30,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0182250, y=37.6248237), size = 45,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0170957, y=37.6409194), size = 45,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0326000, y=37.6359904), size = 60,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0303889, y=37.6163262), size = 60,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0283051, y=37.6201638), size = 75,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0266000, y=37.6443571), size = 75,alpha=0.1,color="#003399")
gang.map

#[ 강북 경찰서 범위 소스 코드 ]
g_m <- get_map(location=c(lon=127.0273396, lat=37.6374119), zoom=14, 
               maptype="roadmap")
gang.map <- ggmap(g_m) + geom_point(data=gangbuk, aes(x=LON, y=LAT), size=3, 
                                    alpha=0.7, color="#980000") +
  geom_point(aes(x=127.0273396, y=37.6374119), size=110, alpha=0.1, color="#003399")
gang.map

# [ 강북구 ? 번 3동 파출소 범위 ]
g_m <- get_map(location=c(lon=127.0481630, lat=37.6236052), zoom=14, 
               maptype="roadmap")
gang.map <- ggmap(g_m) + geom_point(data=gangbuk, aes(x=LON, y=LAT), size=3, alpha=0.7, color="#980000") +
  geom_point(aes(x=127.0481630, y=37.6236052), size=110, alpha=0.1, color="#003399")
gang.map

#[ 송파구 소스 코드 ]
songpa <- read.csv("project_songpa_data.csv", header=T)
#송파구_전체 CCTV
s_m <- get_map("songpagu", zoom=13, maptype="roadmap")
song.map <- ggmap(s_m) + geom_point(data=songpa, aes(x=LON, y=LAT), size=2, alpha=0.7, color="#980000")
song.map

# [ 송파구 CCTV 현황 + 경찰서 현황 ]
s_m <- get_map("songpagu", zoom=13, maptype="roadmap")
song.map <- ggmap(s_m) + geom_point(data=songpa, aes(x=LON, y=LAT), size=2, 
                                    alpha=0.7, color="#980000") +
  geom_point(aes(x=127.128335, y=37.5022377), size=30,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.112516, y=37.5201289), size=30,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.148227, y=37.496981), size=30,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.1465599, y=37.495096), size=30,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.1281358, y=37.506912), size=30,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.1167715, y=37.5287594), size=30,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.1296875, y=37.4852096), size=45,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0928051, y=37.5027089), size=60,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.1252841, y=37.4982471), size=60,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.0821432, y=37.5120908), size=75,alpha=0.1,color="#003399") +
  geom_point(aes(x=127.1257587, y=37.5102538), size=75,alpha=0.1,color="#003399")
song.map

# [ 송파 경찰서 소스 코드 ]
#1.송파구_송파 경찰서
s_m <- get_map(location=c(lon=127.128335, lat=37.5022377), zoom=14, 
               maptype="roadmap")
song.map <- ggmap(s_m) + geom_point(data=songpa, aes(x=LON, y=LAT), size=3, 
                                    alpha=0.7, color="#980000") +
  geom_point(aes(x=127.128335, y=37.5022377), size=110, alpha=0.1, color="#003399")
song.map

# [ 송파구 신천 파출소 현황 ]
#2.송파구_신천 파출소
s_m <- get_map(location=c(lon=127.112516, lat=37.5201289), zoom=14, 
               maptype="roadmap")
song.map <- ggmap(s_m) + geom_point(data=songpa, aes(x=LON, y=LAT), size=3, 
                                    alpha=0.7, color="#980000") +
  geom_point(aes(x=127.112516, y=37.5201289), size=110, alpha=0.1, color="#003399")
song.map

