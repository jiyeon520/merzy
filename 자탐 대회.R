getwd()
setwd("C:\\Users\\User\\Documents\\카카오톡 받은 파일")
getwd()
drama <- readxl::read_xlsx("Drama (2).xlsx")

install.packages("dplyr")
library(dplyr)

sbs <- drama[drama$company=="SBS",]
kbs <- drama[drama$company=="KBS",]
mbc <- drama[drama$company=="MBC",]
tvn <- drama[drama$company=="tvN",]
ocn <- drama[drama$company=="OCN",]
jtbc <- drama[drama$company=="JTBC",]

summary(sbs$`ost 대표곡 좋아요`, na.rm=TRUE)
summary(sbs$`ost 유투브 조회수`, na.rm=TRUE)
summary(sbs$최고시청률, na.rm=TRUE)
boxplot(sbs$최고시청률, na.rm=TRUE)
summary(sbs$`명장면 조회수`, na.rm=TRUE)
summary(sbs$`도깨비 기준(100) 검색량`, na.rm=TRUE)
table(sbs$시간대)

summary(kbs$`ost 대표곡 좋아요`, na.rm=TRUE)
summary(kbs$`ost 유투브 조회수`, na.rm=TRUE)
summary(kbs$최고시청률, na.rm=TRUE)
boxplot(kbs$최고시청률, na.rm=TRUE)
summary(kbs$`명장면 조회수`, na.rm=TRUE)
summary(kbs$`도깨비 기준(100) 검색량`, na.rm=TRUE)
table(kbs$시간대)

summary(mbc$`ost 대표곡 좋아요`, na.rm=TRUE)
summary(mbc$`ost 유투브 조회수`, na.rm=TRUE)
summary(mbc$최고시청률, na.rm=TRUE)
boxplot(mbc$최고시청률, na.rm= TRUE)
summary(mbc$`명장면 조회수`, na.rm=TRUE)
summary(mbc$`도깨비 기준(100) 검색량`, na.rm=TRUE)
table(mbc$시간대)

summary(tvn$`ost 대표곡 좋아요`, na.rm=TRUE)
summary(tvn$`ost 유투브 조회수`, na.rm=TRUE)
summary(tvn$최고시청률, na.rm=TRUE)
boxplot(tvn$최고시청률, na.rm=TRUE)
summary(tvn$`명장면 조회수`, na.rm=TRUE)
summary(tvn$`도깨비 기준(100) 검색량`, na.rm=TRUE)
table(tvn$시간대)

summary(jtbc$`ost 대표곡 좋아요`, na.rm=TRUE)
summary(jtbc$`ost 유투브 조회수`, na.rm=TRUE)
summary(jtbc$최고시청률, na.rm=TRUE)
boxplot(jtbc$최고시청률, na.rm=TRUE)
which(jtbc$최고시청률>=12)
jtbc$name # 26, 32 이상치 존재 품위있는 그녀, 스카이캐슬
rownames(jtbc$name)
summary(jtbc$`명장면 조회수`, na.rm=TRUE)
summary(jtbc$`도깨비 기준(100) 검색량`, na.rm=TRUE)
table(jtbc$시간대)

summary(ocn$`ost 대표곡 좋아요`, na.rm=TRUE)
summary(ocn$`ost 유투브 조회수`, na.rm=TRUE)
summary(ocn$최고시청률, na.rm=TRUE)
boxplot(ocn$최고시청률, na.rm= TRUE)
summary(ocn$`명장면 조회수`, na.rm=TRUE)
summary(ocn$`도깨비 기준(100) 검색량`, na.rm=TRUE)
table(ocn$시간대)

mean(sbs$`ost 대표곡 좋아요`, na.rm=TRUE)
boxplot(sbs$`ost 대표곡 좋아요`, na.rm=TRUE)
which(sbs$`ost 대표곡 좋아요`>=100000)
sbs$name #별에서 온 그대, 괜찮아 사랑이야, 달의연인 보보경심려, 키스먼저할까요 이상치

mean(kbs$`ost 대표곡 좋아요`, na.rm=TRUE)
boxplot(kbs$`ost 대표곡 좋아요`, na.rm=TRUE)
which(kbs$`ost 대표곡 좋아요`>=40000)
kbs$name #착한남자, 빅, 비밀, 연애의발견, 프로듀사, 태양의후예, 쌈마이웨이

mean(mbc$`ost 대표곡 좋아요`, na.rm=TRUE)
boxplot(mbc$`ost 대표곡 좋아요`, na.rm=TRUE)
which(mbc$`ost 대표곡 좋아요`>=40000) 
mbc$name #해를품은달, 구가의서, 그녀는 예뻤다, 킬미 힐미, 돈꽃, 군주

mean(tvn$`ost 대표곡 좋아요`, na.rm=TRUE)
boxplot(tvn$`ost 대표곡 좋아요`, na.rm=TRUE)
which(tvn$`ost 대표곡 좋아요`>=100000) 
tvn$name #응답하라 1997, 응답하라 1988, 도깨비, 또 오해영

mean(jtbc$`ost 대표곡 좋아요`, na.rm=TRUE)
boxplot(jtbc$`ost 대표곡 좋아요`, na.rm=TRUE)
which(jtbc$`ost 대표곡 좋아요`>=18000) 
jtbc$name # 이번주 아내가 바람을 핍니다, 힘쎈여자 도봉순, 스카이 캐슬, 밥 잘사주는 예쁜 누나, 라이프, 뷰티 인사이드

mean(ocn$`ost 대표곡 좋아요`, na.rm=TRUE)
boxplot(ocn$`ost 대표곡 좋아요`, na.rm=TRUE)
which(ocn$`ost 대표곡 좋아요`>=6000) 
ocn$name # 아름다운 나의 신부, 38 사기동대, 보이스

mean(sbs$`ost 유투브 조회수`, na.rm=TRUE)
boxplot(sbs$`ost 유투브 조회수`, na.rm=TRUE)
which(sbs$`ost 유투브 조회수`>=1e+6) #어떻게함
sbs$name # 별에서 온 그대, 주군의 태양, 괜찮아 사랑이야, 닥터스, 푸른바다의 전설, 달의연인 보보경심려, 키스먼저할까요?

mean(kbs$`ost 유투브 조회수`, na.rm=TRUE)
boxplot(kbs$`ost 유투브 조회수`, na.rm=TRUE)
which(kbs$`ost 유투브 조회수`>=10^6.5) 
kbs$name # 각시탈, 빅, 드림하이2, 연애의 발견, 태양의 후예, 고백부부, 쌈마이웨이

mean(mbc$`ost 유투브 조회수`, na.rm=TRUE)
boxplot(mbc$`ost 유투브 조회수`, na.rm=TRUE)
which(mbc$`ost 유투브 조회수`>=5^9.7) 
mbc$name # 해를 품은달, 기황후, 구가의서, 그녀는 예뻤다, 돈꽃, 군주 가면의 주인


mean(tvn$`ost 유투브 조회수`, na.rm=TRUE)
boxplot(tvn$`ost 유투브 조회수`, na.rm=TRUE)
which(tvn$`ost 유투브 조회수`>=10^7.1) 
tvn$name #응답하라 1997, 응답하라 1988, 도깨비, 또 오해영


mean(jtbc$`ost 유투브 조회수`, na.rm=TRUE)
boxplot(jtbc$`ost 유투브 조회수`, na.rm=TRUE)
which(jtbc$`ost 유투브 조회수`>=7^7) 
jtbc$name #이번주 아내가 바람을 핍니다, 힘쎈여자 도봉순, 스카이캐슬, 미스티, 내 아이디는 강남미인, 뷰티 인사이드

mean(ocn$`ost 유투브 조회수`, na.rm=TRUE)
boxplot(ocn$`ost 유투브 조회수`, na.rm=TRUE)
which(ocn$`ost 유투브 조회수`>=600000) 
ocn$name #보이스, 블랙

mean(sbs$`명장면 조회수`, na.rm=TRUE)
boxplot(sbs$`명장면 조회수`, na.rm=TRUE)
which(sbs$`명장면 조회수`>=10^6.63)
sbs$name #상속자들, 장옥정 사랑에 살다, 괜찮아 사랑이야

mean(kbs$`명장면 조회수`, na.rm=TRUE)
boxplot(kbs$`명장면 조회수`, na.rm=TRUE)
which(kbs$`명장면 조회수`>=2.0e+06)
kbs$name #프로듀사, 태양의 후예, 아버지가 이상해, 쌈마이웨이, 하나뿐인 내편

mean(mbc$`명장면 조회수`, na.rm=TRUE)
boxplot(mbc$`명장면 조회수`, na.rm=TRUE)
which(mbc$`명장면 조회수`>=10^6.2)
mbc$name # 해를 품은 달, 기황후, 그녀는 예뻤다, 불어라 미풍아


mean(tvn$`명장면 조회수`, na.rm=TRUE)
boxplot(tvn$`명장면 조회수`, na.rm=TRUE)
which(tvn$`명장면 조회수`>=10^6.8)
tvn$name #응답하라 1997, 로맨스가 필요해 2012

mean(ocn$`명장면 조회수`, na.rm=TRUE)
boxplot(ocn$`명장면 조회수`, na.rm=TRUE)
which(ocn$`명장면 조회수`>=10^5.6)
ocn$name # 나쁜 녀석들, 리셋, 신의 퀴즈 시즌4, 처용

library(ppcor)
broad <- data.frame(sbs, kbs, mbc, tvn, jtbc, ocn)
cor(sbs$`ost 대표곡 좋아요`, sbs$`ost 유투브 조회수`, use="complete.obs")
cor(sbs$`ost 대표곡 좋아요`, sbs$`명장면 조회수`, use="complete.obs")
cor(sbs$`ost 유투브 조회수`, sbs$`명장면 조회수`, use="complete.obs")

cor(kbs$`ost 대표곡 좋아요`, kbs$`ost 유투브 조회수`, use="complete.obs")
cor(kbs$`ost 대표곡 좋아요`, kbs$`명장면 조회수`, use="complete.obs")
cor(kbs$`ost 유투브 조회수`, kbs$`명장면 조회수`, use="complete.obs")

cor(mbc$`ost 대표곡 좋아요`, mbc$`ost 유투브 조회수`, use="complete.obs")
cor(mbc$`ost 대표곡 좋아요`, mbc$`명장면 조회수`, use="complete.obs")
cor(mbc$`ost 유투브 조회수`, mbc$`명장면 조회수`, use="complete.obs")

cor(tvn$`ost 대표곡 좋아요`, tvn$`ost 유투브 조회수`, use="complete.obs")
cor(tvn$`ost 대표곡 좋아요`, tvn$`명장면 조회수`, use="complete.obs")
cor(tvn$`ost 유투브 조회수`, tvn$`명장면 조회수`, use="complete.obs")

cor(jtbc$`ost 대표곡 좋아요`, jtbc$`ost 유투브 조회수`, use="complete.obs")
cor(jtbc$`ost 대표곡 좋아요`, jtbc$`명장면 조회수`, use="complete.obs")
cor(jtbc$`ost 유투브 조회수`, jtbc$`명장면 조회수`, use="complete.obs")

cor(ocn$`ost 대표곡 좋아요`, ocn$`ost 유투브 조회수`, use="complete.obs")
cor(ocn$`ost 대표곡 좋아요`, ocn$`명장면 조회수`, use="complete.obs")
cor(ocn$`ost 유투브 조회수`, ocn$`명장면 조회수`, use="complete.obs")

cor(sbs$최고시청률, sbs$`ost 대표곡 좋아요`, use="complete.obs")
cor(sbs$최고시청률, sbs$`ost 유투브 조회수`, use="complete.obs")
cor(sbs$최고시청률, sbs$`명장면 조회수`, use="complete.obs")

morn <- drama[drama$시간대=="아침",]
mn <- drama[drama$시간대=="저녁",]
night <- drama[drama$시간대=="밤",]
ln <- drama[drama$시간대=="자정",]

summary(morn$최고시청률, na.rm=TRUE)
summary(mn$최고시청률, na.rm=TRUE)
summary(night$최고시청률, na.rm=TRUE)
summary(ln$최고시청률, na.rm=TRUE)

mean(morn$`ost 대표곡 좋아요`, na.rm=T)
mean(morn$`ost 유투브 조회수`, na.rm=T)
mean(morn$`명장면 조회수`, na.rm=T)

mean(mn$`ost 대표곡 좋아요`, na.rm=T)
mean(mn$`ost 유투브 조회수`, na.rm=T)
mean(mn$`명장면 조회수`, na.rm=T)

mean(night$`ost 대표곡 좋아요`, na.rm=T)
mean(night$`ost 유투브 조회수`, na.rm=T)
mean(night$`명장면 조회수`, na.rm=T)

mean(ln$`ost 대표곡 좋아요`, na.rm=T)
mean(ln$`ost 유투브 조회수`, na.rm=T)
mean(ln$`명장면 조회수`, na.rm=T)

t.test(morn$최고시청률,mn$최고시청률)
t.test(morn$최고시청률,night$최고시청률)
t.test(morn$최고시청률,ln$최고시청률)
t.test(mn$최고시청률,night$최고시청률)
t.test(mn$최고시청률,ln$최고시청률)
t.test(night$최고시청률,ln$최고시청률)

t.test(morn$`ost 대표곡 좋아요`,mn$`ost 대표곡 좋아요`)
t.test(morn$`ost 대표곡 좋아요`,night$`ost 대표곡 좋아요`)
t.test(morn$`ost 대표곡 좋아요`,ln$`ost 대표곡 좋아요`)
t.test(mn$`ost 대표곡 좋아요`,night$`ost 대표곡 좋아요`)
t.test(mn$`ost 대표곡 좋아요`,ln$`ost 대표곡 좋아요`)
t.test(night$`ost 대표곡 좋아요`,ln$`ost 대표곡 좋아요`)

t.test(morn$`ost 유투브 조회수`,mn$`ost 유투브 조회수`)
t.test(morn$`ost 유투브 조회수`,night$`ost 유투브 조회수`)
t.test(morn$`ost 유투브 조회수`,ln$`ost 유투브 조회수`)
t.test(mn$`ost 유투브 조회수`,night$`ost 유투브 조회수`)
t.test(mn$`ost 유투브 조회수`,ln$`ost 유투브 조회수`)
t.test(night$`ost 유투브 조회수`,ln$`ost 유투브 조회수`)

t.test(morn$`명장면 조회수`,mn$`명장면 조회수`)
t.test(morn$`명장면 조회수`,night$`명장면 조회수`)
t.test(morn$`명장면 조회수`,ln$`명장면 조회수`)
t.test(mn$`명장면 조회수`,night$`명장면 조회수`)
t.test(mn$`명장면 조회수`,ln$`명장면 조회수`)
t.test(night$`명장면 조회수`,ln$`명장면 조회수`)


tot <- drama[drama$종류=="종합편성",]
cab <- drama[drama$종류=="케이블",]
gru <- drama[drama$종류=="지상파",]

summary(tot$최고시청률, na.rm=TRUE)
summary(cab$최고시청률, na.rm=TRUE)
summary(gru$최고시청률, na.rm=TRUE)

mean(tot$`ost 대표곡 좋아요`, na.rm=T)
mean(tot$`ost 유투브 조회수`, na.rm=T)
mean(tot$`명장면 조회수`, na.rm=T)

mean(cab$`ost 대표곡 좋아요`, na.rm=T)
mean(cab$`ost 유투브 조회수`, na.rm=T)
mean(cab$`명장면 조회수`, na.rm=T)

mean(gru$`ost 대표곡 좋아요`, na.rm=T)
mean(gru$`ost 유투브 조회수`, na.rm=T)
mean(gru$`명장면 조회수`, na.rm=T)

t.test(tot$최고시청률,cab$최고시청률)
t.test(tot$최고시청률,gru$최고시청률)
t.test(cab$최고시청률,gru$최고시청률)


t.test(tot$`ost 대표곡 좋아요`,cab$`ost 대표곡 좋아요`)
t.test(tot$`ost 대표곡 좋아요`,gru$`ost 대표곡 좋아요`)
t.test(cab$`ost 대표곡 좋아요`,gru$`ost 대표곡 좋아요`)

t.test(tot$`ost 유투브 조회수`,cab$`ost 유투브 조회수`)
t.test(tot$`ost 유투브 조회수`,gru$`ost 유투브 조회수`)
t.test(cab$`ost 유투브 조회수`,gru$`ost 유투브 조회수`)


t.test(tot$`명장면 조회수`,cab$`명장면 조회수`)
t.test(tot$`명장면 조회수`,gru$`명장면 조회수`)
t.test(cab$`명장면 조회수`,gru$`명장면 조회수`)









barplot(morn$최고시청률)
axis(1,at=1:6,lab=c())

tot <- drama[drama$종류=="종합편성",]
cab <- drama[drama$종류=="케이블",]
gru <- drama[drama$종류=="지상파",]

a <- tot$최고시청률
b <- cab$최고시청률
c <- gru$최고시청률

a1 <- tot$`명장면 조회수`&tot$`ost 유투브 조회수`
b2 <- cab$`명장면 조회수`&tot$`ost 유투브 조회수`
c1 <- gru$`명장면 조회수`&tot$`ost 유투브 조회수`

t.test(a1, c1)








plot(a)
plot(b)
plot(c)
hist(a)
hist(b)
hist(c)

t.test(a,b)
t.test(a,c)
t.test(b,c)
?t.test
?plot


a <- is.na(mbc)*1
is.na(sbs)*1
is.na(kbs)*1
is.na(tvn)*1
e <- is.na(ocn)*1
sum(e)
