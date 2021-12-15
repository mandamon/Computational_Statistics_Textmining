#연관어 분석 

# 한글 처리를 위한 패키지 설치
#install.packages('rJava')
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_151')
#library(rJava) # 아래와 같은 Error 발생 시 Sys.setenv()함수로 java 경로 지정
#install.packages("KoNLP") 
#library(KoNLP) # rJava 라이브러리가 필요함
library(stringr)

# (1) 데이터 가져오기
df <- read.csv("4차 대유행.csv", header=TRUE)
news <- df$text[!is.na(df$text)]
news_pre <- paste0(news, collapse = NULL)
news_pre <- str_replace_all(news_pre, pattern = "[^가-힣]", replacement = " ")

erase <- readLines("erase.txt", encoding = "UTF-8")
erase <- unlist(strsplit(as.character(erase), split=","))
erase
for(i in erase){
  news_pre <- gsub(i, "", news_pre)
}

# (2) 줄 단위 단어 추출
lword <- Map(extractNoun, news_pre) 
length(lword) # [1] key = 1차|1215, 2차|1477, 3차|2791, 4차|3822

lword <- unique(lword) # 중복제거1(전체 대상)
length(lword) # [1] 1차|1213 (2개 제거), 2차|1473 (4개 제거), 3차|2787 (4개 제거), 4차|3822 (동일)

# (3) 중복단어 제거와 추출 단어 확인
lword <- sapply(lword, unique) # 중복제거2 (줄 단위 대상) 
length(lword) # [1] 동일


# (4) 연관어 분석을 위한 전처리 

# 단어 필터링 함수 정의 (길이 2~4사이 한글 단어 추출)
filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}

filter2 <- function(x){
  Filter(filter1, x)
}

# 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)


# (5) 트랜잭션 생성

# 연관분석을 위한 패키지 설치
#install.packages("arules")
library(arules) 

# 트랜잭션 생성
wordtran <- as(lword, "transactions") # lword에 중복데이터가 있으면 error 발생
wordtran

# (6) 단어 간 연관규칙 산출 
# default (support지지도=0.1, confidence신뢰도=0.8, maxlen최대길이=10)
# 지지도와 신뢰도를 높이면 발견되는 규칙수가 줄어듦
tranrules_025_005 <- apriori(wordtran, parameter=list(supp=0.25, conf=0.05)) 
tranrules_025_08 <- apriori(wordtran, parameter=list(supp=0.25, conf=0.8)) 
tranrules_03_005 <- apriori(wordtran, parameter=list(supp=0.3, conf=0.05)) 
tranrules_03_05 <- apriori(wordtran, parameter=list(supp=0.3, conf=0.5))

# 연관규칙 생성 결과보기 
inspect(tranrules_025_005) # 연관규칙 생성 결과 보기
inspect(tranrules_025_08)
inspect(tranrules_03_005) # 2차, 3차, 4차
inspect(tranrules_03_05) # 1차

# (7)  연관어 시각화 

# 연관단어 시각화를 위해서 자료구조 변경
rules <- labels(tranrules_03_005, ruleSep=" ")
rules
class(rules)

# 문자열로 묶인 연관단어를 행렬구조 변경 
rules <- sapply(rules, strsplit, " ",  USE.NAMES=F) 
rules
class(rules) 

# 행 단위로 묶어서 matrix로 반환
rulemat <- do.call("rbind", rules)
rulemat
class(rulemat)

# 연관어 시각화를 위한 igraph 패키지 설치
#install.packages("igraph") # graph.edgelist(), plot.igraph(), closeness() 함수 제공
library(igraph)   

# edgelist보기 - 연관단어를 정점 형태의 목록 제공 
ruleg <- graph.edgelist(rulemat[c(24:51),], directed=F) # [1,]~[30,] "{}" 제외
ruleg

#  edgelist 시각화()
plot.igraph(ruleg,
            vertex.label=V(ruleg)$name,
            vertex.label.cex=1.0,
            vertex.label.color='black',
            vertex.size=30,
            vertex.color='green',
            vertex.frame.color='blue',
            vertex.dist=100)

