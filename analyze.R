install.packages("rJava")
install.packages("NLP")
install.packages("wordcloud")
install.packages("slam")
install.packages("Sejong")
install.packages("hash")
install.packages("tau")
install.packages("XML")
install.packages("cli")

Sys.setenv(JAVA_HOME="C:/Program Files/JAVA/jre1.8.0_301")

library(stringr)
library(rJava)
library(slam)
library(RSQLite)
library(httr)
library(XML)
library(KoNLP)
library(tm)
library(wordcloud)

# 데이터 불러오기
df <- read.csv("1차 대유행.csv", header=TRUE)
news <- df$text[!is.na(df$text)]
news_pre <- paste0(news, collapse = NULL)

# 1차 전처리
news_pre <- str_replace_all(news_pre, pattern = "[^가-힣]", replacement = " ")

# 명사 추출
news_noun <- extractNoun(news_pre)

# 2차 전처리
erase <- readLines("erase.txt", encoding = "UTF-8")
erase <- unlist(strsplit(as.character(erase), split=","))

for(i in erase){
  news_noun <- gsub(i, "", news_noun)
}

# 말뭉치 생성
newsCorpus <- Corpus(VectorSource(news_noun))
TDM <- TermDocumentMatrix(newsCorpus, control=list(wordLengths=c(4,16)))
TDM

tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)
tdm.df

wordResult <- sort(rowSums(tdm.df),decreasing =T)
#wordResult <- wordResult[c(1:100)]
wordResult

myNames <- names(wordResult)
df <- data.frame(word=myNames, freq=wordResult)
df
pal <- brewer.pal(11,"RdYlBu")
wordcloud(df$word, df$freq, min.freq=2, random.order=F, scale=c(3,0.7), 
          rot.per=0, colors=pal, family="malgun")

windowsFonts(A=windowsFont("serif"))

