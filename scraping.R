library(rvest)
library(stringr)
# 1차 2020.02.18 ~ 2020.03.18 1240 | 2차 2020.08.15 ~ 2020.09.15 1520 | 3차 2020.12.01 ~ 2021.01.31 2880 | 4차 2021.07.01 ~ 2021.08.31 3880
# query = 검색어
# sort = 정렬 | 관련도순, 최신순, 오래된순
# photo = 유형 | 전체, 포토, 동영상, 지면기사, 보도자료, 자동생성기사
# ds = 시작날짜, de = 끝 날짜
# start = 기사 번호, 페이지 넘어갈 때마다 10씩
#언론사 .info_group > a.info.press
#날짜 .t11
#기사 링크 .info_group > a:nth-child(4)
#기사제목 #articleTitle
#기사본문 #articleBodyContents
url <- c("https://search.naver.com/search.naver?where=news&query=대유행&sm=tab_pge&sort=2&photo=3&pd=3&ds=2020.02.18&de=2020.03.18&start=",
        "https://search.naver.com/search.naver?where=news&query=대유행&sm=tab_pge&sort=2&photo=3&pd=3&ds=2020.08.15&de=2020.09.15&start=",
        "https://search.naver.com/search.naver?where=news&query=대유행&sm=tab_pge&sort=2&photo=3&pd=3&ds=2020.12.01&de=2021.01.31&start=",
        "https://search.naver.com/search.naver?where=news&query=대유행&sm=tab_pge&sort=2&photo=3&pd=3&ds=2021.07.01&de=2021.08.31&start=")
page_count <- c(124, 152, 288, 388)
news_urls = c() # 뉴스 url

# 뉴스 url, 언론사 scraping
for(j in 0:page_count[4]){
  tmp_url <- paste0(url[4], j*10)
  html <- read_html(tmp_url)
  node <- html_nodes(html, ".info_group")
  
  # 뉴스 url
  node_url <- html_nodes(html, ".info_group")%>%html_nodes(".info")%>%html_attr('href')
  node_url <- node_url[!is.na(node_url)]
  node_url <- node_url[grep("https://news.naver.com", node_url)]
  news_urls <- c(news_urls, node_url)
}

# 뉴스 데이터프레임 만들기
  news <- data.frame(
  url = news_urls
)

# 뉴스 url으로 들어가 뉴스 일자, 헤드라인, 본문 스크래핑
news$date <- NA
news$press <- NA
news$headline <- NA
news$text <- NA

for(i in rownames(news)) {
  html <- read_html(news[i, "url"])
  # 뉴스 일자
  node_date <- substr(html_text(html_node(html, ".t11")), 1, 10)
  # 뉴스 언론사
  node_press <- html_node(html, ".press_logo")%>%html_node("img")%>%html_attr("title")
  # 뉴스 헤드라인
  node_headline <- html_text(html_node(html, "#articleTitle"))
  # 뉴스 본문
  node_text <- html_text(html_node(html, "#articleBodyContents"))
  if((length(node_headline) < 0) | (length(node_text) < 0)) next
  news[i, "date"] <- node_date
  news[i, "press"] <- node_press
  news[i,"headline"] <- node_headline
  news[i,"text"] <- node_text
}

# 데이터 저장
write.csv(news, "4차 대유행.csv")
