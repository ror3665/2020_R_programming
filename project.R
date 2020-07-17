#init
#install.packages("XML")
#install.packages("KoNLP")
#install.packages("tm")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("wordcloud2")
#install.packages("topicmodels")
library(KoNLP)
library(tm)
library(stringr)
library(XML)
library('ggplot2')
library('wordcloud2')
library("topicmodels")

serviceURL <- "http://apis.data.go.kr/1262000/SafetyNewsList/"

operation <- "getCountrySafetyNewsList"

rows <- 100

pg <-1

ServiceKey <- "%2FyHfOQ8uNKvNAOMoEDGGYo5xTi6FwjIXa1Uj7gGicxdTvOk5xYxiSXVE9stQeiiGlZEs7Z%2FS2pr1ZSEK5DOu1Q%3D%3D"

url <- paste0(serviceURL, operation, paste0("?servicekey=", ServiceKey), paste0("&numOfRows=", rows), paste0("&pageNo=", pg))


xmlDocument <- xmlTreeParse(url, useInternalNodes = TRUE, encoding="UTF-8")

rootNode <- xmlRoot(xmlDocument)

numOfRows <- as.numeric(xpathSApply(rootNode, "//numOfRows", xmlValue))

totalCount <- as.numeric(xpathSApply(rootNode, "//totalCount", xmlValue))

loopCount <- round(totalCount /numOfRows, 0)

pagelist <- c(1:loopCount)

if (loopCount * numOfRows < totalCount) {
  loopCount <- loopCount +1
}

totalData <- data.frame()

for(i in 1:loopCount) {
  url <- paste0(serviceURL, operation, paste0("?servicekey=", ServiceKey), paste0("&numOfRows=", rows), paste0("&pageNo=", i))
  
  
  doc <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "UTF-8")
  
  rootNode <- xmlRoot(doc)
  
  xmlData <- xmlToDataFrame(nodes = getNodeSet(rootNode, '//item'))
  
  totalData <- rbind(totalData, xmlData)
}



#Delete CountryEnName, fileURL, ID, Date
totalData <- totalData[,-2]
totalData <- totalData[,-3]
totalData <- totalData[,-3]
totalData <- totalData[,-4]

#Delete duplicate rows
totalData <- unique(totalData)
View(totalData)


countries <- totalData[[2]]

#CountryName 데이터 처리 및 결과
#ggplot2를 이용한 각 국가의 안전소식 빈도
country.table <- table(unlist(countries))
country.table
country.data <- data.frame(country.table)
ggplot(data=country.data, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity")+coord_flip()+guides(fill=FALSE)+
  geom_hline(aes(yintercept=median(country.table)))+
  xlab("국가명")+
  ylab("빈도수")+
  scale_y_continuous(breaks = seq(0,100, by =1)) +
  theme(  
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10,face='bold'),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))


#국가별 코로나 관련 소식 빈도
Covid <- totalData[grep("코로나",totalData$content),]
Covid <- unique(Covid)
Count <- table(Covid$countryName)
Chart = sort(Count,decreasing = TRUE)

covid.data <- data.frame(Chart)
ggplot(data=covid.data, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity")+coord_flip()+guides(fill=FALSE)+
  geom_hline(aes(yintercept=median(Chart)))+
  xlab("국가명")+
  ylab("빈도수")+
  scale_y_continuous(breaks = seq(0,100, by =1)) +
  theme_bw()


#Content 데이터 처리 시작
contents <- totalData[[1]]

#중국과 코로나 두개의 단어를 가지고 있는 내용
install.packages('tidyverse')
library(tidyverse)
pattern <- c("코로나", "중국")
covid.china <- map(pattern, str_subset, string = contents) %>% reduce(intersect)
covid.china

#말뭉치 생성
content.corpus <- VCorpus(VectorSource(contents))
content.corpus

mycorpusfunc <- function(myobject,oldexp,newexp){
  newobject <- tm_map(myobject,
                      content_transformer(function(x,pattern){str_replace_all(x,pattern,newexp)}),
                      oldexp)
  newobject
}

mycorpus <- content.corpus 
#말뭉치 전처리 
mycorpus <- mycorpusfunc(mycorpus, "‘","")
mycorpus <- mycorpusfunc(mycorpus, "’","")
mycorpus <- mycorpusfunc(mycorpus, "“","")
mycorpus <- mycorpusfunc(mycorpus, "”","")
mycorpus <- mycorpusfunc(mycorpus, "’","")
mycorpus <- mycorpusfunc(mycorpus, '"',"")
mycorpus <- mycorpusfunc(mycorpus, "&nbsp;","")
mycorpus <- mycorpusfunc(mycorpus, "-","")
mycorpus <- mycorpusfunc(mycorpus, "\\+","")
mycorpus <- mycorpusfunc(mycorpus, "○","")
mycorpus <- mycorpusfunc(mycorpus, "①|②|③|④|⑤|⑥","")
mycorpus <- mycorpusfunc(mycorpus, "☞",".")
mycorpus <- mycorpusfunc(mycorpus, "[[:digit:]]{1,}\\.[[:digit:]]{1,}","")
mycorpus <- mycorpusfunc(mycorpus, "[[:punct:]]"," ")
mycorpus <- mycorpusfunc(mycorpus, "([[:upper:]]{1}|[[:lower:]]{1})[[:lower:]]{0,}","")
mycorpus <- tm_map(mycorpus, stripWhitespace)


myNounFun <- function(mytext){
  myNounList <- paste(extractNoun(mytext),collapse=' ')
  myNounList
}

#숫자 정리
myNounCorpus <- mycorpus 
for (i in 1:length(mycorpus)) {
  myNounCorpus[[i]]$content <- myNounFun(mycorpus[[i]]$content)
}

#눈에 보이는 것만 정리
myNounCorpus <- mycorpusfunc(myNounCorpus,"[[:digit:]]{1,}\\,{0,1}[[:digit:]]{0,}","")
myNounCorpus <- mycorpusfunc(myNounCorpus,"러시아[[:alpha:]]{1,}","러시아")
myNounCorpus <- mycorpusfunc(myNounCorpus,"레바논[[:alpha:]]{1,}","레바논")
myNounCorpus <- mycorpusfunc(myNounCorpus,"독일[[:alpha:]]{1,}","독일")
myNounCorpus <- mycorpusfunc(myNounCorpus,"강탈[[:alpha:]]{1,}","강탈")
myNounCorpus <- mycorpusfunc(myNounCorpus,"개인[[:alpha:]]{1,}","개인")
myNounCorpus <- mycorpusfunc(myNounCorpus,"결제[[:alpha:]]{1,}","결제")
myNounCorpus <- mycorpusfunc(myNounCorpus,"대피[[:alpha:]]{1,}","대피")
myNounCorpus <- mycorpusfunc(myNounCorpus,"국민[[:alpha:]]{1,}","국민")
myNounCorpus <- mycorpusfunc(myNounCorpus,"뎅기열[[:alpha:]]{1,}","뎅기열")
myNounCorpus <- mycorpusfunc(myNounCorpus,"국영[[:alpha:]]{1,}","국영")
myNounCorpus <- mycorpusfunc(myNounCorpus,"동물[[:alpha:]]{1,}","동물")
myNounCorpus <- mycorpusfunc(myNounCorpus,"도움[[:alpha:]]{1,}","도움")
myNounCorpus <- mycorpusfunc(myNounCorpus,"롬바르디아주[[:alpha:]]{1,}","렘바르디아주")
myNounCorpus <- mycorpusfunc(myNounCorpus,"레벨단계[[:alpha:]]{1,}","레벨단계")
myNounCorpus <- mycorpusfunc(myNounCorpus,"네덜란드[[:alpha:]]{1,}","네덜란드")
myNounCorpus <- mycorpusfunc(myNounCorpus,"공격[[:alpha:]]{1,}","공격")
myNounCorpus <- mycorpusfunc(myNounCorpus,"후베이성[[:alpha:]]{1,}","후베이성")
myNounCorpus <- mycorpusfunc(myNounCorpus,"확진[[:alpha:]]{1,}","확진")
myNounCorpus <- mycorpusfunc(myNounCorpus,"호치민시[[:alpha:]]{1,}","호치민시")
myNounCorpus <- mycorpusfunc(myNounCorpus,"현황[[:alpha:]]{1,}","현황")
myNounCorpus <- mycorpusfunc(myNounCorpus,"현금[[:alpha:]]{1,}","현금")
myNounCorpus <- mycorpusfunc(myNounCorpus,"해외여행[[:alpha:]]{1,}","해외여행")
myNounCorpus <- mycorpusfunc(myNounCorpus,"한국[[:alpha:]]{1,}","한국")
myNounCorpus <- mycorpusfunc(myNounCorpus,"하와이[[:alpha:]]{1,}","하와이")
myNounCorpus <- mycorpusfunc(myNounCorpus,"필리핀[[:alpha:]]{1,}","필리핀")
myNounCorpus <- mycorpusfunc(myNounCorpus,"프랑크푸르트[[:alpha:]]{1,}","프랑크푸르트")
myNounCorpus <- mycorpusfunc(myNounCorpus,"폭탄[[:alpha:]]{1,}","폭탄")
myNounCorpus <- mycorpusfunc(myNounCorpus,"파푸아뉴기니[[:alpha:]]{1,}","파푸아뉴기니")
myNounCorpus <- mycorpusfunc(myNounCorpus,"코로나[[:alpha:]]{1,}","코로나")
myNounCorpus <- mycorpusfunc(myNounCorpus,"일본[[:alpha:]]{1,}","일본")
myNounCorpus <- mycorpusfunc(myNounCorpus,"여러분[[:alpha:]]{1,}","여러분")
words_nouns <- lapply(myNounCorpus,
                      function(x){str_extract_all(x$content,boundary("word"))}
)

dtm.content <- DocumentTermMatrix(myNounCorpus)
dtm.content

#Content 말뭉치 분석
word.freq <- apply(dtm.content[,],2,sum)

#단어와 총 등장 수
length(word.freq); sum(word.freq)

#상위 15개의 단어
sort.word.freq <- sort(word.freq,decreasing=TRUE)
head(sort.word.freq,15)

#말구름 생성
WC_data <- data.frame(names(sort.word.freq),sort.word.freq)
names(WC_data) <- c('word','freq')
wordcloud2(WC_data[WC_data$freq>9,],
           size=0.7, 
           shuffle=FALSE,  
           rotateRatio=0.3,
           shape="circle")

#코로나와 .40이상의 상관관계 크기를 갖는 단어
findAssocs(dtm.content,"코로나",0.40)

#상관계수 분석
my.assoc.func <- function(mydtm,term1,term2){
  myvar1 <- as.vector(mydtm[,term1])
  myvar2 <- as.vector(mydtm[,term2])
  cor.test(myvar1,myvar2)
}

length.doc <- length(rownames(dtm.content))
my.doc.cor <- matrix(NA,nrow=length.doc,ncol=length.doc)
for (i in 1:length.doc) {
  for (j in 1:length.doc) {
    my.doc.cor[i,j] <- my.assoc.func(t(dtm.content),rownames(dtm.content)[i],rownames(dtm.content)[j])$est
  }
}
rownames(my.doc.cor) <- colnames(my.doc.cor) <- rownames(dtm.content)
diag(my.doc.cor) <- NA 

df_cor <- data.frame(my.doc.cor[lower.tri(my.doc.cor)])
names(df_cor) <- 'cor_coef'
ggplot(df_cor, aes(x=cor_coef))+
  geom_histogram(bins=40)+
  labs(title="안전소식간 상관계수 분포",
       x="상관계수", y="빈도")+
  theme_classic()
summary(df_cor)

#연관 규칙 분석
#install.packages("arules")
library("arules")
words_appear <- rep(NA,length(myNounCorpus))
for (i in 1:length(myNounCorpus)){
  my_appear_words <- names(table(unlist(str_split(myNounCorpus[[i]]$content," "))))
  words_appear[i] <- str_c(my_appear_words,collapse=" ")
}
data_AR <- str_split(words_appear," ",length(colnames(dtm.content)))
data_AR <- as(data_AR, "transactions")
lift.analysis <- apriori(data_AR, parameter=list(support=0.20,confidence=0.50))

#향상도 순서대로
inspect(head(sort(lift.analysis, by="lift"),10)) 

#'코로나' 연관 규칙 분석 
analysis.covid <- apriori(data=data_AR,
                          parameter=list(support=0.10,confidence=0.50),
                          appearance=list(rhs='코로나'))
inspect(analysis.covid)

#inspect초기화
inspect <-tm::inspect
