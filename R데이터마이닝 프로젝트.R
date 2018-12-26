library(stringr)
library(readr)
library(tm)
library(KoNLP)
library(doSNOW)
# LOAD R DATA
load("C:\\Users\\hp1\\Documents\\R데이터마이닝 프로젝트1.RData")

# URL 크롤링

get_science <- function(){
  out <- NULL
  url <- "http://navercast.naver.com/list.nhn?cid=453&category_id=453&sort=update&list_type=image&page="
  for(i in 1:59){
    page.url <- paste0(url,i)
    temp <- readLines(page.url)
    get <- temp[which(str_detect(temp,"<div class=\"card_w\""))+1]
    get <- str_sub(get, 16)
    end <- str_locate(get,"class")[,1]-3
    get <- str_sub(get, end=end)
    out <- append(out, get)
    cat("\n",i)
  }
  out <- paste0("http://navercast.naver.com/", out)
  return(out)
}
get_liberal <- function(){
  out <- NULL
  url <- "http://navercast.naver.com/list.nhn?cid=451&category_id=451&sort=update&list_type=image&page="
  for(i in 1:64){
    page.url <- paste0(url,i)
    temp <- readLines(page.url)
    get <- temp[which(str_detect(temp,"<div class=\"card_w\""))+1]
    get <- str_sub(get, 16)
    end <- str_locate(get,"class")[,1]-3
    get <- str_sub(get, end=end)
    out <- append(out, get)
    cat("\n",i)
  }
  out <- paste0("http://navercast.naver.com/", out)
  return(out)
} 

science_url <- get_science()
liberal_url <- get_liberal()

url <- c(science_url, liberal_url)
category <- c(rep('Science', 1475), rep('Liberal', 1599))
url_memory <- data.frame(url, category)

write.csv(url_memory, "url_memory.csv", row.names = F)

#################################################################3

url_memory <- read.csv("url_memory.csv", stringsAsFactors = F)
liberal_url <- subset(url_memory, category == "Liberal")[,1]
Science_url <- subset(url_memory, category == "Science")[,1]

## 텍스트리 전처리

# 인문과학 글 크롤링&전처리
pre <- function(x){
  temp <- readLines(x, encoding = "UTF-8")
  start <- which(str_detect(temp, "-- 컨텐츠 내용 --"))
  end <- which(str_detect(temp, "-- 관련글 --"))
  contents <- temp[start:end]
  contents <- gsub("<.*?>","", contents)
  contents <- gsub("\t","",contents)
  contents <- gsub("[0-9]","",contents)
  contents <- gsub("[con-zcon-z]","",contents)
  contents <- contents[-which(nchar(contents)==0)]
  contents <- paste(contents, collapse = " ")
  contents <- gsub("<.*?>","", contents)
  contents <- gsub("[con-zcon-z]","",contents)
  contents <- gsub("인쇄 본 콘텐츠의 저작권은 저자 또는 제공처에 있으며, 이를 무단 이용하는 경우 저작권법 등에 따라 법적책임을 질 수 있습니다.외부 저작권자가 제공한 콘텐츠는 네이버의 입장과 다를 수 있습니다.","",contents)
  contents <- gsub("네이버","",contents)
  contents <- gsub("그림","",contents)
  contents <- gsub("출처","", contents)
  contents <- gsub("주석","",contents)
  contents <- gsub("[[:punct:]]"," ", contents)
  cat("\n",i)
  i <<- i+1
  return(contents)
}

i = 1
Liberal_Contents <- as.vector(sapply(liberal_url, pre))

# 자연과학 글 크롤링&전처리
i =1
Science_Contents <- as.vector(sapply(Science_url, pre))

# Tokenizer
konlp_tokenizer <- function(x){
  x <- as.character(x)
  foreach(p=x, .combine='c') %do% extractNoun(p)
}
ko.words <- function(x){
  x <- as.character(x)
  pos <- paste(SimplePos09(x))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}

## KoNLP에서 오류나오는 문서 삭제
memory.error.liberal <- NULL
for(i in 1:1599){
  tryCatch({
    extractNoun(Liberal_Contents[i])
    cat("\n",i)
  }, error=function(e){memory.error.liberal <<- c(memory.error.liberal, i)})
}
memory.error.liberal

memory.error.science <- NULL
for(i in 1:1475){
  tryCatch({
    extractNoun(Science_Contents[i])
    cat("\n",i)
  }, error=function(e){memory.error.science <<- c(memory.error.science, i)})
}
memory.error.science

Liberal_Contents1 <- Liberal_Contents[-memory.error.liberal]
Science_Contents1 <- Science_Contents[-memory.error.science]
## KoNLP에서 오류나오는 문서 삭제

# 인문과학 Term-Document Matrix 생성
corpus.liberal <- Corpus(VectorSource(Liberal_Contents1))

tdm.liberal <- TermDocumentMatrix(corpus.liberal,
                                  control=list(tokenize = ko.words,
                                               wordLengths=c(2,5))
)
inspect(tdm.liberal[1000:1020,1:4])
dim(inspect(tdm.liberal))
findFreqTerms(tdm.liberal, lowfreq = 10)
count.liberal <- sort(rowSums(as.matrix(inspect(tdm.liberal))), decreasing = T)
View(data.frame(count.liberal[1:100]))

# 자연과학 Term-Document Matrix 생성
corpus.science <- Corpus(VectorSource(Science_Contents1))
tdm.science <- TermDocumentMatrix(corpus.science,
                                  control=list(tokenize = ko.words,
                                               wordLengths=c(2,5))
)

inspect(tdm.science[1:20,1:4])
dim(inspect(tdm.science))
findFreqTerms(tdm.science, lowfreq = 10)
count.science <- sort(rowSums(as.matrix(inspect(tdm.science))), decreasing = T)
View(data.frame(count.science[1:100]))


View(data.frame(names(count.science[1:1000]), names(count.liberal[1:1000])))


## 전체문서 Term-Document Matrix 생성
corpus.full <- Corpus(VectorSource(c(Science_Contents1, Liberal_Contents1)))
tdm.full <- TermDocumentMatrix(corpus.full,
                               control=list(tokenize = ko.words,
                                            wordLengths=c(2,5))
)
count.full <- sort(rowSums(as.matrix(inspect(tdm.full))), decreasing = T)
View(data.frame(count.full[1:100]))
dim(tdm.full)


## Word Cloud
library(wordcloud)
library(RColorBrewer)
palete <- brewer.pal(100, "Set1")
par(mfrow=c(1,2))
wordcloud(names(count.science[1:100]), freq=count.science[1:100]^2, scale=c(5,1), rot.per = 0.1,
          random.order = F, colors = palete)
wordcloud(names(count.liberal[1:100]), freq=count.liberal[1:100]^2, scale=c(5,1), rot.per = 0.1,
          random.order = F, colors = palete)


## 특징추출 - LDA Topic Modeling
library(lda)
nouns <- list()
length(nouns) <- 3061
Full_Contents <- c(Liberal_Contents1, Science_Contents1)
for(i in 1:3061){
  nouns[[i]] <- ko.words(Full_Contents[i])
  cat("\n",i)
}

corpus <- lexicalize(nouns)
result <- lda.collapsed.gibbs.sampler(corpus$documents, 100, corpus$vocab,
                                      1000, 1, 0.1)
topic <- top.topic.words(result$topics, 30, by.score = T)
topic <- as.data.frame(topic)
for(i in 1:100){
  topic[,i] <- as.character(topic[,i])
  topic[,i] <- gsub("[[:punct:]]","",topic[,i])
  topic[,i] <- gsub("\n","",topic[,i])
}

# LDA를 활용한 차원축소
data.reduce <- matrix(0, nrow=3061, ncol=100)
for(x in 1:3061){
  for(i in 1:100){
    for(j in 1:30){
      if(topic[j,i] %in% nouns[[x]]){
        data.reduce[x,i] <- 1
      }
    }
  }
  cat("\n",x)
}

data.reduce1 <- matrix(0, nrow=3061, ncol=100)
for(x in 1:3061){
  for(i in 1:100){
    for(j in 1:30){
      if(topic[j,i] %in% nouns[[x]]){
        data.reduce1[x,i] <- data.reduce1[x,i]+1
      }
    }
  }
  cat("\n",x)
}
data.reduce1[1020:1030,]
data.reduce1 <- as.data.frame(data.reduce1)

## 분류모델 - Naive Bayes, Knn, Support Vector Machine

# Data partition
library(caret)
label <- c(rep("Science",1471),rep("Liberal",1590))
train.index <- as.vector(createDataPartition(label, p=0.5, list=F))

train.data <- data.reduce[train.index,]
train.label <- label[train.index]
train.data <- cbind(train.label, train.data)
colnames(train.data)[1] <- 'Label'

test.data <- data.reduce[-train.index,]
test.label <- label[-train.index]
dim(test.data)

###################################################

train.data1 <- data.reduce1[train.index,]
train.data1 <- cbind(train.label, train.data1)
colnames(train.data1)[1] <- 'Label'

test.data1 <- data.reduce1[-train.index,]
test.label <- label[-train.index]
dim(test.data)

# Naive bayes
library(e1071)
nb_fit <- naiveBayes(Label~., data=train.data, laplace = 1)
mean(predict(nb_fit, train.data, type='class')==train.label)

pred.nb <- predict(nb_fit, test.data, type='class')
mean(pred.nb==test.label) # 58.30% - 생각보다 낮은 정분류율..
table(pred.nb, test.label) # 대부분의 문서를 Liberal이라고 예측..

x = train.data[,-c(n,1)]
y = train.data[,1]

# Cross Validation으로 성능향상
nb_fit1 <- train(x, y, 'nb',
                 trControl = trainControl('cv', number=10))
nb_fit1
mean(predict(nb_fit1, train.data, type='raw')==train.label) # 64.27%

pred.nb1 <- predict(nb_fit1, test.data, type='raw')
mean(pred.nb1==test.label) # 64.70% - 성능이 약간 상승했지만 여전히 낮은 편
table(pred.nb1, test.label) # 여전히 많은 문서를 Liberal이라 예측


# Support Vector Machine
svm_fit <- svm(Label ~ . , data = train.data)
svm_fit
mean(predict(svm_fit, train.data, type='class')==train.label) # 93.53% - 상당히 좋은 성능
table(predict(svm_fit, train.data, type='class'),train.label) # 골고루 잘 맞춘다.

pred.svm <- predict(svm_fit, test.data, type='class')
mean(pred.svm==test.label) # 85.62%
table(pred.svm, test.label)

boost.svm <- function(n, train.data, test.data){
  mm <- NULL
  for(i in 1:n){
    temp <- train.data[sample(1:1531, 0.7*1531),]
    model <- svm(Label ~ ., data = temp)
    out <- predict(model, test.data)
    mm <- cbind(mm, as.character(out))
    cat("\n",i)
  }
  ret <- function(x){
    return(names(sort(table(x), decreasing = T))[1])
  }
  return(as.factor(apply(mm, 1,ret)))
}

pred.svm1 <- boost.svm(30, train.data, test.data)
mean(pred.svm1==test.label) # 86.20% - 소폭상승..
table(pred.svm1, test.label)


# KNN
library(class)
knn_fit <- knn(train=x, test.data[,-(n-1)], cl=y, k=20)
mean(knn_fit==test.label) # 84.37% - 생각보다 knn의 성능이 우수하다.
table(knn_fit, test.label)

knn_fit1 <- knn(train.data1[,-1], test.data1, cl=train.data1[,1], k=10)
mean(knn_fit1==test.label) # 96.14%
table(knn_fit1, test.label)
