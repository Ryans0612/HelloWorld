library(tm)
library(readr)
library(readxl)
library(rlang)

install.packages("tm")
library(tm)

englishdata<-read_excel("D:/Data STATBIS ITS/Sem 6/Metode Data Tidak Terstruktur/prakyiium2/englishdata.xlsx")
View(englishdata)
#mengubah data menjadi corpus/kumpulan
corpusdata<-Corpus(VectorSource(onepiece$`Tweet Text`))
inspect(corpusdata[1:10])

#mengubah huruf menjadi kecil
casefolding<-tm_map(corpusdata, content_transformer(tolower))
inspect(casefolding[1:10])

#menghapus url
removeURL<-function(x) gsub("http[^[:space:]]*","",x)
data_URL<-tm_map(casefolding,content_transformer(removeURL))
inspect(data_URL[1:10])

#menghapus mention
removemention<-function(x) gsub("@\\s+","",x)
data_mention<-tm_map(data_URL,removemention)
inspect(data_mention[1:10])

#menghapus hastag
removehashtag<-function(x) gsub("#\\s+","",x)
data_hashtag<-tm_map(data_mention,removehashtag)
inspect(data_hashtag[1:10])

#menghapus tanda baca
data_punctuation<-tm_map(data_hashtag,content_transformer(removePunctuation))
inspect(data_punctuation[1:10])

#menghapus angka
data_nonumber<-tm_map(data_punctuation,content_transformer(removeNumbers))
inspect(data_nonumber[1:10])

#menghapus emoticon
removeemot<-function(x) gsub("[^\x01-\x7F]","",x)
data_emot<-tm_map(data_nonumber, content_transformer(removeemot))
inspect(data_emot[1:10])


#import data stopword
library(readxl)
dictionary<-read_excel("D:/Data STATBIS ITS/Sem 6/Metode Data Tidak Terstruktur/prakyiium2/dictionary.xlsx")
View(dictionary)

#remove stopword
removestopword<-tm_map(data_emot,removeWords,dictionary$dictionary)
inspect(removestopword[1:10])

#remove spasi berlebihan
removespace<-tm_map(removestopword,content_transformer(stripWhitespace))
inspect(removespace[1:10])


#tokenizing
install.packages("tokenizers")
library(tokenizers)
datatoken<-data.frame(text=unlist(sapply(removespace, '[')))
token<-tokenize_words(datatoken$text)
View(token)

#menyimpan clean data
cleandata<-data.frame(text=unlist(sapply(removespace,'[')), tringSASFactors=F)
write.csv(cleandata,file="cleandata.csv")


install.packages("devtools")
library(devtools)
install_github("nurandi/katadasaR")
library(katadasaR)
katadasaR::katadasar

#stemming
stemming<-function(x){
  paste(lapply(x,katadasar),collapse=" ")}
tweets<-lapply(tokenize_words(cleandata$text[]), stemming)
View(tweets)

#wordcloud
library(wordcloud)
wordcloud(cleandata$text, max.words = 100,colors=brewer.pal(8,"Dark2"))





file <- "cleandata.csv"
data <- read.csv(file)
View(data)