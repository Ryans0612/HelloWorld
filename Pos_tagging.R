getwd()
library(rJava)
library(readxl)
cleandata<- read.csv("D:/Data STATBIS ITS/Sem 6/Metode Data Tidak Terstruktur/Praktikum terjemahan/2043201062_Andryan.csv")
View(cleandata)

#
install.packages('rJava', type = 'binary')
library("stringr")
library("NLP")
library("openNLP")
library("openNLPdata")

#convert to corpus
library(tm)
library(NLP)
corpus_text<-Corpus(VectorSource(cleandata$Indo))
inspect(corpus_text)

postag<-function(object){
  require("stringr")
  require("NLP")
  require("openNLP")
  require("openNLPdata")
  
  #define path to corpus file
  corpus.tmp<-object
  sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  pos_tag_annotator <- openNLP::Maxent_POS_Tag_Annotator(language="en",probs=FALSE, model= NULL)
  
  #Convert all file content to strings
  Corpus<- lapply(corpus.tmp,function(x){
    x<- as.String(x)
  })
  #loop over file contents
  lapply(Corpus, function(x){
    y1<-NLP::annotate(x,list(sent_token_annotator,word_token_annotator))
    y2<-NLP::annotate(x,pos_tag_annotator,y1)
    y2w<-subset(y2,type=="word")
    tags<-sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s/%s", x[y2w], tags)
    r2 <- paste(r1, collapse = " ")
    return(r2)
  })
}

#pos tagging data
pos_tagging <- postag(object = corpus_text)
View(pos_tagging)
