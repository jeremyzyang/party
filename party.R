#party#   


########################################################################################################################
#get text from webpage and analyze the text

# we need "XML" package to read html files
install.packages("XML")
library(XML)


# HtmltoTxt function: extract text from webpage
## we pass a url and a local path to the function,it returns a txt file at the location we specified in the path
## for simplicity, it only takes one url but we can easily generalize it to take a vector of urls and return multiple txt files

HtmltoTxt <- function (url,path)                           #filename needs to be included in the path
{ library(XML) 
  
  html <- htmlTreeParse(url,useInternal=TRUE)              #parse html file into text
  text <- unlist(xpathApply(html,'//p',xmlValue)) 
  text <- gsub('\\n',' ',text)
  text <- paste(text,collapse=' ')
  
  write.table(text,path,row.names=FALSE,col.names=FALSE)   #write text into txt file and save it
}

## an example of how to use HtmltoTxt
HtmltoTxt("http://www.presidency.ucsb.edu/ws/index.php?pid=101962","C:/Users/Jeremy/Desktop/democratic2012.txt")




# creat a R object for the corpus we need to analyze
library(tm)  
corpus <- Corpus(DirSource("C:/Users/Jeremy/Desktop/Workspace/R/party/democrat")) # specify to the name of the folder containing all text files
                 
# wordcount function: count the frequency of keyword
## we pass the corpus object and the keyword we need to count to the function,it returns a vector of frequency of word
## for simplicity, it only takes one keyword but again, we can easily generalize it to take a vector of keywords  

wordcount <- function (corpus,keyword)                     #read in the R object we created for the corpus
{ count <- 1:length(corpus)  
  
  for (i in 1:length(corpus))                              #loop over all texts in corpus
    {txt <- corpus[[i]]
    s <- strsplit(txt," ")
    s <- tolower(s[[1]])
    count[i] <- length(grep(c(keyword),s,value=TRUE))}  
  
  return (count)                                           #return the vector of frequency of keyword
}  

## an example of how to use wordcount function
wordcount(corpus,"god")



#########################################################################################################################
# visualization

# Some simple graphing
library(ggplot2)

abort.plot <- ggplot(abort, aes(x=time)) + 
  geom_point(aes(y = freq.d.abort)) +
  geom_point(aes(y = freq.r.abort)) +
  xlab("Year") +
  ylab("Freq of 'abort'") +
  ggtitle("freq and dens of 'abort' 1856-2012: Democratic vs. Republican") +
  geom_line(aes(y = freq.d.abort, colour= "red")) + 
  geom_line(aes(y = freq.r.abort, colour= "blue")) +
  theme(legend.position="none") +
  scale_x_continuous(breaks = seq(1856, 2012, 8))

god.plot <- ggplot(god, aes(x=time)) + 
  geom_point(aes(y = freq.d.god)) +
  geom_point(aes(y = freq.r.god)) +
  xlab("Year") +
  ylab("Freq of 'god'") +
  ggtitle("freq of 'god' 1856-2012: Democratic vs. Republican") +
  geom_line(aes(y = freq.d.god, colour= "red")) + 
  geom_line(aes(y = freq.r.god, colour= "blue")) +
  theme(legend.position="none") +
  scale_x_continuous(breaks = seq(1856, 2012, 8))

religi.plot <- ggplot(religi, aes(x=time)) + 
  geom_point(aes(y = freq.d.religi)) +
  geom_point(aes(y = freq.r.religi)) +
  xlab("Year") +
  ylab("Freq of 'religi'") +
  ggtitle("freq of 'religi' 1856-2012: Democratic vs. Republican") +
  geom_line(aes(y = freq.d.religi, colour= "red")) + 
  geom_line(aes(y = freq.r.religi, colour= "blue")) +
  theme(legend.position="none") +
  scale_x_continuous(breaks = seq(1856, 2012, 8))

##############################################################################
# wordcloud 

install.packages('NLP')
library(NLP)
install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)

d.corpus <- Corpus(DirSource("C:/Users/Jeremy/Desktop/Workspace/R/party/democrat"))
summary(d.corpus)

d.corpus.1<- d.corpus[1:21] # 1840-1920
d.corpus.2 <- d.corpus[22:26] # 1924-1940
d.corpus.3 <- d.corpus[27:36] # 1944-1980
d.corpus.4 <- d.corpus[37:44] # 1980-2012

normalize <- function(corpus){ # conversion to lower case, other function: removeNumbers,removePunctuation,removeWords, 
wordcloud <- tm_map(corpus,content_transformer(removePunctuation))
wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
return(wordcloud)}


# try a subset of cleaning functions
wordcloud <- tm_map(d.corpus,content_transformer(removePunctuation))
wordcloud <- tm_map(d.corpus,content_transformer(tolower)) 
wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))

d.wordcloud <- normalize(d.corpus)
d.wordcloud.1 <- normalize(d.corpus.1)
d.wordcloud.2 <- normalize(d.corpus.2)
d.wordcloud.3 <- normalize(d.corpus.3)
d.wordcloud.4 <- normalize(d.corpus.4)

wordlist <- function(wordcloud){
dtm <- DocumentTermMatrix(wordcloud)
wordlist <- colSums(as.matrix(dtm))
return(wordlist)}

d.df <- DocumentTermMatrix(wordcloud)
d.df <- as.data.frame( t(as.matrix(  d.df )) ) 
dim(d.df)
class(d.df)

write.csv(d.df,'C:/Users/Jeremy/Desktop/R_example.csv')

d.df[1:20,1:30]

d.wordlist <- wordlist(d.wordcloud)
d.wordlist.1 <- wordlist(d.wordcloud.1)
d.wordlist.2 <- wordlist(d.wordcloud.2)
d.wordlist.3 <- wordlist(d.wordcloud.3)
d.wordlist.4 <- wordlist(d.wordcloud.4)

# build a word cloud
install.packages('wordcloud')
library(wordcloud)

wordcloud(names(d.wordlist.1),d.wordlist.1,max.word=400,rot.per=0.2)
wordcloud(names(d.wordlist.2),d.wordlist.2,max.word=400,rot.per=0.2)
wordcloud(names(d.wordlist.3),d.wordlist.3,max.word=400,rot.per=0.2)
wordcloud(names(d.wordlist.4),d.wordlist.4,max.word=400,rot.per=0.2)
wordcloud(names(d.wordlist),d.wordlist,max.word=400,rot.per=0.2,color=brewer.pal(6,'Dark2'))

# then we do the same thing for republican
r.corpus <- Corpus(DirSource("C:/Users/Jeremy/Desktop/Workspace/R/party/republican"))
summary(r.corpus)

# divide the corpus into four sub-corpus by era
r.corpus.1 <- r.corpus[1:17] # 1856-1920
r.corpus.2 <- r.corpus[18:22] # 1924-1940
r.corpus.3 <- r.corpus[23:32] # 1944-1980
r.corpus.4 <- r.corpus[33:40] # 1984-2012

r.wordcloud <- normalize(r.corpus)
r.wordcloud.1 <- normalize(r.corpus.1)
r.wordcloud.2 <- normalize(r.corpus.2)
r.wordcloud.3 <- normalize(r.corpus.3)
r.wordcloud.4 <- normalize(r.corpus.4)

r.wordlist <- wordlist(r.wordcloud)
r.wordlist.1 <- wordlist(r.wordcloud.1)
r.wordlist.2 <- wordlist(r.wordcloud.2)
r.wordlist.3 <- wordlist(r.wordcloud.3)
r.wordlist.4 <- wordlist(r.wordcloud.4)

# we build 5 wordcloud for republican, one for each era and one for all years

# 1856-1920
wordcloud(names(r.wordlist.1),r.wordlist.1,max.word=400,rot.per=0.2)
# 1924-1940
wordcloud(names(r.wordlist.2),r.wordlist.2,max.word=400,rot.per=0.2)
# 1944-1980
wordcloud(names(r.wordlist.3),r.wordlist.3,max.word=400,rot.per=0.2)
# 1984-2012
wordcloud(names(r.wordlist.4),r.wordlist.4,max.word=400,rot.per=0.2)

# all years
wordcloud(names(r.wordlist),r.wordlist,max.word=400,rot.per=0.2,colors=brewer.pal(6,'Dark2'))

###################################################################################
# To Freq Words Data Frame

## democrat manifesto 
d.corpus <- Corpus(DirSource("C:/Users/Jeremy/Desktop/Workspace/R/party/democrat"))

d.corpus.clean <- normalize(d.corpus)
dtm <- DocumentTermMatrix(d.corpus.clean)

m <- as.matrix(dtm)
wordlist <- colSums(as.matrix(dtm))

v <- sort(m[1,], decreasing=TRUE)
vm <- as.matrix(v)  
vm.top <- as.matrix(vm[1:100,])
colnames(vm.top) <- 'freq'
time <- rep(1840,100)
democrat <- rep(1,100)
manifesto <- rep(1,100)
words <- rownames(vm.top) 
w <- cbind(time,words,vm.top,democrat,manifesto)
rownames(w) <- NULL

for (i in 2:44) {

  v <- sort(m[i,], decreasing=TRUE)
  vm <- as.matrix(v)  
  vm.top <- as.matrix(vm[1:100,])
  colnames(vm.top) <- 'freq'
  time <- rep(1840+4*(i-1),100)
  democrat <- rep(1,100)
  manifesto <- rep(1,100)
  words <- rownames(vm.top) 
  v <- cbind(time,words,vm.top,democrat,manifesto)
  rownames(v) <- NULL
  w <- rbind(w,v)
} 

d.w <- w

## republican manifesto
r.corpus <- Corpus(DirSource("C:/Users/Jeremy/Desktop/Workspace/R/party/republican"))

r.corpus.clean <- normalize(r.corpus)
r.dtm <- DocumentTermMatrix(r.corpus.clean)

r.m <- as.matrix(r.dtm)

v <- sort(r.m[1,], decreasing=TRUE)
vm <- as.matrix(v)  
vm.top <- as.matrix(vm[1:100,])
colnames(vm.top) <- 'freq'
time <- rep(1856,100)
democrat <- rep(0,100)
manifesto <- rep(1,100)
words <- rownames(vm.top) 
r.w <- cbind(time,words,vm.top,democrat,manifesto)
rownames(r.w) <- NULL

for (i in 2:40) {
  v <- sort(m[i,], decreasing=TRUE)
  vm <- as.matrix(v)  
  vm.top <- as.matrix(vm[1:100,])
  colnames(vm.top) <- 'freq'
  time <- rep(1856+4*(i-1),100)
  democrat <- rep(0,100)
  manifesto <- rep(1,100)
  words <- rownames(vm.top) 
  v <- cbind(time,words,vm.top,democrat,manifesto)
  rownames(v) <- NULL
  r.w <- rbind(r.w,v)
} 

## democrat presidential speech 
d.s.corpus <- Corpus(DirSource("C:/Users/Jeremy/Desktop/Workspace/R/party/democrat_speech"))

d.s.corpus.clean <- normalize(d.s.corpus)
d.s.dtm <- DocumentTermMatrix(d.s.corpus.clean)

d.s.m <- as.matrix(d.s.dtm)

v <- sort(d.s.m[1,], decreasing=TRUE)
vm <- as.matrix(v)  
vm.top <- as.matrix(vm[1:100,])
colnames(vm.top) <- 'freq'
time <- rep(1916,100)
democrat <- rep(1,100)
manifesto <- rep(0,100)
words <- rownames(vm.top) 
d.s.w <- cbind(time,words,vm.top,democrat,manifesto)
rownames(d.s.w) <- NULL

for (i in 2:23) {
  v <- sort(m[i,], decreasing=TRUE)
  vm <- as.matrix(v)  
  vm.top <- as.matrix(vm[1:100,])
  colnames(vm.top) <- 'freq'
  time <- rep(1924+4*(i-1),100)
  democrat <- rep(1,100)
  manifesto <- rep(0,100)
  words <- rownames(vm.top) 
  v <- cbind(time,words,vm.top,democrat,manifesto)
  rownames(v) <- NULL
  d.s.w <- rbind(d.s.w,v)
} 

## republican presidential speech
r.s.corpus <- Corpus(DirSource("C:/Users/Jeremy/Desktop/Workspace/R/party/republican_speech"))

r.s.corpus.clean <- normalize(r.s.corpus)
r.s.dtm <- DocumentTermMatrix(r.s.corpus.clean)

r.s.m <- as.matrix(r.s.dtm)
dim(r.s.m)

v <- sort(r.s.m[1,], decreasing=TRUE)
vm <- as.matrix(v)  
vm.top <- as.matrix(vm[1:100,])
colnames(vm.top) <- 'freq'
democrat <- rep(0,100)
manifesto <- rep(0,100)
words <- rownames(vm.top) 
r.s.w <- cbind(words,vm.top,democrat,manifesto)
rownames(r.s.w) <- NULL

for (i in 2:27) {
  v <- sort(m[i,], decreasing=TRUE)
  vm <- as.matrix(v)  
  vm.top <- as.matrix(vm[1:100,])
  colnames(vm.top) <- 'freq'
  democrat <- rep(0,100)
  manifesto <- rep(0,100)
  words <- rownames(vm.top) 
  v <- cbind(words,vm.top,democrat,manifesto)
  rownames(v) <- NULL
  r.s.w <- rbind(r.s.w,v)
} 

time1 <- c(rep(1864,100),rep(1880,100),rep(1888,100),rep(1892,100),rep(1900,100),rep(1908,100),rep(1920,100),rep(1932,100))
a <- rep(1940,100)
for (j in 2:19){
  b <- rep(1940+4*(j-1),100)
  a <-c(a,b)  
}
time2 <- a
time <- c(time1,time2) 

r.s.w <- cbind(time,r.s.w)

## merge 4 pieces of data into one
df <- rbind(d.w,r.w,d.s.w,r.s.w)
dim(df)

write.csv(df,"C:/Users/Jeremy/Desktop/R_example.csv")
x <- read.csv("C:/Users/Jeremy/Desktop/R_example.csv")
x <- x[,-1]
head(x)
x <- x[,c('time','democrat','manifesto','words','freq')]
head(x)
write.csv(x,"C:/Users/Jeremy/Desktop/R_example.csv")

## sentiment analysis
install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
require("tm.lexicon.GeneralInquirer")
sapply(acq[1:10], tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
sapply(acq[1:10], tm_term_score, terms_in_General_Inquirer_categories("Negativ"))
positive <- tm_term_score(dtm,terms_in_General_Inquirer_categories("Positiv"))
negative <- tm_term_score(dtm,terms_in_General_Inquirer_categories("Negativ"))

class(positive)
x <- positive[1]
as.numeric(x)

