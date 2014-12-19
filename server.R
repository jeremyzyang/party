#### server script 

library(shiny)
library(tm)
library(wordcloud)
library(SnowballC)

# read data
data <- Corpus(DirSource("data"))

# server function

shinyServer(function(input, output) {

  
  
  
  cloud.d <- reactive({
  
  year <- (as.numeric(input$select1)-1952)/4    
  
  wordcloud <- tm_map(data[year],content_transformer(removePunctuation))
  wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
  wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
  wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
  wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
  wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
  wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
   
  dtm <- DocumentTermMatrix(wordcloud)
  wordlist.d <- colSums(as.matrix(dtm))    
  return(wordlist.d)
  
  })
  
  cloud.r <- reactive({
    
    year <- (as.numeric(input$select1)-1952)/4 + 30    
    
    wordcloud <- tm_map(data[year],content_transformer(removePunctuation))
    wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
    wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
    wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
    wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
    
    dtm <- DocumentTermMatrix(wordcloud)
    wordlist.r <- colSums(as.matrix(dtm))
    
    return(wordlist.r)
    
  })
  
  cloud.d.s <- reactive({
    
    year <- (as.numeric(input$select1)-1952)/4 + 15 
    
    wordcloud <- tm_map(data[year],content_transformer(removePunctuation))
    wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
    wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
    wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
    wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
    
    dtm <- DocumentTermMatrix(wordcloud)
    wordlist.d.s <- colSums(as.matrix(dtm))
    
    return(wordlist.d.s)
    
  })
  
  cloud.r.s <- reactive({
    
    year <- (as.numeric(input$select1)-1952)/4 + 45    
    
    wordcloud <- tm_map(data[year],content_transformer(removePunctuation))
    wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
    wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
    wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
    wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
    
    dtm <- DocumentTermMatrix(wordcloud)
    wordlist.r.s <- colSums(as.matrix(dtm))
    
    return(wordlist.r.s)
    
  })
  

# check it's manifesto or speech
  
  
  output$plot1 <- renderPlot({
    wordcloud(names(cloud.d()),cloud.d(),scale=c(5,.5),max.word=100,rot.per=.3,color=brewer.pal(6,'Dark2')) 
  })
 
  output$plot2 <- renderPlot({
   wordcloud(names(cloud.r()),cloud.r(),scale=c(5,.5),max.word=100,rot.per=.3,color=brewer.pal(6,'Dark2'))
  })  
    
  output$plot3 <- renderPlot({
      wordcloud(names(cloud.d.s()),cloud.d.s(),scale=c(5,.5),max.word=100,rot.per=.3,color=brewer.pal(6,'Dark2')) 
  })
    
  output$plot4 <- renderPlot({
      wordcloud(names(cloud.r.s()),cloud.r.s(),scale=c(5,.5),max.word=100,rot.per=.3,color=brewer.pal(6,'Dark2'))
  })
 

})    
