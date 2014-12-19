#### ui script

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  

  # Application title
  titlePanel("Party Word Clouds"),
  
  p("Jeremy Yang"),

  
  # Sidebar with a slider input for the number of bins
  
  
  fluidRow(
    
   column(2,h5("Democrat Manifesto:")),
    
    column(3,
           plotOutput('plot1')),
 
    column(2,h5("Republican Manifesto:")),
        
    column(3,
           plotOutput('plot2'))
  ),
  
  fluidRow(
    
    column(2,h5("Democrat Speech:")),
    column(3,
           plotOutput('plot3')),
    column(2,h5("Republican Speech:")),
    column(3,
           tableOutput('table'))
  ),
  
  hr(),
  
  fluidRow(
    
    column(5,p("")),
    column(2,
             selectInput("select1",label=p("select a year:"),choices=as.character(seq(1956,2012,4)),selected="2012")),
    column(5,p(""))

    
    
    )
  

  ))

  

