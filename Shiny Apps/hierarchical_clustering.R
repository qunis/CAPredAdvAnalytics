
qpal <- c("#009fe3","#023e84", "#2bad70", "#e8423b", "#1d1d1b", "#7c7c7b", "#d0d0d0", "#ffffff" )
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  # titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        sliderInput("cl", "Cluster level",
                     min=1, max=150, value=1, step=1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot<- renderPlot({
      
      cl <- input$cl
      
      dat <- select(iris, Petal.Length, Petal.Width)
      cluster <- hclust(dist(dat))
      
      m <- cluster$merge
      dat <- mutate(dat, weight=1)
      cldat <- mutate(dat, Petal.Length=NA, Petal.Width=NA, weight=NA)
      for (k in 1:cl){
          mk <- m[k,]
          dat[abs(mk),]
          newcl <- list()
          for(i in 1:2){
              if(mk[i]<0) {
                  newcl[[i]] <- list(data=dat[abs(mk[i]),1:2], weight=1)
                  dat[abs(mk[i]),] <- NA
              }else{
                  newcl[[i]]  <- list(data=cldat[abs(mk[i]),1:2], weight=cldat[abs(mk[i]),3])
                  cldat[abs(mk[i]),] <- NA
              }
          }
          sumweight <- newcl[[1]]$weight + newcl[[2]]$weight
          newpoint <-( newcl[[1]]$data * newcl[[1]]$weight + 
                           newcl[[2]]$data * newcl[[2]]$weight ) / sumweight
          cldat[k,] <- c(newpoint, sumweight)
      }
      
      plotdat <- na.omit(rbind(dat, cldat))
      
      
      ggplot(plotdat) + geom_point(aes(Petal.Length, Petal.Width, size=weight), alpha=.4, color=qpal[2]) +
          coord_cartesian(xlim=c(0,7), ylim=c(0,3)) + scale_size_identity() +
          guides(size=FALSE)
      #geom_point(data=dat[c(1,2),], aes(Petal.Length, Petal.Width), size=2, color=qpal[4])
      
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)