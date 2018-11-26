library(plyr)
library(tidyverse)
# Qunis Palette
qpal <- c("#009fe3","#023e84", "#2bad70", "#e8423b", "#1d1d1b", "#7c7c7b", "#d0d0d0", "#ffffff" )

ui <- fluidPage(

  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      numericInput('cluster', 'Anzahl Cluster', min=1, max=10, value = 1),
      checkboxInput("showspec", "Show Species")
    ),
    
    # Show a plot 
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  data("iris")
  dat <- iris[, 3:4]
  output$plot<- renderPlot({
    
    km <- kmeans(dat, input$cluster, nstart = 20) 
    
    find_hull <- function(df) df[chull(df[,1], df[,2]), ]
    hulls <- ldply(split(dat, km$cluster), find_hull, .id='cluster')
    hulls2 <- ldply(split(dat, iris$Species), find_hull, .id='species')
    
    center_df <- mutate(as_tibble(km$centers), cluster=factor(1:input$cluster))
    
    g <- ggplot(iris, aes(Petal.Length, Petal.Width, color = as.factor(km$cluster))) + geom_point() +
      scale_color_discrete(name='Cluster') + 
      geom_polygon(data=hulls, aes(Petal.Length, Petal.Width, color=cluster), alpha = 0.2) +
      geom_point(data=center_df, aes(Petal.Length, Petal.Width, color=cluster), size=5, shape=4)
    
    if (input$showspec) g <- g+  geom_polygon(data=hulls2, aes(Petal.Length, Petal.Width, color=species), alpha = 0.2) 
    g
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)