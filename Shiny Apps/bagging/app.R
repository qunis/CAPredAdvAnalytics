
library(ElemStatLearn)
library(caret)

data(ozone,package="ElemStatLearn")
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")




ui <- fluidPage(

  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
        numericInput("n",
                  "Show Model Number:",
                  min = 1,
                  max = 10,
                  value = 1, 
                  step = 1),
        checkboxInput("showbag", "Show Bagged Model")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$plot<- renderPlot({
      showbag <- input$
          showbag
      n <- input$n
      plot(ozone$ozone,temperature,col='lightgrey',pch=19)
      points(ozone$ozone,predict(treebag$fits[[n]]$fit,predictors),pch=19,col="red")
      
      if(showbag) points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)