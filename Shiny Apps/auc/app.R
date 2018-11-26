# Qunis Palette
qpal <- c("#009fe3","#023e84", "#2bad70", "#e8423b", "#1d1d1b", "#7c7c7b", "#d0d0d0", "#ffffff" )
library(tidyverse)
library(kernlab)
library(caret)
library(zoo)

ui <- fluidPage(

  # Sidebar with a slider input 
  fluidRow(
    column(width=6,
        sliderInput("alpha", "Overlap",
                    min=0,
                    max=1,
                    value=.5,
                    step=.05)
            ),
    column(width=6,
        h4('Die Fläche unter der ROC Curve ist ein gängiges Gütemaß:'),
        textOutput("auc")
        )
    ),
    
    # Show a plot of the generated distribution
    fluidRow(
    column(width=6, plotOutput("plot")),
    column(width=6, plotOutput("plot2"))
    )
)

# Define server logic 
server <- function(input, output) {

  x <- seq(-8,8, .1)

    output$plot<- renderPlot({
        alpha <- input$alpha
        y1 <- dnorm(x, (1-alpha)*4)
        y2 <- dnorm(x, (alpha-1)*4)
     
        ggplot() + geom_area(aes(x, y1), fill=qpal[1], alpha=.2) + 
            geom_area(aes(x, y2), fill=qpal[4], alpha=.2) +
            geom_vline(xintercept = 0, linetype=2) + 
            labs(x="", y="") +
            scale_x_continuous(labels=NULL) + 
            scale_y_continuous(labels=NULL)
    }, height=300, width=350)
 
    output$plot2<- renderPlot({
        k_seq <- seq(-8,8,by=.05)  
        alpha <- input$alpha
        ROC_dat  <- data.frame(
            Sensitivity = pnorm(k_seq, (1-alpha)*4, lower.tail = F),
            Specificity = pnorm(k_seq, (alpha-1)*4, lower.tail = T))
        
        ggplot() + geom_line(aes(x=1-Specificity, y = Sensitivity), ROC_dat, color=qpal[2], size=1) +
                geom_abline(slope=1, intercept=0, linetype=4, color=qpal[1])
    }, height=300, width=350)
    
    output$auc <- renderText({
        k_seq <- seq(-8,8,by=.05)  
        alpha <- input$alpha
        ROC_dat  <- data.frame(
            Sensitivity = pnorm(k_seq, (1-alpha)*4, lower.tail = F),
            Specificity = pnorm(k_seq, (alpha-1)*4, lower.tail = T))
        
        x <- 1- ROC_dat$Specificity
        y <- ROC_dat$Sensitivity
        id <- order(x)
        AUC <- sum(diff(x[id])*rollmean(y[id],2))
       
        paste("Area under the Curve (AUC):", round(AUC,2))
        
    })
    
    
    
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)