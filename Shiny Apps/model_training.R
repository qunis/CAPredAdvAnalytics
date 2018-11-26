# Qunis Palette
qpal <- c("#009fe3","#023e84", "#2bad70", "#e8423b", "#1d1d1b", "#7c7c7b", "#d0d0d0", "#ffffff" )

library(tidyverse)

set.seed(20)

orders <- read_csv2('../data/customer/orders.csv')
customers <- read_csv2('../data/customer/customers.csv')


# Errechnung der RFM Metriken 
# Recency (wie lange her), Frequency (wie oft), Market Value
# --------------------------------------------------------------------------------
RFM <- orders %>% group_by(CustomerID) %>% summarise(
    Recency = as.integer(Sys.Date() - max(Date)),
    Frequency = n(),
    Mon.Value = sum(OrderTotal)
)


# Cluster auf die RFM Tabelle
cl <- kmeans(RFM %>% select(-CustomerID), 2) # nur 2 Cluster -> High Potential, Low Potential Kunden

# Join die Clusternummer [1 oder 2] zum RFM Frame
RFM_clust <- mutate(RFM, clust = cl$cluster %>% factor(labels = c('High', 'Low')))

dat <- customers %>% inner_join(RFM_clust) %>% 
    select(-Recency, -Mon.Value, -Frequency)


confusion <- function(k, dat){
    confusionMatrix(ifelse(dat$Age > k, "High", "Low") %>% as.factor, dat$clust)
}
    

ui <- fluidPage(

  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
    #     radioButtons("dataset",
    #                             "Dataset",
    #                             choices=list("Training", "Test")
    # ),
        sliderInput("k", "Threshhold",
                    min=0,
                    max=100,
                    value=20,
                    step=1),
        verbatimTextOutput("confmat"),
        tableOutput("acc")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic 
server <- function(input, output) {
    
    #trigger <- reactiveVal("Training")
    
    # observeEvent(input$dataset,{
    #     trigger(input$dataset)
    #     
    # })

  output$plot<- renderPlot({
      # if(trigger()=="Training"){
      #     dat <- trainDat
      # }else{
      #     dat <- testDat
      # }
      k <- input$k
      lim <- c(0,100)
      
      dat %>% #filter(var!=0) %>% 
      ggplot() + geom_density(aes(x=Age, fill=clust), alpha = .5)+ 
          geom_vline(xintercept = k, color=qpal[4], linetype=2, size=1)  +
          coord_cartesian(xlim=lim)
  }
  #, height=300, width=500
  )
 
  output$confmat <- renderPrint({
      # if(trigger()=="Training"){
      #     dat <- trainDat
      # }else{
      #     dat <- testDat
      # }
      k <- input$k
      ans <- confusion(k, dat)
      ans$table
  })
  
  output$acc <- renderTable({
      # if(trigger()=="Training"){
      #     dat <- trainDat
      # }else{
      #     dat <- testDat
      # }
      k <- input$k
      ans <- confusion(k, dat)
      data.frame(accuracy=ans$overall[1],
                 recall = ans$byClass[1],
                 fallout = 1 - ans$byClass[2])
  }, digits=3)

}

# Run the application 
shinyApp(ui = ui, server = server)