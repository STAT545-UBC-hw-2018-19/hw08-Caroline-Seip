#Load required packages
library(shiny) 
library(ggplot2)
library(dplyr)
library(shinythemes)

#Input data file
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

#Create user interface
ui <- fluidPage( theme = shinytheme("yeti"),
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      #Add slider bar for selecting price
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      #Add radio buttons for selecting product type
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      
      uiOutput("countryOutput")
    ),
    mainPanel(
      #Plot results
      plotOutput("coolplot"),
      br(), br(),
      #Add interactive table of results
      DT::dataTableOutput("results"),
      #Add BCL image at bottom of page
      img(src = "bcl_image.png")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- DT::renderDataTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)