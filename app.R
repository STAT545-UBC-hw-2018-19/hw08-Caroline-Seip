#Load required packages, use install.packages() first if not installed already
library(shiny) 
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)
library(rsconnect)

#Input data file
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

#Create user interface for app that draws a histogram and interactive data table
ui <- fluidPage( #fluid means it will adapt to the screen you are using
  theme = shinytheme("yeti"), #choose a theme for the app
  titlePanel("BC Liquor Store prices"), #edit the title
  sidebarLayout( #edit the sidebar
    sidebarPanel(
      #Add slider bar for selecting price
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      #Add radio buttons for selecting product type
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      
      uiOutput("countryOutput")
      #specify output
    ),
    mainPanel( #edit main panel
      #Plot results
      plotOutput("coolplot"),
      br(), br(), #line break
      #Add interactive table of results
      DT::dataTableOutput("results"),
      #Add BCL image at bottom of page
      img(src = "bcl_image.png")
    )
  )
)

#Define the server logic to create outputs
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
    
    #Build histogram
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
  
  #Build interactive data table
  output$results <- DT::renderDataTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)