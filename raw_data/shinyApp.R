library(shiny)
library(leaflet)

census_race <- source("~/stop-question-frisk/raw_data/census_race_data.RData")



ui <- fluidPage(
# Add a sidebar layout to the application
  sidebarLayout(
    
# Add a sidebar panel around the text and inputs
    sidebarPanel(
      
      h3("Predict Stop and Frisk"),
      radioButtons("race", h5("Race"),
                   choices = list("Black" = 1, "White" = 2, "Hispanic" = 3),selected = 1),
      numericInput("age", "Age", 90),
      
      # selectInput("intensity", h5("Select your choice of intensity"), 
      #           choices = list("Hands" = 1, "Handcuffed" = 2,"Pushed to Wall" = 3,
      #                          "Pushed to ground" = 4, "Weapon Drawn" = 5, "Weapon Pointed" = 6,
      #                          "Peppersprayed" = 7, "Baton" = 8), selected = 1)
                ),
  
# Add a main panel around the plot and table
          mainPanel(
             plotOutput("plot"),
             tableOutput("table"), leafletOutput(mymap)
              )
              )
            )


# Define the server logic
server <- function(input, output) {

  output$mymap <- renderLeaflet({
    
    
    race <- switch(input$race, 
                   "White" = counties$white,
                   "Black" = counties$black,
                   "Hispanic" = counties$hispanic)
    
    age <- switch(input$var)
    
    
    predict(data, input$race, input$age)
    
   
  })
 
}

# Run the application
shinyApp(ui = ui, server = server)
