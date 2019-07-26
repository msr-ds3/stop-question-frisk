install.packages("shiny")
library(shiny)


library(shiny)

# Define UI for app 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Predict Stop and Frisk"),
  
  sidebarLayout(
    sidebarPanel(
      position = "left",
      
      #Select Race
      column(3,
             radioButtons("radio", h3("Race"),
                          choices = list("Black" = 1, "White" = 2),selected = 1)),
      #Choose Intensity
      column(3,
             selectInput("intensity", h3("Select your choice of intensity"), 
                         choices = list("Hands" = 1, "Handcuffed" = 2,"Pushed to Wall" = 3,
                         "Pushed to ground" = 4, "Weapon Drawn" = 5, "Weapon Pointed" = 6,
                         "Peppersprayed" = 7, "Baton" = 8), selected = 1)),
      
      #Intensity Slider
      column(3, 
             sliderInput("intensity", h3("Intensity"),
                         min = 0, max = 7, value = 7)
             
      )  
    )),
    mainPanel(
      "Main Panel"
    )
  )
  




server <- function(input, output) {
  
 
  output$distPlot <- renderPlot({
    
  
    
  })
  
}

shinyApp(ui = ui, server = server)

