library(shiny)
library(leaflet)

census_by_race <- source("~/stop-question-frisk/raw_data/census_race_data.RData")



ui <- fluidPage(
# Add a sidebar layout to the application
  sidebarLayout(
    
# Add a sidebar panel around the text and inputs
    sidebarPanel(
      
      h3("Predict Stop and Frisk"),
      radioButtons("race", h5("Race"),
                   choices = list("Black" = 1, "White" = 2, "Hispanic" = 3),selected = 1),
      numericInput("age", "Age", 90)
      
      # selectInput("intensity", h5("Select your choice of intensity"), 
      #           choices = list("Hands" = 1, "Handcuffed" = 2,"Pushed to Wall" = 3,
      #                          "Pushed to ground" = 4, "Weapon Drawn" = 5, "Weapon Pointed" = 6,
      #                          "Peppersprayed" = 7, "Baton" = 8), selected = 1)
                ),
  
# Add a main panel around the plot and table
          mainPanel(
             plotOutput("plot"),
             tableOutput("table"), leafletOutput("leafletmap", height = "500"))
              )
              )
            


# Define the server logic
server <- function(input, output) {

  #Working leaflet map 
  output$leafletmap <- renderLeaflet(leafletmaphigh <- leaflet() %>% 
                  addProviderTiles("CartoDB.Positron") %>%
                  
                  addPolygons(data=joint_prop_high_white,
                              fillColor = ~mypal3(joint_prop_high_white$prob),
                              weight = 2,
                              opacity = 1,
                              fillOpacity = 0.7,
                              popup = mypopup3, group="High-White") %>%
                  addPolygons(data=joint_prop_high_black,
                              fillColor = ~mypal4(joint_prop_high_black$prob),
                              weight = 2,
                              opacity = 1,
                              fillOpacity = 0.7,
                              popup = mypopup4, group="High-Black") %>%
                  addLegend(position = "topleft", 
                            pal = mypal3, 
                            values = prob_high_intensity_given_race$prob) %>%
                  addLayersControl(c("High-White", "High-Black"),
                                   options = layersControlOptions(collapsed = FALSE))
                
  )
  
  
  #selected data reactive function

  

    

 
}

# Run the application
shinyApp(ui = ui, server = server)
