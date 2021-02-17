
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Juan's Mosquito App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("numbers",
                        "Pick a number",
                        min=1,
                        max=20,
                         value= 15),
            selectInput("region",
                    "Pick a region",
                    choices = sort(unique(df$region))),
            checkboxGroupInput("species",
                               "Select mosquito species",
                               choices = sort(unique(df$species)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
           plotOutput("juan")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   source("Res_trans.R") 
    output$juan = renderPlotly({
     plot_ly(data = pd %>%
               filter(!is.na(indoor_biting))%>%
               filter(region==input$region)%>%
               filter(species %in% input$species),
             
              x = ~key, y = ~hbi, z = ~indoor_biting,
              type='scatter3d', mode='markers', color = ~region)
    })
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
