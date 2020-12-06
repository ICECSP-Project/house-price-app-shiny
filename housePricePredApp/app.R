#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("House price prediction App"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)
server <- function(input, output) {
    output$distPlot <- renderPlot({
        mdl = load("./models/final_model.rds", .GlobalEnv)
        square = input$square
        constructionTime = input$constructionTime
        subway = input$subway
        district = input$district
        data = data.frame(square,constructionTime,subway,district)
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}
shinyApp(ui = ui, server = server)
