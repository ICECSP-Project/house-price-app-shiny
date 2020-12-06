#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(caret)
library(stats)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("House price prediction App"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("square", "Square Area of plot", 0, 650, 1,step = 1),
            sliderInput("constructionTime", "construction Time of building", 1950, 2020, 1,step = 1),
            radioButtons("subway", "Subway nearby:",
                         c("Yes" = "Has_Subway",
                           "No" = "No_Subway")
                         ),
            radioButtons("district", "Select district:",
                         c("Chao Yang" = "ChaoYang",
                           "Chang Ping" = "ChangPing",
                           "Dong Cheng" = "DongCheng",
                           "Men Tou Gou" = "MenTouGou",
                           "Xi Cheng" = "XiCheng",
                           "Feng Tai" = "FengTai",
                           "Hai Dian" = "HaiDian",
                           "Fa Xing" = "FaXing",
                           "Fang Shang" = "FangShang",
                           "Da Xing" = "DaXing",
                           "ShiJing Shan" = "ShiJingShan",
                           "Shun Yi" = "ShunYi",
                           "Tong Zhou" = "TongZhou"
                           )),
    
            ),
        mainPanel(
            tableOutput("print")
        )
    ),
    
)
server <- function(input, output) {
    
    output$print = renderPrint({
        mdl = load(file ="./models/final_model.rda", .GlobalEnv)
        square = as.numeric(input$square)
        constructionTime = as.numeric(input$constructionTime)
        subway = as.character(input$subway)
        district = as.character(input$district)
        data = data.frame(square,constructionTime,subway,district)
        pred = predict(get(mdl), newdata = data)
        cat("The price is estimated to be: ",pred)
    })
}
shinyApp(ui = ui, server = server)
