library(caret)
library(stats)
library(shiny)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap) 
library(gridExtra)

mdl = get(load(file ="./models/extended_model.rda", .GlobalEnv))
data = read.csv("data/s_data.csv")
suppressWarnings({load("data/map/beijing_map.RData", verbose = T)})

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("House price prediction App"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("square", "Square Area of plot", 0, 650, 1,step = 1),
            sliderInput("floor", "Flooring", 0, 70, 1,step = 1),
    
            sliderInput("living", "Living rooms", 0, 9, 1,step = 1),
            sliderInput("drawing", "Drawing rooms", 0, 6, 1,step = 1),
            sliderInput("kitchen", "Kitchens", 0, 4, 1,step = 1),
            sliderInput("bathroom", "Bathrooms", 0, 9, 1,step = 1),
            sliderInput("constructionTime", "Year constructed", 1950, 2020, 1,step = 1),
            
            selectInput("district", "Select district:",
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
            selectInput("buildType", "Select Building Type:",
                        c("Tower" = "Tower",
                          "Plate/Tower" = "Plate/Tower",
                          "Plate" = "Plate",
                          "Bunglow" = "Bunglow"
                        )),
            selectInput("buildStruct", "Select Building Structure:",
                        c("Steel/Concrete" = "Steel/Concrete",
                          "Mixed" = "Mixed",
                          "Brick/Concrete" = "Brick/Concrete",
                          "Brick/Wood" = "Brick/Wood",
                          "Steel" = "Steel",
                          "Unavailable" = "Unavailable"
                        )),
            selectInput("renovateCond", "Renovation condition:",
                        c("Hardcover" = "Hardcover",
                          "Other" = "Other",
                          "Simplicit" = "Simplicit",
                          "Rough" = "Rough"
                        )),
            radioButtons("elevator", "Elevator present:",
                         c("Yes" = "Has_Elevator",
                           "No" = "No_elevator")
            ),
            radioButtons("subway", "Subway nearby:",
                         c("Yes" = "Has_Subway",
                           "No" = "No_Subway")
            ),
            actionButton("Run_model", "Submit")
            ),
        mainPanel(
            #tableOutput("table"),
            tabsetPanel(type = "tabs",
                        tabPanel("Districts", plotOutput("plot1")),
                        tabPanel("Summary", tableOutput("summary")),
                        tabPanel("Learning Model", tableOutput("table"))
            )
        )
    ),
)
server <- function(input, output) {
    
    createUserDf = reactive({
        # this is how you fetch the input variables from ui component
        
        square = input$square
        floor = input$floor
        livingRoom = input$living
        drawingRoom = input$drawing
        kitchen = input$kitchen
        bathRoom = input$bathroom
        constructionTime = input$constructionTime
        subway = input$subway
        district = input$district
        elevator = input$elevator
        buildingType = input$buildType
        buildingStructure = input$buildStruct
        renovationCondition = input$renovateCond
        
        m = cbind(square, livingRoom, drawingRoom, kitchen, bathRoom, floor,
                   buildingType, constructionTime, renovationCondition,
                   buildingStructure, elevator, subway, district)
        df = as.data.frame(m)
        df = mutate(df, square = as.numeric(square),
                    livingRoom = as.numeric(livingRoom),
                    kitchen = as.numeric(kitchen),
                    bathRoom = as.numeric(bathRoom),
                    drawingRoom = as.numeric(drawingRoom),
                    constructionTime = as.numeric(constructionTime),
                    floor = as.numeric(floor)
                    )
        df
    })

    
    output$plot1 = renderPlot({
        map1 = beijing + geom_point(data = data, aes(x = Lng, y = Lat, colour = factor(district)), alpha = 0.4, na.rm = T)
        bar1 = ggplot(data, 
                      aes(y = price, x = factor(district), fill = factor(district))
        ) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        grid.arrange(map1, bar1, nrow = 1)
    }) 
    output$table = renderPrint({
        cat("<h1>Predicted value:</h1>")
        cat("<h3>The price is estimated to be: ",pred(),"</h3>")
        cat("<h5>Using the parameters you can find the predicted cost of house 
            the selected region<h5>")
    })
    output$summary = renderTable({
        print({createUserDf()})
    })
    pred = eventReactive(input$Run_model, {
        predict(mdl, newdata = createUserDf())
    })
}
shinyApp(ui = ui, server = server)
