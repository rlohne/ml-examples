
library(shiny)


ui <- fluidPage(

    
    titlePanel("Select Plottype"),

    
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId ="plottype", 
                label = "Select plot type",
                choices = c("Bar","Line")
            )
        ),

        
        mainPanel(
           plotOutput("Plot")
        )
    )
)


server <- function(input, output) {

    output$Plot <- renderPlot({
        if(input$plottype == "Bar"){
            ggplot(iris, aes(x= Species, y = Petal.Width)) +
                   geom_bar(stat = "identity")
        } else {
            ggplot(iris, aes(x= Species, y = Petal.Width)) +
                geom_line(stat = "identity")
        }
    })
}


shinyApp(ui = ui, server = server)
