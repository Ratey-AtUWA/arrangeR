library(shiny)
library(sf)

ui <- fluidPage(
  # Application title
  titlePanel("Lake Claremont sampling map"),

  sidebarLayout(
    sidebarPanel(
      # inputs
      fileInput("reserve", "Reserve boundary file", accept = ".csv"),
      numericInput("n", "Show this many rows of coordinates:", value = 5, min = 1, step = 1),
      tableOutput("head")
    ),
    mainPanel(
      plotOutput("lcmap", width="70%", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$reserve)
    vroom::vroom(input$reserve$datapath, delim = ",")
  })
  output$lcmap <- renderPlot({
    par(mar=c(3,3,.5,.5), mgp=c(1.7,0.3,0),tcl=0.2)
    plot(data(), asp=1, type="l", lwd=2, col="#ffa08080",
         xlab = expression(bold("Easting, UTM Zone 50S")),
         ylab = expression(bold("Northing, UTM Zone 50S")),
         cex.lab=1.4)
  })
  output$head <- renderTable({
    head(data(), input$n)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
