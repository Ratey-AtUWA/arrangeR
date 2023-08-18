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
    ),
    mainPanel(
      plotOutput("lcmap", width="70%", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$reserve)
    st_read(input$reserve$datapath) |>
      st_as_sf(coords=c("E","N"), crs=st_crs(32750))
  })
  output$lcmap <- renderPlot({
    par(mar=c(3,3,.5,.5), mgp=c(1.7,0.3,0),tcl=0.2, font.lab=2)
    plot(data(), type="l", lwd=2, col="#ffa08080",
         xlab = "Easting, UTM Zone 50S", ylab = "Northing, UTM Zone 50S",
         cex.lab=1.4, axes=T)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
