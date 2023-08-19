library(shiny)
library(sf)

ui <- fluidPage(
  # Application title
  titlePanel("Lake Claremont sampling map"),

  sidebarLayout(
    sidebarPanel(
      # inputs
      fileInput("reserve", "Reserve boundary file", accept = ".csv"),
      fileInput("lake", "Lake boundary file", accept = ".csv"),
      numericInput("xsp", "X (W-E) grid coordinate spacing:",
                   value = 50, min = 20, step = 5),
      numericInput("ysp", "Y (S-N) grid coordinate spacing:",
                   value = 50, min = 20, step = 5),
    ),
    mainPanel(
      plotOutput("lcmap", width="70%", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  data1 <- reactive({
    req(input$reserve)
    read.csv(input$reserve$datapath) |>
      as.matrix()
  })
  data2 <- reactive({
    req(input$lake)
    read.csv(input$lake$datapath) |>
      as.matrix()
  })
  data0 <- reactive({
    st_polygon(list(data1(),data2())) |> st_sfc(crs=st_crs(32750))
    })
  output$lcmap <- renderPlot({
    par(mar=c(3,3,.5,.5), mgp=c(1.7,0.3,0),tcl=0.2, font.lab=2)
    plot(data0(), type="l", lwd=1, col="#d0c00040",
         xlab = "Easting, UTM Zone 50S", ylab = "Northing, UTM Zone 50S",
         cex.lab=1.4, axes=T)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
