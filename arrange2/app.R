# load packages
library(shiny)
library(ggplot2)
library(sf)

lcb <- read.csv("LC_SampledArea.csv", stringsAsFactors = TRUE)
lcw <- read.csv("LC_WaterEdge.csv", stringsAsFactors = TRUE)
lake <- st_polygon(list(as.matrix(lcw))) |> st_sfc(crs=st_crs(32750))
lcpoly <- st_polygon(list(as.matrix(lcb),as.matrix(lcw))) |>
  st_sfc(crs=st_crs(32750))

# ui.R ----
ui <- fluidPage(
  titlePanel("Sampling grid illustration map"),
  h3("Lake Claremont"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "xysp",
                  label = "Grid spacing (metres)",
                  min=30, max=200, value= 80),
      sliderInput(inputId = "off0",
                  label = "Offset from origin (metres)",
                  min=0, max=50, value= 0.001),
      selectInput(inputId="ifsq",
                  label="Do you want a rectangular grid?\n(triangular otherwise)",
                  choices=c("Yes"="TRUE", "No"="FALSE"), selected="TRUE"),
      sliderInput(inputId = "rand0",
                  label = "Amount of randomness (metres)",
                  min=0, max=25, value= 0),
      textOutput(outputId = "GridInfo")
    ),
    mainPanel(
      tabsetPanel(
       plotOutput("lcmap", height = "640px"),
       dataTableOutput("samples")
       )
      )
    )
  )


# server.R ----
server <- function(input, output) {
  sampgrid <- reactive({
    st_make_grid(lcpoly,
                 cellsize=c(input$xysp, input$xysp),
                 offset = st_bbox(lcpoly)[c("xmin", "ymin")]+input$off0,
                 what="centers", square=as.logical(input$ifsq))
  })
  output$lcmap <- renderPlot(
    ggplot(lcpoly) +
      geom_sf(data=lake, bg="#40c0ff40", col="#40c0ff40") +
      geom_sf(data=lcpoly, bg="#e0e08040", col="#808020") +
      geom_sf(data=st_as_sf(st_jitter(st_intersection(sampgrid(), lcpoly),
                                      amount=input$rand0)),
              shape=10, col="blue3", size=3, stroke=1) +
      xlab(label = "Easting (m, UTM Zone 50S)") +
      ylab(label = "Northing (m, UTM Zone 50S)") +
      geom_text(aes(x=384400, y=6461800, label="Lake\nClaremont",
                    fontface = "italic"), col="royalblue", size=6) +
      theme_bw() +
      theme(axis.title = element_text(size=18, face = "bold"),
            axis.text = element_text(size=14),
            axis.ticks.length = unit(-0.2, "cm")) +
      coord_sf(datum = st_crs(32750))
  )
  output$GridInfo <- renderText({
    paste("Showing",NROW(st_intersection(sampgrid(), lcpoly)),"points")
  })
  output$samples <- renderDataTable(
    st_coordinates(st_intersection(sampgrid(), lcpoly)),
    options = list(pageLength = 10)
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
