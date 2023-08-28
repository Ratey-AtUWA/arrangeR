# check packages
packages <- c("shiny", "sf", "ggplot2","stars")
install.packages(setdiff(packages, rownames(installed.packages())))

# load packages
library(shiny)
library(sf)
library(ggplot2)
library(stars)

lcb <- read.csv("https://github.com/Ratey-AtUWA/arrangeR/raw/main/arrange2/LC_SampledArea.csv", stringsAsFactors = TRUE)
lcw <- read.csv("https://github.com/Ratey-AtUWA/arrangeR/raw/main/arrange2/LC_WaterEdge.csv", stringsAsFactors = TRUE)
lake <- st_polygon(list(as.matrix(lcw))) |> st_sfc(crs=st_crs(32750))
lcpoly <- st_polygon(list(as.matrix(lcb),as.matrix(lcw))) |>
  st_sfc(crs=st_crs(32750))

# ui.R ----
ui <- fluidPage(
  titlePanel("Sampling grid illustration map"),
  h3("Lake Claremont"),
  sidebarLayout(
    sidebarPanel(
      textOutput("howto"),
      textOutput("spacer1"),
      sliderInput(inputId = "xysp",
                  label = "Grid spacing (metres)",
                  min=30, max=130, value= 80),
      sliderInput(inputId = "off0",
                  label = "Offset from origin (metres)",
                  min=0, max=50, value= 0.001),
      selectInput(inputId="ifsq",
                  label="Choose your grid geometry",
                  choices=c("Rectangular"="TRUE", "Triangular (hexagonal)"="FALSE"), selected="TRUE"),
      sliderInput(inputId = "rand0",
                  label = "Amount of randomness (metres)",
                  min=0, max=25, value= 0),
      textOutput(outputId = "GridInfo")
    ),
    mainPanel(
       plotOutput("lcmap", height = "720px"),
       textOutput("spacer2"),
       textOutput("tcaption"),
       dataTableOutput("samples")
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
  output$howto <- renderText({
    paste("Adjust the values of grid parameters below","and see what happens!")
  })
  output$spacer1 <- renderText({
    paste("\u00A0","\u00A0","\u00A0")
  })
  output$GridInfo <- renderText({
    paste("Current map shows",NROW(st_intersection(sampgrid(), lcpoly)),"sampling points.")
  })
  output$spacer2 <- renderText({
    paste("\u00A0","\u00A0","\u00A0")
  })
  output$tcaption <- renderText({
    paste("⊞ Data Table with",NROW(st_intersection(sampgrid(),lcpoly)),"
          points; use the box below to choose how many rows are displayed.")
  })
  output$samples <- renderDataTable(
    round(st_coordinates(st_intersection(sampgrid(), lcpoly)),1),
    options = list(pageLength = 10)
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
