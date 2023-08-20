# load packages
library(shiny)
library(ggplot2)
library(sf)

lcb <- read.csv("LC_SampledArea.csv", stringsAsFactors = TRUE)
lcw <- read.csv("LC_WaterEdge.csv", stringsAsFactors = TRUE)
lcpoly <- st_polygon(list(as.matrix(lcb),as.matrix(lcw))) |>
  st_sfc(crs=st_crs(32750))

# ui.R ----
ui <- fluidPage(
  titlePanel("Sample planning map: Lake Claremont"),
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
                  choices=c("Yes"="TRUE", "No"="FALSE"), selected="TRUE")
    ),
    mainPanel(
      plotOutput("lcmap", height = "640px")
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
      geom_sf(data=lcpoly, bg="#e0e08040", col="#808020") +
      geom_sf(data=st_intersection(sampgrid(), lcpoly),
              shape=10, col="tan4", size=3, stroke=1) +
      xlab(label = "Longitude") +
      ylab(label = "Latitude") +
      geom_text(aes(x=384400, y=6461800, label="Lake\nClaremont",
                    fontface = "italic"), col="royalblue", size=6) +
      theme_bw() +
      theme(axis.title = element_text(size=14, face = "bold"),
            axis.text = element_text(size=12)) +
      coord_sf(crs = st_crs(32750))
    )
}

# Run the app ----
shinyApp(ui = ui, server = server)
