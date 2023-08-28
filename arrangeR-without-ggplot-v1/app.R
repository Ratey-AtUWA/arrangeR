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
  titlePanel("Sample planning map: Lake Claremont"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "xysp",
                  label = "Grid spacing (metres)",
                  min=30, max=200, value= 80),
      sliderInput(inputId = "xyadj",
                  label = "Offset from origin (metres)",
                  min=0, max=50, value= 0.001),
      selectInput(inputId="ifsq",
                  label="Do you want a rectangular grid?\n(alternative = triangular)",
                  choices=c("Yes"="TRUE", "No"="FALSE"), selected="TRUE"),
      sliderInput(inputId = "xyjit",
                  label = "Amount of randomness",
                  min=0, max=0.5, value=0)
    ),
    mainPanel(
      plotOutput("lcmap", height = "640px")
    )
  )
)


# server.R ----
server <- function(input, output) {
    sampgrid <- reactive({
      st_as_sf(sg1, coords=c("X","Y"), crs=st_crs(32750))
    })
    output$lcmap <- renderPlot({
      par(mar=c(3.5,3.5,0.5,0.5), mgp=c(1.7,0.3,0), tcl=0.25, font.lab=2)
      plot(lcpoly, col="#F0E68C80", border="#e0d67C", bg="#00000000", axes=T,
         xlab="Easting (m, UTM Zone 50S",
         ylab="Northing (m, UTM Zone 50S")
      plot(lake, col="#e0e0ff", border="#00000000", axes=T, add = T)
      grid()
      text(384400, 6461850, labels="Lake\nClaremont",
         font = 3, col="royalblue", cex=1.2)
      plot(st_intersection(sampgrid, lcpoly), add=T, pch=3, cex=0.8, col="azure3")
      plot(st_intersection(st_jitter(sampgrid, amount = xysp*xyjit), lcpoly),
         add=T, pch=10, col="blue3")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
