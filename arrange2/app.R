# load packages
library(shiny)
library(ggplot2)
library(sf)

lcb <- read.csv("C:/Users/00028958/OneDrive - The University of Western Australia/R Projects/arrangeR/arrange2/LC_SampledArea.csv", stringsAsFactors = TRUE)
lcw <- read.csv("C:/Users/00028958/OneDrive - The University of Western Australia/R Projects/arrangeR/arrange2/LC_WaterEdge.csv", stringsAsFactors = TRUE)
lcpoly <- st_polygon(list(as.matrix(lcb),as.matrix(lcw))) |>
  st_sfc(crs=st_crs(32750))

# ui.R ----
ui <- fluidPage(
  titlePanel("sampling map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "xsp",
                  label = "x spacing (west-east), metres",
                  min=20, max=200, value= 50),
      sliderInput(inputId = "ysp",
                  label = "y spacing (south-north), metres",
                  min=20, max=200, value= 50)
    ),
    mainPanel(
      plotOutput("lcmap", height = "500px")
    )
  )
)


# server.R ----
server <- function(input, output) {
  output$lcmap <- renderPlot(
    ggplot(lcpoly) +
      geom_sf(data=lcpoly) +
      theme_bw() +
      theme(axis.title = element_text(size=18, face = "bold"),
            axis.text = element_text(size=16))
    )
}

# Run the app ----
shinyApp(ui = ui, server = server)
