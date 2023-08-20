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
      sliderInput(inputId = "xsp",
                  label = "x spacing (west-east), metres",
                  min=20, max=200, value= 80),
      sliderInput(inputId = "ysp",
                  label = "y spacing (south-north), metres",
                  min=20, max=200, value= 80)
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
                 cellsize=c(input$xsp,input$ysp),
                 what="centers")
    })
  output$lcmap <- renderPlot(
    ggplot(lcpoly) +
      geom_sf(data=lcpoly, bg="#e0c08040", col="#806020") +
      geom_sf(data=st_intersection(sampgrid(), lcpoly),
              shape=3, col="darkgoldenrod", size=3, stroke=2) +
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
