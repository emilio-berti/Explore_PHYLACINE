library(shiny)
library(raster)
library(sf)
library(DT)


phy <- read.csv("https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Traits/Trait_data.csv", sep = ",")
Species <- phy$Binomial.1.2

url <- "https://github.com/MegaPast2Future/PHYLACINE_1.2/blob/master/Data/Ranges/"

w <- raster("continents.tif")
w[!is.na(w)] <- 1

ui <- fluidPage(
  column(3, offset = 4, 
         titlePanel("",
                    "Explore PHYLACINE")),
  br(),
  fluidRow(
    h3("Geographic range"),
    column(2,
           selectInput("species",
                       "Species",
                       choices = Species
           )
    )),
  fluidRow(
    column(2,
           checkboxInput("plot_cu",
                         "Plot current range", value = TRUE)
    ),
    column(10,
           plotOutput("ranges")
    )
  ),
  fluidRow(
    h3("PHYLACINE Table"),
    column(2,
           textInput("family",
                     "Family",
                     value = "")),
    column(10,
           dataTableOutput('family_table'))
  )
)

server <- function(input, output) {
  species <- reactive(input$species)
  pn <- reactive({
    r <- file.path("PHYLACINE_1.2", "PHYLACINE_1.2-master", "Data", "Ranges",
                   "Present_natural", paste0(species(), ".tif"))
    r <- raster(r)
    r[r == 0] <- NA
    r
  })
  cu <- reactive({
    r <- file.path("PHYLACINE_1.2", "PHYLACINE_1.2-master", "Data", "Ranges",
                   "Current", paste0(species(), ".tif"))
    r <- raster(r)
    r[r == 0] <- NA
    r
  })
  output$ranges <- renderPlot({
    par(mar = c(0, 0, 0, 0))
    plot(w, col = "grey80", box = FALSE, axes = FALSE, legend = FALSE)
    plot(pn(), col = "steelblue", add = TRUE, legend = FALSE)
    if (input$plot_cu) {
      plot(cu(), col = "gold2", add = TRUE, legend = FALSE)
      legend(-1.5 * 10^7, -3 * 10^6, 
             legend = c("Present-natural", "Current"), 
             fill = c("steelblue", "gold2"),
             border = c("steelblue", "gold2"),
             box.lwd = 0, cex = 1.5)
    } else {
    legend(-1.5 * 10^7, -3 * 10^6, 
           legend = c("Present-natural"), 
           fill = c("steelblue"),
           border = c("steelblue"),
           box.lwd = 0, cex = 1.5)
    }
  })
  fam_tab <- reactive({
    phy[phy$Family.1.2 == input$family, c("Order.1.2", "Family.1.2", "Binomial.1.2", "Mass.g", "IUCN.Status.1.2")]
  })
  output$family_table <- renderDataTable(fam_tab())
}

# Run the application 
shinyApp(ui = ui, server = server)

