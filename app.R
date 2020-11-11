library(shiny)
library(raster)
library(sf)
library(DT)

# used for data table
phy <- read.csv("PHYLACINE_1.2-master/Data/Traits/Trait_data.csv")
Species <- phy$Binomial.1.2
Family <- unique(phy$Family.1.2)

# used for ranges - not stable
url <- "/vsicurl/https://github.com/MegaPast2Future/PHYLACINE_1.2/blob/master/Data/Ranges/"
path <- file.path("PHYLACINE_1.2-master", "Data", "Ranges")

# base raster for plot - grey world, no antarctica
w <- raster("continents.tif")
w[!is.na(w)] <- 1

## define UI for shiny
ui <- fluidPage(
  column(3, offset = 4, 
         titlePanel("",
                    "Explore PHYLACINE")),
  br(),
  fluidRow(
    h3("Geographic range"),
    column(2,
           selectizeInput("species",  # selectizeInput makes writable and searchable dropdown menu
                          "Species",
                          choices = Species, 
                          selected = "Canis_lupus"  # default is wolf
           )
    )
  ),
  fluidRow(
    column(2,
           radioButtons("plot_range", 
                        "Range to Plot", 
                        choices=c("Current", "Present-natural", "Combined")
           )
    ),
    column(10,
           plotOutput("ranges")
    )
  ),
  fluidRow(
    h3("PHYLACINE Table"),
    # using uiOutput to have dynamic change of input to table
    # - matching family to the input species
    column(2,
           uiOutput("family_table_select")  # defined in server
    )
  ),
  br(),
  fluidRow(
    column(10,
           dataTableOutput('family_table')
    )
  )
)

## define server for shiny
server <- function(input, output) {
  species <- reactive(input$species)
  cu <- reactive({
    r.cu <- raster(paste0(path, "/Current/", species(), ".tif"))
    r.cu[r.cu == 0] <- NA
    r.cu
  })
  # get present-natural raster
  pn <- reactive({
    r.pn <- raster(paste0(path, "/Present_natural/", species(), ".tif"))
    r.pn[r.pn == 0] <- NA
    r.pn
  })
  # calculate overlay raster with 1=cu, 2=pn, 3=overlay, 0=NA
  ol <- reactive({
    r.cu <- cu()
    r.pn <- pn()
    r.pn[r.pn == 1] <- 2
    # overlay = current AND present-natural
    ol <- sum(r.cu, r.pn, na.rm = TRUE)
    ol[ol == 0] <- NA
    ol
  })
  output$ranges <- renderPlot({
    par(mar = c(0, 0, 0, 0))
    plot(w, col = "grey80", box = FALSE, axes = FALSE, legend = FALSE)  # base plot, no ranges
    # conditional plot
    if (input$plot_range == "Combined") {  # plot both ranges + overlay
      # order colors so they match with order of any raster
      var.order <- unique(values(ol()))[!is.na(unique(values(ol())))]
      var.colors <- c("gold2", "steelblue", "green3")[match(var.order, c(1,2,3))]
      # plot and legend
      plot(ol(), col = var.colors, add = TRUE, legend = FALSE)
      legend(-1.5 * 10^7, -3 * 10^6, 
             legend = c("Present-natural", "Current", "Overlap"), 
             fill = c("steelblue", "gold2", "green3"),
             border = c("steelblue", "gold2", "green3"),
             box.lwd = 0, cex = 1.5)
    } else if (input$plot_range == "Current") {  # plot only current range
      plot(cu(), col = "gold2", add = TRUE, legend = FALSE)
      legend(-1.5 * 10^7, -3 * 10^6, 
             legend = c("Current"), 
             fill = c("gold2"),
             border = c("gold2"),
             box.lwd = 0, cex = 1.5)
    } else if (input$plot_range == "Present-natural") {  # plot only present natural range
      plot(pn(), col = "steelblue", add = TRUE, legend = FALSE)
      legend(-1.5 * 10^7, -3 * 10^6, 
             legend = c("Present-natural"), 
             fill = c("steelblue"),
             border = c("steelblue"),
             box.lwd = 0, cex = 1.5)
    }
  })
  # using renderUI to make dynamic input (i.e. it changes the family based on
  # the input species)
  output$family_table_select <- renderUI({
    family_fits <- phy[phy$Binomial.1.2==species(), "Family.1.2"]
    selectizeInput("family",
                   "Family",
                   choices = Family,
                   selected = family_fits)
  })
  fam_tab <- reactive({
    phy[phy$Family.1.2 == input$family, c("Order.1.2", "Family.1.2", 
                                          "Binomial.1.2", "Mass.g", 
                                          "IUCN.Status.1.2")]
  })
  output$family_table <- renderDataTable(fam_tab())
}

# Run the application 
shinyApp(ui = ui, server = server)
