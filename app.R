# The web application has been inspired 
# by examples from https://rstudio.github.io/leaflet

library(shiny)
library(leaflet)
#setwd('C:/Users/202006784/Documents/BIGDATA/MBDFME')
#load data, remove NaN and subset location columns
df <- na.omit(read.csv('Melbourne_housing_FULL.csv'))
loc<-data.frame(df$Longtitude,df$Lattitude)
colnames(loc) <-c('longitude', 'latitude')

#beatCol <- colorFactor(palette = 'RdYlGn', df$Price)
PriceCols <- colorNumeric(c('Green','Red'), domain = df$Price)

# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()



ui <- fluidPage(
  titlePanel('Melbourne Housing Market'),
  leafletOutput("myMap", width  = "100%", height = 550),
  p(),
  actionButton("recalc", "Resample")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    #plotting points for the whole df is cumbersome and my laptop cannot deal
    #with it. The data is sampled so as to avoid overflow.
    loc[sample(1:8887, input$n_samples),] 
  }, ignoreNULL = FALSE)
  
  output$myMap <- renderLeaflet({
    # leaflet() %>%
    #   addProviderTiles(providers$Stamen.TonerLite,
    #                    options = providerTileOptions(noWrap = FALSE)
    #   ) %>%
    #   addMarkers(data = points(),clusterOptions = markerClusterOptions())
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = FALSE)) %>%
      addCircleMarkers(data = df[sample(1:8887,500),], lat = ~Lattitude, lng = ~Longtitude,
                       color = ~PriceCols(Price)) %>%
      addLegend(position = 'bottomright', pal = PriceCols, values = df$Price,
                title = 'e.g. Property price level',
                opacity = 1)
  })
}

shinyApp(ui, server)
