

library(shiny)
library(leaflet)
library(htmltools)
library(metricsgraphics)

source("load.R")

shinyServer(function(input, output, session) {

  donnees <- reactive({
    tmp <- RFDUiris %>% 
      filter(année %in% input$year) %>% 
      spread(variable, value) %>%
      as.data.frame
    
    iris@data[, c("RFUCET", "RFUCGI", "RFUCIQ", "RFUCMO", "RFUCQ2", "RFUCRD")] <- tmp[match(iris@data$DCOMIRIS, tmp$CodeIris), c("RFUCET", "RFUCGI", "RFUCIQ", "RFUCMO", "RFUCQ2", "RFUCRD")]
    iris
  })
  
  
    output$carte <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(noWrap = TRUE)) %>%
        addPolygons(data = iris, stroke = FALSE, layerId = paste0("iris", row.names(iris)))  %>%
        addPolygons(data = communes, stroke = TRUE, color = "blue", weight = 2, opacity = 0.3, fill = FALSE, layerId = paste0("communes", row.names(communes)))
    })

    legendes_small <- c("RFUCQ2" = "Revenu médian", "RFUCIQ" = "Écart inter-quartile", "RFUCRD" = "Rapport inter-décile (log)", "RFUCET" = "Écart-type", "RFUCGI" = "Coefficient de Gini")
    
      
  observe({
    if (is.null(input$year) | is.null(input$variable))
      return()
    isolate({
      pal <- colorNumeric("Reds", RFDUiris[RFDUiris$variable %in% input$variable, "value"])
      
      data <- donnees()
      leafletProxy("carte") %>%
        removeShape(layerId = paste0("iris", row.names(iris))) %>%
        addPolygons(data = data, stroke = FALSE, color = pal(data@data[,input$variable]), fillOpacity = 0.8, popup = ~paste0("Commune : ", NOM_COM, "<BR>IRIS : ", NOM_IRIS, "<BR>", legendes_small[input$variable], " : ", round(data@data[,input$variable], 2)), layerId = paste0("iris", row.names(iris)))
      })
  })
  
  legendes <- c("RFUCQ2" = "Revenu médian<BR>par unité de consommation, année ",
                "RFUCIQ" = "Écart interquartile des revenus<BR>par unité de consommation, année ",
                "RFUCRD" = "Log du rapport interdécile des revenus<BR>par unité de consommation, année ",
                "RFUCET" = "Écart-type des revenus<BR>par unité de consommation, année ",
                "RFUCGI" = "Coefficient de Gini des revenus<BR>par unité de consommation, année ") 
  
  observe({
    pal <- colorNumeric("Reds", RFDUiris[RFDUiris$variable %in% input$variable, "value"])
    
    leafletProxy("carte") %>%
      clearControls() %>%
      addLegend("bottomleft", pal = pal, values = RFDUiris[RFDUiris$variable %in% input$variable,][["value"]],
                title = paste0(legendes[input$variable], input$year),
                opacity = 1)
    
  })
  
  
  observe({
    output$scatterplot <- renderMetricsgraphics({
      donnees()@data %>%
        mjs_plot(x=RFUCQ2, y = input$variable) %>%
        mjs_point() %>%
        mjs_labs("Revenu médian", as.vector(legendes_small[input$variable]))
    })
  })
  
#   observe({
#     output$histogram <- renderMetricsgraphics({
#       donnees()@data %>%
#         mjs_plot() %>%
#         mjs_histogram()
#     })
#   })
  

})


