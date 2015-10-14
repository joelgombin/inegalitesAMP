

library(shiny)
library(shinythemes)
library(leaflet)
library(htmltools)
library(metricsgraphics)


shinyUI(navbarPage(
  theme = shinytheme("readable"),
  div(class = "outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
#        includeScript("gomap.js")
      ),
  
      title = "Les inégalités dans la métropole Aix-Marseille-Provence",

      leafletOutput("carte", height = "100%", width = "100%"),
      
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    p("Cette application permet d'explorer l'évolution et la géographie des inégalités de revenus au sein de la métropole Aix-Marseille-Provence de 2001 à 2011. "),
                    
                    sliderInput("year", "Année", min = 2001, max = 2011, value = 2011, step = 1, sep = ""),
                    selectInput("variable", "Indicateur d'inégalités", c("Revenu médian" = "RFUCQ2", "Écart inter-quartile" = "RFUCIQ", "Rapport inter-décile" = "RFUCRD", "Écart-type" = "RFUCET", "Coefficient de Gini" = "RFUCGI")),
    
                    metricsgraphicsOutput("scatterplot"),
                #    metricsgraphicsOutput("histogram"),
                    p('Réalisation : Joël Gombin. Sources : dispositif "revenus fiscaux localisés des ménages" (INSEE/DGFiP), IGN. ')
      )

  )

))
