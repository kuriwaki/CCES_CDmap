#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)


library(leaflet)
library(donnermap)
library(ggthemes)
library(ggplot2)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$cdmap <- renderPlot({
      cd_race <- left_join(cd_shp, race_by_cd, by = "cd") %>%
        filter(race == input$race)

        ggplot(cd_race, aes(fill = frac)) +
          geom_sf() +
          theme_map() +
          labs(fill = input$race)
    })

})
