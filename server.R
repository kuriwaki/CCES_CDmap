#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)

library(plotly)
library(donnermap)
library(ggthemes)
library(tidyverse)
library(sf)
library(ccesMRPprep)
library(glue)
library(scales)

# Define server logic required to draw
shinyServer(function(input, output) {

    output$cdmap <- renderPlotly({

      cd_race <- cd_shp %>%
        # add race data
        left_join(race_by_cd, by = "cd") %>%
        filter(race == input$race) %>%
        # add CD info
        left_join(cd_info_2018, by = "cd") %>%
        mutate(across(starts_with("pct_"), ~percent(.x, accuracy = 1))) %>%
        # Make label
        mutate(cd_lab = glue("<b>{cd}</b><br>({dailykos_name})<br>",
                             "{input$race}: {percent(frac, accuracy = 1)}<br>",
                             "McCain {pct_mccain}<br>Romney {pct_romney}<br>Trump {pct_trump}"))

        p <- ggplot(cd_race, aes(fill = frac, text = cd_lab)) +
          geom_sf(color = "white", size = 0.1) +
          scale_fill_viridis_c(option = "cividis") +
          theme_map() +
          labs(fill = input$race)

        ggplotly(p,
                 tooltip = "text",
                 width = (0.9*as.numeric(input$dimension[1])),
                 height = (0.9*as.numeric(input$dimension[2]))) %>%
          style(hoverlabel = list(bgcolor = "white"), hoveron = "fill") %>%
          layout(margin = list(b = 20), ##bottom margin in pixels
                 annotations =
                   list(x = 0.1, y = 0.2,
                        text = "Shiro Kuriwaki (https://github.com/kuriwaki/CCES_CDmap).<br>Map from Daniel Donner (http://dkel.ec/map).<br>Election Data from DailyKos, Race Data from 2018 ACS.",
                        showarrow = FALSE,
                        xref = 'paper', yref = 'paper',
                        align = 'left',
                        xanchor = 'left', yanchor = 'bottom',
                        xshift = 0, yshift = 0,
                        font = list(size = 8))
                 ) %>%
          plotly_build()
    })

})
