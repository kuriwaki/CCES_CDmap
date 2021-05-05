# This is the user-interface definition of a Shiny web application. You can

library(shiny)

library(plotly)
library(donnermap)
library(ggthemes)
library(tidyverse)
library(sf)
library(glue)
library(scales)
library(janitor)

# Base data
cd_race <- cd_shp %>%
  left_join(race_by_cd, by = "cd") %>%
  left_join(cd_info, by = "cd") %>%
  mutate(across(starts_with("pct_"), ~percent(.x, accuracy = 1)))
st_crs(cd_race) <- 3857

cc_rpv <- read_rds("data/by-cd-race2.rds")
shp_rpv <- cd_shp %>%
  left_join(cc_rpv, by = "cd") %>%
  mutate(across(starts_with("pct_|white_"), ~percent(.x, accuracy = 1)))
st_crs(shp_rpv) <- 3857

cc_diff <- cc_rpv %>%
  pivot_wider(id_cols = c(cd, pct_trump, white_elec),
              names_from = race2, values_from = p_mrp_est) %>%
  clean_names()
shp_diff <- cd_shp %>%
  left_join(cc_diff, by = "cd") %>%
  left_join(select(cd_info, cd, dailykos_name, largest_place), by = "cd") %>%
  mutate(across(starts_with("pct_|white_"), ~percent(.x, accuracy = 1)))
st_crs(shp_diff) <- 3857

mrp_range <- range(cc_rpv$p_mrp_est)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Racial Voting by Congressional District (Work in Progress)"),
  em("Shiro Kuriwaki (https://github.com/kuriwaki/CCES_CDmap)."),
  br(),
  em("Map from Daniel Donner (http://dkel.ec/map). MRP and synthetic turnout population estimates by Shiro Kuriwaki."),
  hr(),
  tabsetPanel(
    tabPanel("Race and Racially Polarized Voting",
             column(4,
                    selectInput("race", "Distrct Info. Show Prevalence of", unique(race_by_cd$race)),
                    plotlyOutput("cdmap")),
             column(8,
                    br(),
                    strong("Pr(Trump | White) - Pr(Trump | Non-White), among estimated 2016 Electorate"),
                    br(),
                    # Auto-scale size https://stackoverflow.com/a/44437161
                    tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
                    plotlyOutput("diff_trump"))
    ),
    tabPanel("Component Estimates",
             column(6,
                    strong("Trump vote among Whites"),
                    plotlyOutput("white_trump")),
             column(6,
                    strong("Trump vote among Non-Whites"),
                    plotlyOutput("black_trump"))
    )
  ),
)
)


server <- shinyServer(function(input, output) {

    output$cdmap <- renderPlotly({
      cd_race <- cd_race %>%
        filter(race == input$race) %>%
        mutate(cd_lab = glue("<b>{cd}</b><br><i>{str_wrap(dailykos_name, width = 15)}</i><br>",
                             "{str_wrap(ush116_rep, width = 15)} ({ush116_party})<br>",
                             "{input$race}: {percent(frac, accuracy = 1)}<br>",
                             "McCain {pct_mccain}<br>Romney {pct_romney}<br>Trump {pct_trump}"))


      p <- ggplot(cd_race, aes(fill = frac, text = cd_lab)) +
        geom_sf(color = "white", size = 0.1) +
        scale_fill_viridis_c(option = "viridis", limits = c(0, 1), labels = percent_format(accuracy = 1)) +
        theme_map() +
        labs(fill = input$race) +
        guides(fill = FALSE)

      ggplotly(p,
               width = ((3.5/12)*as.numeric(input$dimension[1])),
               height = (0.4*as.numeric(input$dimension[2])),
               tooltip = "text",
               colors = "Viridis"
      ) %>%
        style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
    })

  output$white_trump <- renderPlotly({

    white_rpv <- shp_rpv %>%
      filter(race2 == "White") %>%
      mutate(cd_lab = glue("<b>{cd}</b><br>",
                           "Pr(Trump | White): {percent(p_mrp_est, accuracy = 1)}<br>"))

    p <- ggplot(white_rpv, aes(fill = p_mrp_est, text = cd_lab)) +
      geom_sf(color = "white", size = 0.1) +
      scale_fill_fermenter(palette = "Reds", limits = c(0, 0.75), direction = 1) +
      theme_map() +
      labs(fill = NULL) +
      theme(legend.position = "bottom") +
      guides(fill = FALSE)

    ggplotly(p,
             width = (0.5*as.numeric(input$dimension[1])),
             height = (0.7*as.numeric(input$dimension[2])),
             tooltip = "text") %>%
      style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
  })

  output$black_trump <- renderPlotly({

    nonwhite_rpv <- shp_rpv %>%
      filter(race2 == "Non-White") %>%
      mutate(cd_lab = glue("<b>{cd}</b><br>",
                           "Pr(Trump | Non-White): {percent(p_mrp_est, accuracy = 1)}<br>"))

    p <- ggplot(nonwhite_rpv, aes(fill = p_mrp_est, text = cd_lab)) +
      geom_sf(color = "white", size = 0.1) +
      scale_fill_fermenter(palette = "Reds", limits = c(0, 0.75), direction = 1) +
      theme_map() +
      guides(fill = FALSE)

    ggplotly(p,
             width = (0.5*as.numeric(input$dimension[1])),
             height = (0.7*as.numeric(input$dimension[2])),
             tooltip = "text") %>%
      style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
  })

  output$diff_trump <- renderPlotly({

    diff_rpv <- shp_diff %>%
      mutate(p_mrp_est = white - non_white) %>%
      mutate(cd_lab = glue("<b>{cd}</b><br><i>{str_wrap(dailykos_name, width = 15)}</i><br>",
                           "Diff: {percent(p_mrp_est, accuracy = 1, suffix = 'pp')}<br>",
                           "White {percent(white, accuracy = 1)} - Non-white {percent(non_white, accuracy = 1)}<br>"))

    p <- ggplot(diff_rpv, aes(fill = p_mrp_est, text = cd_lab)) +
      geom_sf(color = "white", size = 0.1) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      theme_map() +
      labs(fill = "Race Gap") +
      guides(fill = FALSE)

    ggplotly(p,
             width = ((7.5/12)*as.numeric(input$dimension[1])),
             height = (0.7*as.numeric(input$dimension[2])),
             tooltip = "text") %>%
      style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")

  })

})


# Run the application
shinyApp(ui = ui, server = server)
