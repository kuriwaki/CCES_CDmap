# This is the user-interface definition of a Shiny web application. You can

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
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

    # Application title
    # titlePanel("Congressional District Map"),

    # Sidebar with input on race to show
    selectInput("race", "Racial Group",
                unique(race_by_cd$race)
    ),

    # Show a plot of the generated distribution
    mainPanel(
        plotlyOutput("cdmap")
    )
)
)
