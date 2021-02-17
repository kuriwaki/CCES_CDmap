# This is the user-interface definition of a Shiny web application. You can

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Congressional District Map"),

    # Sidebar with input on race to show
    selectInput("race", "Racial Group",
                unique(race_by_cd$race)
    ),

    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("cdmap")
    )
)
)
