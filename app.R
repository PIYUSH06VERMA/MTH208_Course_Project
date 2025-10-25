#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

########################################################################################################################
ui<- page_navbar(
  title = "Our title",
  theme=bs_theme(
    bg="aliceblue",
    fg="#333333",
    primary = "green",
    "navbar-bg"="green",
  ),
  #introduction statements to be added
  tabPanel(
    title="Introduction",
    h1("Welcome to our Macroeconomic analyzer!"),
    card(p("we will be presenting a deep analysis on how various macroeconomic parameters are related to each other. blah blah blah")),
    ),
  tabPanel(
    title="Explore the plots",
    icon=icon("chart-line"),
    h1("Hereby we present the plots we have analyzed: "),
      navset_card_pill(
        nav_panel(
          title="preston curve",
          icon=icon("arrow-up-right-dots"),
      ),
      nav_panel(
        title="GDP vs time",
      )
    ),#options for various plots
  ),
  tabPanel(title = "Dataset")
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
