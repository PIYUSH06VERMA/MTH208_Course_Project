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
    card(
    h1("Hereby we present the plots we have analyzed: "),
    ),
    layout_columns(
      col_widths = c(4,4,4),
      
      card(
        bg="green",
        fg="white",
        card_body(
          class="text-center",
            tags$a(href = "#", # Replace # with actual link/tab id if needed
                   style = "text-decoration: none; color: inherit;", # Remove underline, keep text color
                   icon("arrow-up-right-dots", height = "3rem", class = "mb-3"),
                   h4("Preston Curve"),
                   p("Analysis of life expectancy vs. GDP per capita")
          )
        )
      ),
      
      card(
        bg = "green",
        fg = "white",
        card_body(
          class = "text-center",
          tags$a(
            href = "#",
            style = "text-decoration: none; color: inherit;",
            icon("bar-chart", height = "3rem", class = "mb-3"),
            h4("Inflation Rate"),
            p("Examining inflation trends and impact")
          )
        )
      ),
      
      # --- Card 4 ---
      card(
        bg = "green",
        fg = "white",
        card_body(
          class = "text-center",
          tags$a(
            href = "#",
            style = "text-decoration: none; color: inherit;",
            icon("money-bill-wave", height = "3rem", class = "mb-3"),
            h4("Interest Rates"),
            p("How interest rates influence the economy")
          )
        )
      ),
      card(
        bg = "green",
        fg = "white",
        card_body(
          class = "text-center",
          tags$a(
            href = "#",
            style = "text-decoration: none; color: inherit;",
            icon("users", height = "3rem", class = "mb-3"),
            h4("Unemployment Rate"),
            p("Tracking labor market statistics")
          )
        )
      ),
      
      # --- Card 6 ---
      card(
        bg = "green",
        fg = "white",
        card_body(
          class = "text-center",
          tags$a(
            href = "#",
            style = "text-decoration: none; color: inherit;",
            icon("industry", height = "3rem", class = "mb-3"),
            h4("Industrial Production"),
            p("Key indicators of economic output")
          )
        )
      ),
      
      # --- Card 7 ---
      card(
        bg = "green",
        fg = "white",
        card_body(
          class = "text-center",
          tags$a(
            href = "#",
            style = "text-decoration: none; color: inherit;",
            icon("globe", height = "3rem", class = "mb-3"),
            h4("Trade Balance"),
            p("Imports, exports, and global trade dynamics")
          )
        )
      ),
      card(
        card_body(
          class="text-center",
            tags$a(href = "#",
                   style = "text-decoration: none; color: inherit;",
                   icon("chart-line", height = "3rem", class = "mb-3"),
                   h4("GDP vs Time"),
                   p("Analysis of GDP trends over the years"),
          )    
        )
      )
    )
  ),
    #options for various plots
  tabPanel(title = "Dataset"),
  tabPanel(title="About")
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
