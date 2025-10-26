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
   # TAB 1: INTRODUCTION
  # -------------------------------------------------------------------
  tabPanel(
    title = "Introduction",
    icon = icon("globe"),
    fluidPage(
      br(),
      h1("🌍 Welcome to the Macroeconomic Analyzer!", 
         style = "text-align:center; color:darkgreen; font-weight:700; font-size:40px;"),
      br(),
      
      # Dashboard intro section
      fluidRow(
        column(
          width = 12,
          card(
            card_header(div(icon("info-circle"), " Discover the Dashboard", 
                            style = "font-size:20px; font-weight:600; color:#1b5e20;")),
            card_body(
              p("Step into a world where global economics comes alive! The Macroeconomic Analyzer allows you to interactively explore how GDP, inflation, and unemployment shaped the fortunes of nations from 2015 to 2023. 
                 Compare countries, observe trends over time, and uncover the economic stories behind the numbers.", 
                style = "font-size:16px; color:#333333;"),
              
              p("Here, you can uncover:", style = "font-size:16px; font-weight:600; color:#1b5e20;"),
              tags$ul(
                tags$li("How income levels influence life expectancy across countries using the Preston Curve."),
                tags$li("Time trends in GDP and how continents responded to major economic disruptions like COVID-19."),
                tags$li("Interactions between inflation, unemployment, and GDP to reveal the hidden dynamics of the global economy."),
                tags$li("Interactive plots and visual storytelling that make complex economic data easy to understand and explore.")
              ),
              
              p("Whether you are a student, researcher, or policy enthusiast, this dashboard provides a hands-on experience to observe patterns, analyze trends, and gain data-driven insights in a visually engaging way.", 
                style = "font-size:16px; color:#333333;")
            )
          )
        )
      ),
      
      br(),
      
      # Highlight cards (objectives in flowing style)
      fluidRow(
        column(
          width = 12,
          card(
            card_body(
              div(icon("chart-line", style = "color:green;"), 
                  " Gain insights into global economic inequalities and growth patterns."),
              
              div(icon("chart-area", style = "color:green;"), 
                  " Track GDP trends over time and understand continent-wise economic resilience."),
              
              div(icon("chart-pie", style = "color:green;"), 
                  " Explore interactions between inflation, unemployment, and GDP to reveal underlying dynamics.")
            )
          )
        )
      ),
      
      br(),
      
      # Footer
      div(
        card(
          card_body(
            h4("💡 Key Insight", style = "color:darkgreen; font-weight:600;"),
            p("Economic indicators rarely move in isolation. This dashboard tells the story they create together, enabling you to visualize trends, correlations, and anomalies in a post-pandemic world. Discover patterns, compare nations, and understand the economic forces shaping global growth.", 
              style = "font-size:15px; color:#444444;")
          ),
          card_footer(
            em(""),
            style = "text-align:center; color:gray;"
          )
        )
      )
    )
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
  tabPanel(title = "Dataset")
),
# TAB 4: ABOUT THE PROJECT
  # -------------------------------------------------------------------
  tabPanel(
    title = "About the Project",
    icon = icon("book-open"),
    fluidPage(
      br(),
      h1("📘 About the Project", style = "text-align:center; color:darkgreen; font-weight:700;"),
      br(),
      
      # 1. Problem & Motivation
      card(
        card_header(div(icon("exclamation-circle"), " Problem & Motivation", 
                        style = "font-size:20px; font-weight:600; color:#1b5e20;")),
        card_body(
          p("In an interconnected world, economic health is often reflected through indicators such as GDP, inflation, unemployment, and life expectancy. 
             Yet, understanding how these variables interact across countries and over time remains a complex task.", style = "font-size:16px;"),
          p("This project aims to simplify that complexity. By combining statistical modeling with visual analytics, 
             the Macroeconomic Analyzer provides a structured view of global economic performance from 2015 to 2023—highlighting 
             how regions evolved, responded to global shocks like COVID-19, and how key macroeconomic relationships 
             such as the Preston Curve and Phillips Curve hold in recent years.", style = "font-size:16px;")
        )
      ),
      
      br(),
      
      # 2. Data Sources & Methodology
      card(
        card_header(div(icon("database"), " Data Sources & Methodology", 
                        style = "font-size:20px; font-weight:600; color:#1b5e20;")),
        card_body(
          h5("Data Sources", style = "font-weight:600; color:#1b5e20;"),
          p("The dataset used in this project is extracted from the World Bank’s Open Data API 
             through the R package 'WDI'. It contains annual data for over 190 countries between 2015 and 2023 
             covering indicators such as:", style = "font-size:16px;"),
          tags$ul(
            tags$li("GDP per capita (constant 2015 US$)"),
            tags$li("Unemployment rate (% of total labor force)"),
            tags$li("Inflation, consumer prices (annual %)"),
            tags$li("Life expectancy at birth"),
            tags$li("Labor force participation rate")
          ),
          p("Additional references such as the World Happiness Report and Wikipedia are used for contextual insights 
             and indicator definitions.", style = "font-size:16px;"),
          
          h5("Methodology", style = "font-weight:600; color:#1b5e20;"),
          p("Data was collected and merged from multiple sources, cleaned for missing or inconsistent records, 
             and imputed where necessary using mean substitution. The analysis involves descriptive summaries, 
             trend visualization, and regression-based exploration of relationships among key indicators.", 
            style = "font-size:16px;")
        )
      ),
      
      
      # 3. Tools & Technologies
      card(
        card_header(div(icon("tools"), " Tools & Technologies", 
                        style = "font-size:20px; font-weight:600; color:#1b5e20;")),
        card_body(
          tags$ul(
            tags$li(strong("R Language:"), " for data cleaning, transformation, and visualization."),
            tags$li(strong("Shiny:"), " to develop an interactive dashboard for real-time exploration."),
            tags$li(strong("Packages Used:"), " WDI, dplyr, ggplot2, tidyr, bslib, countrycode, plm, and others."),
            tags$li(strong("APIs:"), " World Bank Open Data API for real-time indicator extraction."),
            tags$li(strong("Design Framework:"), " bslib for a cohesive and modern interface using Bootstrap 5 themes.")
          ),
          p("Together, these tools enable a seamless integration of statistical modeling, interactive visualization, 
             and storytelling in one unified dashboard.", style = "font-size:16px;")
        )
      ),
    
      
      # 4. Learning Outcomes
      card(
        card_header(div(icon("graduation-cap"), " Learning Outcomes", 
                        style = "font-size:20px; font-weight:600; color:#1b5e20;")),
        card_body(
          tags$ul(
            tags$li("Developed practical understanding of how macroeconomic indicators interact globally."),
            tags$li("Gained experience in data collection from APIs and handling multi-year, cross-country datasets."),
            tags$li("Enhanced visualization and storytelling skills through R Shiny app development."),
            tags$li("Applied statistical and econometric reasoning to interpret real-world data patterns.")
          ),
          p("Overall, the project bridges theory with practice — transforming raw economic data into an 
             interactive analytical tool for exploration and insight generation.", style = "font-size:16px;")
        )
      ),
      
      br(),
      
      # 5. References
      card(
        card_header(div(icon("book"), " References", 
                        style = "font-size:20px; font-weight:600; color:#1b5e20;")),
        card_body(
          tags$ul(
            tags$li(a("World Bank Open Data", href = "https://data.worldbank.org/", target = "_blank")),
            tags$li(a("WDI R Package Documentation", href = "https://cran.r-project.org/web/packages/WDI/index.html", target = "_blank")),
            tags$li(a("World Happiness Report", href = "https://worldhappiness.report/", target = "_blank")),
            tags$li("Wikipedia pages on macroeconomic indicators (Inflation, GDP, Unemployment, etc.)")
          ),
          p("All data sources are publicly available and used strictly for academic and educational purposes.", 
            style = "font-size:15px; color:#444444; font-style:italic;")
        )
      ),
      
      br(),
      div(style = "text-align:center; color:gray; font-style:italic;",
          "Department of Economics and Statistics | 2025")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
