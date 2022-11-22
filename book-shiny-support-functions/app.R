
###########################################################################
###########################################################################
###                                                                     ###
###                              SSL INDEX                              ###
###                                                                     ###
###########################################################################
###########################################################################

## Data handling from HTML format
#devtools::install_github("rstudio/miniUI")
require(miniUI)

## Data handling
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)

## Visualizations
require(ggplot2)
require(ggnewscale)
require(RColorBrewer)
require(cowplot)
require(plotly)
require(grid)
require(ggpubr)
require(ggforce)

## Tables
require(formattable)
# require(gt)
# require(gtExtras) #Github package
require(reactable)


## Package for handling date and time
require(lubridate)

## Packages for handling strings
require(stringr)
require(stringi)

## Loading packages for handling RMarkdown files
require(rmarkdown)
require(markdown)

##Loading Database packages for SQLite database
require(DBI)
require(dbplyr)
require(RSQLite)

## Loading Shiny packages
require(shiny)
require(DT)
require(knitr)
require(kableExtra)
require(shinycssloaders)
require(shinyjs)
require(shinydashboard)
require(fresh)
require(shiny.router)
require(shinyAce)

require(leaflet)

##################################################################
##                          Logo and Theme                      ##
##################################################################

# customLogo <- 
#   shinyDashboardLogoDIY(
#     boldText = "",
#     mainText = tags$a(
#       href='https://simulationhockey.com/',
#       target="_blank",
#       tags$img(src='shl_analytics_logo.png', height = "70"),
#     ),
#     badgeText = version,
#     badgeTextColor = "white",
#     badgeBackColor = sslRed
#   )

customTheme <- 
  mytheme <- create_theme(
    adminlte_color(
      light_blue = "#434C5E"
    ),
    adminlte_sidebar(
      # width = "400px",
      dark_bg = "#D8DEE9",
      dark_hover_bg = "#81A1C1",
      dark_color = "#2E3440"
    ),
    adminlte_global(
      content_bg = "#FFF",
      box_bg = "#D8DEE9", 
      info_box_bg = "#D8DEE9"
    )
  )

#################################################################
##               Running all modules for the app               ##
#################################################################

fileSources <- c("app-modules")
# 
# ## Loads and runs RMarkdown files
# rmdFiles <- 
#   sapply(
#     X = fileSources,
#     FUN = function(x){
#       list.files(path = x, pattern = ".Rmd$") %>% 
#         paste(x, ., sep = "/")
#     },
#     simplify = TRUE,
#     USE.NAMES = FALSE
#   ) %>% 
#   unlist() %>% 
#   .[str_detect(., pattern = ".Rmd")]
# 
# sapply(rmdFiles, rmarkdown::render, quiet = T, output_dir = "app-documents")
# 
## Loads files
sapply(
  X = fileSources,
  FUN = function(x){
    files <- list.files(path = x, pattern = ".R$")

    sapply(
      X = paste(x, files, sep = "/"),
      FUN = source
    )
  }
)


##################################################################
##                  The UI and Server function                  ##
##################################################################

# jsResetCode <- "shinyjs.restart = function() {history.go(0)}"

ui <- function(request){
  dashboardPage(
    ##----------------------------------------------------------------
    ##                            Header                             -
    ##----------------------------------------------------------------
    
    dashboardHeader(
      title = "TEST"#,
      # tags$li(
      #   tags$head(
      #     tags$link(
      #       rel = "icon", 
      #       type = "image/png", 
      #       href = "favicon.png"),
      #     tags$title("SSL Index")
      #   ),
      #   class = "dropdown",
      #   tags$head(
      #     
      #     ## HTML code so that a href link inherits the text color, not the link color
      #     tags$style(HTML("a, a:hover, a:visited, a:active {color: inherit}")),
      #     tags$style(HTML('
      #       thead th {
      #         background-color:#00044d !important;
      #         color:#ffffff !important;
      #       }')),
      #     tags$style(
      #       type="text/css",
      #       "#playerComparison-fieldImage img {max-width: 480px; width: inherit; max-height: 600px;}"
      #     )
      #     # ## Increases the size of the logo box at the top left
      #     # tags$style(".main-header {max-height: 80px}"),
      #     # tags$style(".main-header .logo {height: 80px}"),
      #     # tags$style(".main-header .logo {width: 300px}"),
      #     # 
      #     # ## Changes the margin of the sidebar
      #     # tags$style(".main-header .navbar {margin-left: 300px}"),
      #     # tags$style(type="text/css", "text {font-family: sans-serif, courier}"),
      #     # 
      #   )
      # )
    ),
    
    ##---------------------------------------------------------------
    ##                            Sidebar                           -
    ##---------------------------------------------------------------
    
    dashboardSidebar(
      # # Adjust the sidebar in accordance with the higher header
      # tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
      sidebarMenu(
        id = "tabs",
        menuItem(
          "Welcome",
          tabName = "Welcome",
          selected = TRUE
        ),
        menuItem(
          "Testa kod",
          tabName = "test",
          selected = FALSE
        ),
        menuItem(
          "ANOVA",
          tabName = "anova",
          selected = FALSE
        )
      )
    ),
    
    ##----------------------------------------------------------------
    ##                              Body                             -
    ##----------------------------------------------------------------
    
    dashboardBody(
      use_theme(customTheme),
      useShinyjs(),
      ### Specifies a custom color for value and info boxes
      tags$style(".small-box.bg-orange { background-color: #e08b46 !important; color: #000000 !important; }"),
      tabItems(
        tabItem(
          "test",
          testUI(id = "welcome")
        ),
        tabItem(
          "anova",
          anovaUI(id = "ANOVA")
        )#,
        # tabItem(
        #   "schedule",
        #   titlePanel(
        #     h1("Schedule", align = "center")
        #   ),
        #   scheduleUI(id = "schedule")
        # )
      )
    )
    ##----------------------------------------------------------------
  )
}

server <- function(input, output, session) {
  
  # ## Observes a reset
  # observeEvent(input$reset_button, {js$restart()}) 
  
  
  loadedModuleTest <- reactiveVal(FALSE)
  loadedModuleANOVA <- reactiveVal(FALSE)
  # loadedModuleStandings <- reactiveVal(FALSE)
  # loadedModulePlayerStats <- reactiveVal(FALSE)
  # loadedModulePlayerComparison <- reactiveVal(FALSE)
  # loadedModulePlayerBuilder <- reactiveVal(FALSE)
  # loadedModuleTrackerPosition <- reactiveVal(FALSE)
  # loadedModuleOverviewTeam <- reactiveVal(FALSE)
  # loadedModulePlayerDatabase <- reactiveVal(FALSE)
  # loadedModulePlayerRecords <- reactiveVal(FALSE)
  # loadedModuletrackerTPE <- reactiveVal(FALSE)
  # loadedModuleIIHF <- reactiveVal(FALSE)
  
  
  ##---------------------------------------------------------------
  ##          Loading each of the different backend sites         -
  ##---------------------------------------------------------------
  ### Only run the module once the menu is clicked to fasten start time
  observeEvent(input$tabs,{
    # Checks which menu tab has been selected and whether the module has already been loaded
    if(input$tabs == "test" & !loadedModuleTest()){

      loadedModuleTest(TRUE)

      testSERVER(id = "welcome")

    } else if(input$tabs=="anova" & !loadedModuleANOVA()){

      loadedModuleANOVA(TRUE)

      anovaSERVER(id = "ANOVA")

    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ### Sets the url for each tab
  observeEvent(input$tabs,{
    ## Writes a different url based on the tab
    newURL <- paste0(
      session$clientData$url_protocol,
      "//",
      session$clientData$url_hostname,
      ":",
      session$clientData$url_port,
      session$clientData$url_pathname,
      "#",
      input$tabs
    )
    updateQueryString(newURL, mode = "replace", session)
  })

  observe({
    currentTab <- sub("#", "", session$clientData$url_hash)
    if(!is.null(currentTab)){
      updateTabItems(session, "tabs", selected = currentTab)
    }
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "disable")
