
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#' to prevent double y-scroll bar when changing from Genes to GO terms
#' https://github.com/rstudio/shinydashboard/issues/255#issuecomment-368536215
#' inside: R-x.x.x\library\shinydashboard\AdminLTE\AdminLTE.min.css
#' change
#' .wrapper{height:100%;position:relative;overflow-x:hidden;overflow-y:auto}
#' to
#' .wrapper{height:100%;position:relative;overflow-x:hidden;overflow-y:hidden}


# 1. PACKAGES -------------------------------------------------------------

source("library/load_packages.R")
# file.edit("library/load_packages.R")


# 2. DATA -----------------------------------------------------------------

source("data/data.R")
# file.edit("data/data.R")


# 3. tabItems -----------------------------------------------------------------

source("tabItems.R", local = T)
# file.edit("tabItems.R")


# 4. server2 -----------------------------------------------------------------

# file.edit("server2.R")



# UI ----------------------------------------------------------------------
# Define UI for application that draws a histogram
# ui <- dashboardPage(
ui <- function(request) { 
  
  
  dashboardPage(
    
    
    dashboardHeader(
      title = "ShinyPCRViz",
      dropdownMenu(
        type = "messages",
        messageItem(
          from = " ",
          message = "Feedback to yann.ladner@aofoundation.org",
          href = "mailto:yann.ladner@aofoundation.org"
        )
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("PCR visualization",
                 icon = icon("chart-bar"), startExpanded = T,
                 
                 tab_upload,
                 
                 a(href="Template_ShinyPCRViz.xlsx", HTML("<center>Click here to download an<br>example (template) file:</center><br>"), download=NA, target="_blank"),
                 
                 menuSubItem("Scatter plot basic", 
                             tabName = "scatter1", 
                             icon = icon("chart-line", lib = "font-awesome"), selected = F),
                 menuSubItem("Scatter plot extended", 
                             tabName = "scatter2", 
                             icon = icon("chart-line", lib = "font-awesome"), selected = T)
                 
                 
                 
                 # fluidPage(
                 #   downloadButton(outputId = "downloadData", 
                 #             label = HTML("<center>Download an<br>example (template) file:</center>"),
                 #             accept = c(".xlsx"), multiple = F
                 #   ),
                 #   
                 #   
                 # ),
                 
                
                 
                 
                 )
        )
      ),
    dashboardBody(
      tabItems(
      tab_scatter1,
      tab_scatter2
      )
      )
    )
  }

addResourcePath("tmpuser", getwd()) # needed to call html files - check - output$aggScor_MW


# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # contains all the server parts
  source("server2.R", local = T)
  
} # finish server

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")