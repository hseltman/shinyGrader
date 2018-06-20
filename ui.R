# H. Seltman, Jan 2018

# Shiny User Interface for "shinyGrader" app
# A tabset panel with tabs for (whole) Assignment, Problems,
# Grading, Letters, and Maintenance

library(shiny)
require(shinyjs, quietly=TRUE, warn.conflicts=FALSE)
library(shinyalert)

fluidPage(
  useShinyjs(), useShinyalert(),
  titlePanel("shinyGrader"),
  
  tabsetPanel(
    
    tabPanel(title="Assignment",
             HTML("&nbsp;"),
             fluidRow(column(2, actionButton("changeFolder",
                                  HTML("Change Folder <sup>&Dagger;</sup>"))),
                      column(10, p(id="currentFolder", style="padding:7px;"))),
             
             uiOutput("rosterFileRender"),
             HTML("&nbsp;"),
             h3("Settings:"),
             genUiCode(names(GLOBAL_CONFIG_IDS), as.character(GLOBAL_CONFIG_IDS),
                       initialGCvalues,
                       prefix="gc", perRow=3),
             p(HTML("&nbsp;")),
             p(HTML("&nbsp;")),
             HTML(paste("<small>&Dagger;: To change assignment folder,",
                        "select any file in the assignment folder.</small>"))
    ), # end "Assignment" tabPanel
             
    tabPanel("Problems", eval(parse(text=probPanelCode))), # Problems panel
    
    tabPanel("Grading",
      p("Edit specific configuration"),
      p("Grade one"),
      p("Grade all"),
      p("Grading output")
    ), # end "Grading" tabPanel
    
    tabPanel("Letters",
      p("Assemble letters"),
      p("Send one letter"),
      p("Send all letters")
    ), # end "Letters" tabPanel
    
    tabPanel("Maintenance",
      p("Examine sent letters"),
      p("History for a student")
    ) # end "Maintenence" tabPanel
    
  ) # end tabsetPanel
) # end fluidPage
