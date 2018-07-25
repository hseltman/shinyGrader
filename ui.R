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
  
  tabsetPanel(id="outerTabs",
    
    tabPanel(title="Assignment",
             HTML("&nbsp;"),
             fluidRow(column(2, actionButton("changeFolder",
                                  HTML("Change folder <sup>&Dagger;</sup>"))),
                      column(10, p(id="currentFolder", style="padding:7px;"))),

             
             fluidRow(column(2, actionButton("fileRefresh", "Manual file refresh")),
                      column(10, 
                             p("Use 'Manual file refresh' if you added or removed files manually"))),
             
             fluidRow(column(2, p("Unzip a Canvas zip file", style="padding:8px;font-weight:bold")),
                      column(10, shinyjs::disabled(fileInput("unzip", label=NULL, 
                                                             accept="application/zip",
                                                             width="100%")))),
             
             fluidRow(column(2, actionButton("changeRoster",
                                             HTML("Change roster"))),
                      column(10, p(id="currentRoster", style="padding:7px;"))),
             
             HTML("&nbsp;"),
             p(id="totalPoints", "Total points: 0"),
             h3("Settings:"),
             textInput("courseId", "Course Id (e.g., 36200)",
                       value=staticGlobalConfig[["courseId"]]),
             textInput("assignmentName", "Assignment Name", 
                       value=staticGlobalConfig[["assignmentName"]], width="100%"),
             textInput("instructorEmail", "Instructor Email",
                       value=staticGlobalConfig[["instructorEmail"]], width="100%"),
             p(HTML("&nbsp;")),
             actionButton("debug", "debug"),
             HTML(paste("<small>&Dagger;: To change assignment folder,",
                        "select any file in the assignment folder.</small>"))
    ), # end "Assignment" tabPanel


    # Generate problem rubrics panel automatically
    tabPanel("Problems", eval(parse(text=probPanelCode))),

        
    tabPanel("Grading",
      radioButtons("currentProblem", "Problem to grade", choices=staticCurrentProblem,
                   inline=TRUE),
      fluidRow(column(5, selectInput("selectStudent",
                                     "0 Students (Canvas name; email)",
                                     "Instructor (solution;)", width="100%")),
               column(7, verbatimTextOutput("filesForOne"))),
      fluidRow(column(2, disabled(actionButton("analyzeCode", "Analyze code"))),
               column(2, disabled(actionButton("runCode", "Run code"))),
               column(2, disabled(actionButton("analyzeOutput", "Analyse output")))),
      htmlOutput("inc")
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
