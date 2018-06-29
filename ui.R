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

             fluidRow(column(2, actionButton("changeRoster",
                                             HTML("Change Roster"))),
                      column(10, p(id="currentRoster", style="padding:7px;"))),
             
             HTML("&nbsp;"),
             p(id="totalPoints", "Total points: 0"),
             h3("Settings:"),
             textInput("gccourseId", "Course Id (e.g., 36200)",
                       value=staticGlobalConfig[["courseId"]]),
             textInput("gcassignmentName", "Assignment Name", 
                       value=staticGlobalConfig[["assignmentName"]], width="100%"),
             textInput("gcrosterDirectory", "Roster Directory",
                       value=staticGlobalConfig[["rosterDirectory"]],
                       width="100%"),
             textInput("gcinstructorEmail", "Instructor Email",
                       value=staticGlobalConfig[["instructorEmail"]], width="100%"),
             p(HTML("&nbsp;")),
             HTML(paste("<small>&Dagger;: To change assignment folder,",
                        "select any file in the assignment folder.</small>"))
    ), # end "Assignment" tabPanel


    # Generate problem rubrics panel automatically
    tabPanel("Problems", eval(parse(text=probPanelCode))),

        
    tabPanel("Grading",
      p("Edit specific configuration"),
      p("Grade one"),
      fluidRow(column(5, disabled(selectInput("studentDropdown", "Students", "(none)", width="40%"))),
               column(5, verbatimTextOutput("filesForOne")),
               column(2, disabled(actionButton("runOne1", "Run one student")))),
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
