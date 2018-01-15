# H. Seltman, Jan 2018

# Shiny User Interface for "shinyGrader" app

library(shiny)
library(shinyjs, quietly=TRUE, warn.conflicts=FALSE)

fluidPage(
  useShinyjs(),
  titlePanel("shinyGrader"),
  
  tabsetPanel(
    
    tabPanel(title="Assignment",
             fluidRow(column(2, strong("Current assignment folder:")),
                      column(10, p(id="currentFolder"))
             ),
             fluidRow(column(2, strong("Change folder:")),
                      column(10, actionButton("changeFolder",
                                             "Click, then point to a file in the assignment's folder"))
             ),
             textInput("courseIdText", label="Course Id:"),
             p(strong("Roster info:")),
             textOutput("rosterInfo"), p(),
             checkboxGroupInput("codeFileCheckboxes", label="Choose codefiles to grade:",
                                choices="(None)", inline=TRUE),
             #p(id="rosterEmailCol"),
             p("unzip"),
             p("Choose assignment components")
    ), # end "Setup" tabPanel
    
    tabPanel("Rubrics",
             radioButtons("codefile", label="Choose a codefile:", choices="(None)"),
             textInput("inputReq", "Input requirements", ""),
             textInput("inputAnath", "Input anathemas", ""),
             textInput("outputReq", "Output requirements", ""),
             textInput("outputAnath", "Output anathemas", "")
    ), # end "Setup" tabPanel
    
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
    
    tabPanel("Maintanence",
      p("Examine sent letters"),
      p("History for a student")
    ) # end "Maintenence" tabPanel
    
  ) # end tabsetPanel
) # end fluidPage
