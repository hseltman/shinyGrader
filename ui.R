# H. Seltman, Jan 2018

# Shiny User Interface for "shinyGrader" app

library(shiny)
library(shinyjs, quietly=TRUE, warn.conflicts=FALSE)

fluidPage(
  useShinyjs(),
  titlePanel("shinyGrader"),
  
  tabsetPanel(
    
    tabPanel(title="Assignment",
             fluidRow(column(3, strong("Current assignement folder:")),
                      column(9, textOutput("currentFolder"))
             ),
             fluidRow(column(3, strong("Change folder:")),
                      column(9, actionButton("changeFolder",
                                             "Click and point to a file in the assignment's folder"))
             ),
             fluidRow(column(3, strong("Course Id:")),
                      column(9, textInput("courseIdText", label="")))
             ,
             p(strong("Roster info:")),
             textOutput("rosterInfo"),
             radioButtons("codeFileButtons", label="Main codefiles:",
                          choices=list("(None)"), inline=TRUE),
             #p(id="rosterEmailCol"),
             p("unzip"),
             p("Choose assignment components")
    ), # end "Setup" tabPanel
    
    tabPanel("Rubrics",
             selectInput("codefile", label="Choose a codefile:", choices="(None)"),
             textInput("inputReq", "Input requirements", ""),
             textInput("inputAnatt", "Input anathemas", ""),
             textInput("outputReq", "Output requirements", ""),
             textInput("outputReq", "Output anathemas", "")
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
