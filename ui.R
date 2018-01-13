# H. Seltman, Jan 2018

# Shiny User Interface for "shinyGrader" app

library(shiny)
library(shinyjs)

fluidPage(
  useShinyjs(),
  titlePanel("shinyGrader"),
  
  tabsetPanel(
    
    tabPanel("Setup",
      actionButton("goButton", "load session to analyze"),
      textOutput("wd"),
      textInput("courseId", label="course id:"),
      textOutput("rosterInfo"),
      #p(id="rosterEmailCol"),
      p("unzip"),
      p("Choose assignment components")
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
