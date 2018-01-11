# H. Seltman, Jan 2018

# Shiny User Interface for "shinyGrader" app

fluidPage(
  titlePanel("shinyGrader"),
  
  tabsetPanel(
    
    tabPanel("Setup",
      p("Choose a directory"),
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
