# H. Seltman, Jan 2018

# Server file for shinyGrader app

library(shiny)
library(shinyjs)

# Server function
function(input, output, session) {
  #globalConfig = initializeGlobalConfig()
  txt = globalConfig[["rosterEmailCol"]]
  shinyjs::html(id="rosterEmailCol", txt)
  
} # end server function