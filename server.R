# H. Seltman, Jan 2018

# Server file for shinyGrader app

library(shiny)
library(shinyjs)

# Server function
function(input, output, session) {
  wd = getwd()

  globalConfig = initializeGlobalConfig()
  courseId = globalConfig[["courseId"]]
  updateTextInput(session=session, inputId="courseId", value=courseId)
  roster = getRoster(globalConfig[["courseId"]])
  #shinyjs::html(id="rosterEmailCol", txt)
  
  observe({
    if(input$goButton > 0) {
      f = try(file.choose(), silent=TRUE)
      if (!is(f, "try-error")) {
        wd = dirname(f)
        output$session <- renderText(wd)
        globalConfig = iniitializeGlobalConfig()
        roster = getRoster(globalConfig[["courseId"]])
      }
    }
  })
    
  output$rosterInfo = renderPrint({
    #input$courseId
    #input$goButton
    if (!is.null(roster) && is.list(roster) && len(roster)==1 && is.data.frame(roster[[1]])) {
      cat(nrow(roster[[1]]), "students")
    } else {
      cat("No roster available")
    }
  })
  
  observeEvent(input$courseId, {
    if (nchar(input$courseId) > 0) {
      courseId = globalConfig[["courseId"]] = trimws(input$courseId)
      roster = getRoster(input$courseId)
      if (!is.null(roster)) {
        updateTextInput(session, "courseId", courseId)
      }
    }
  })

} # end server function
