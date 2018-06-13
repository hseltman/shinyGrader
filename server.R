# H. Seltman, Jan 2018

# Server file for shinyGrader app

library(shiny)
library(shinyjs, quietly=TRUE, warn.conflicts=FALSE)

# Server function
function(input, output, session) {
  globalConfig = initializeGlobalConfig()
  updateTextInput(session=session, inputId="courseIdText",
                  value=globalConfig[["courseId"]])
  
  roster = reactive({
    cid = input$courseIdText
    #browser()
    getRoster(cid)
  })
  #shinyjs::html(id="rosterEmailCol", txt)
  
  wd = reactiveVal(getwd())
  parsedFiles = reactive(parseFilenames(list.files()))
  codingFiles = reactive({
    pf = parsedFiles()
    fileList = c()
    if (!is.null(pf)) {
      temp = pf[tolower(pf$fileExtension) %in% c(".r", ".rmd", ".sas", ".py"), ]
      fileList = apply(temp, 1, function(r) paste0(r["baseFileName"],
                                                   r["fileExtension"]))
    }
    return(unique(fileList))
  })
  
  # output$codeFileList = renderPrint({
  #   cf = codingFiles()
  #   validate(need(cf), "No coding files in current folder")
  #   cat(paste(cf, collapse=", "))n
  
  # })
  
  
  observe({
    wd = wd()
    shinyjs::html(id="currentFolder", 
                  paste0("<strong>", wd, "</strong>"))
  })
  
  observe({
    cf = codingFiles()
    req(cf)
    updateCheckboxGroupInput(session, "codeFileCheckboxes",
                             choices=cf, inline=TRUE)
  })

  observeEvent(input$codeFileCheckboxes, {
    cfSel = input$codeFileCheckboxes
    updateRadioButtons(session, "codefile",
                             choices=cfSel, inline=TRUE)
  })
    
  observe({
    if (input$changeFolder) {
      f = try(file.choose(), silent=TRUE)
      if (!is(f, "try-error")) {
        wd(dirname(f))
        globalConfig = initializeGlobalConfig()
        cid = globalConfig[["courseId"]]
        updateTextInput(session, "courseIdText", cid)
        roster = getRoster(cid)
      }
    }
  })
    
  #output$currentFolder = renderPrint({
  #  cat(wd())
  #})
  
  output$rosterInfo = renderPrint({
    roster = roster()
    if (is.data.frame(roster)) {
      cat(nrow(roster), "students")
    } else {
      cat("No roster available")
    }
  })
  
  observeEvent(input$courseIdText, {
    cid = input$courseIdText
    if (nchar(cid) > 0) {
      globalConfig[["courseId"]] = trimws(input$courseIdText)
      writeConfig(globalConfig, GLOBAL_CONFIG_NAME)
      #roster = getRoster(cid)
    }
  })

} # end server function
