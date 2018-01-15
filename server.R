# H. Seltman, Jan 2018

# Server file for shinyGrader app

library(shiny)
library(shinyjs, quietly=TRUE, warn.conflicts=FALSE)

# Server function
function(input, output, session) {
  #wd = getwd()

  globalConfig = initializeGlobalConfig()
  updateTextInput(session=session, inputId="courseIdText",
                  value=globalConfig[["courseId"]])
  roster = reactive({
    cid = input$courseIdText
    #browser()
    validate(need(length(cid)>0, "Enter course Id to load roster"))
    #browser()
    getRoster(cid)
  })
  #shinyjs::html(id="rosterEmailCol", txt)
  
  wd = reactiveVal(getwd())
  parsedFiles = reactive(parseFilenames(list.files()))
  codingFiles = reactive({
    pf = parsedFiles()
    whole = pf[[1]]
    short = pf[[2]]
    fileList = c()
    if (!is.null(whole)) {
      temp = whole[tolower(whole$fileExtension) %in% c(".r", ".rmd", ".sas", ".py"), ]
      fileList = apply(temp, 1, function(r) paste0(r["baseFileName"],
                                                   r["fileExtension"]))
    }
    if (!is.null(short)) {
      temp = short[tolower(short$fileExtension) %in% c(".r", ".rmd", ".sas", ".py"), ]
      fileList = c(fileList,
                   apply(temp, 1, function(r) paste0(r["baseFileName"],
                                                     r["fileExtension"])))
    }
    return(fileList)
  })
  
  # output$codeFileList = renderPrint({
  #   cf = codingFiles()
  #   validate(need(cf), "No coding files in current folder")
  #   cat(paste(cf, collapse=", "))n
  
  # })
  
  observe({
    cf = codingFiles()
    req(cf)
    updateRadioButtons(session, "codeFileButtons", choices=as.vector(cf))
  })
  
  observe({
    if (input$changeFolder) {
      f = try(file.choose(), silent=TRUE)
      if (!is(f, "try-error")) {
        wd = dirname(f)
        #output$ <- renderText(wd)
        globalConfig = initializeGlobalConfig()
        cid = globalConfig[["courseId"]]
        updateTextInput(session, "courseIdText", cid)
        if (cid == "") {
          roster = NULL
        } else {
          roster = getRoster(cid)
        }
      }
    }
  })
    
  output$currentFolder = renderPrint({
    cat(wd())
  })
  
  output$rosterInfo = renderPrint({
    roster = roster()
    if (!is.null(roster) && is.list(roster) && length(roster) == 1
        && is.data.frame(roster[[1]])) {
      cat(nrow(roster[[1]]), "students")
    } else {
      cat("No roster available")
    }
  })
  
  observeEvent(input$courseIdText, {
    cid = input$courseIdText
    #browser()
    if (nchar(cid) > 0) {
      globalConfig[["courseId"]] = trimws(input$courseIdText)
      browser()
      roster = getRoster(cid)
    }
  })

} # end server function
