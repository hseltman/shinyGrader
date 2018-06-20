# H. Seltman, Jan 2018

# Server file for shinyGrader app

library(shiny)
require(shinyjs, quietly=TRUE, warn.conflicts=FALSE)

# Server function
function(input, output, session) {
  
  ### Setup for current directory (things that will change if ###
  ### the user changes directories.                           ###
  
  
  # Create initial status object
  noTime = numeric(0)
  class(noTime) = "POSIXct"
  status = data.frame(student=I(character(0)),
                      problem=integer(0),
                      lastRun=noTime,
                      newestFile=noTime,
                      configTime=noTime,
                      status=integer(0))
  
  # Create initial global configuration object
  globalConfig = reactiveVal(staticGlobalConfig)
  
  rosterFileName = reactiveVal(staticRosterFileName)
  #   gc = staticGlobalConfig
  #   rname = findRoster(gc$courseId, gc$rosterDirectory)
  #   if (is.null(rname)) return(NULL)
  #   if (gc$rosterDirectory != dirname(rname))
  #     globalConfig(updateGlobalConfig(gc, list(rosterDirectory=dirname(rname))))
  #   return(rname)
  # })
  
  # Create initial roster
  roster = reactiveVal(staticRoster)
  #shinyjs::html(id="rosterEmailCol", txt)

  observeEvent(rosterFileName, {
    rname = rosterFileName()
    gc = globalConfig()
    if (is.null(rname)) return(NULL)
    newDir = dirname(rname)
    #browser()
    globalConfig(updateGlobalConfig(gc, list(rosterDirectory=newDir)))
    updateTextInput(session, "gcrosterDirectory", value=newDir)
  })
  
  observeEvent(roster, {
    if (is.null(roster())) return(NULL)
  })
    
  # Complete User Interface
  output$rosterFileRender = renderUI({
    placeholder = ifelse(is.null(staticRoster), "No file selected",
                         attr(staticRoster, "file"))
    fileInput("rosterFile", label=NULL, buttonLabel="New Roster",
              accept=".csv",
              width="100%", placeholder=placeholder)
  })
  
  updateTextInput(session=session, inputId="courseIdText",
                  value=staticGlobalConfig[["courseId"]])
  updateTextInput(session=session, inputId="gcrosterDirectory",
                  value=staticGlobalConfig[["rosterDirectory"]])
  
  wd = reactiveVal(getwd())
  
  parsedFiles = reactive({
    #browser()
    gc = globalConfig()
    parseFilenames(list.files(), globalConfig=gc)
  })
  
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
    
  observeEvent(input$changeFolder, {
    if (input$changeFolder) {
      f = try(file.choose(), silent=TRUE)
      if (!is(f, "try-error")) {
        wd(dirname(f))
        browser()
        gc = initializeGlobalConfig(globalLoc)
        #browser()
        globalConfig(updateGlobalConfig(gc, gc))
        cid = gc[["courseId"]]
        updateTextInput(session, "courseIdText", value=cid)
        roster(getRoster(findRoster(cid, gc$rosterDirectory), gc))
      }
    }
  })
  
  observeEvent(input$rosterFile, {
    if (!is.null(input$rosterFile)) rosterFileName(input$rosterFile[1, "datapath"])
  })
  
  observeEvent(
    eval(parse(text=paste0("c(",
                           paste0("input$gc", names(GLOBAL_CONFIG_IDS), collapse=", "),
                           ")"))), {
    widgetValues = vector("list", length(GLOBAL_CONFIG_IDS))
    names(widgetValues) = names(GLOBAL_CONFIG_IDS)
    for (w in names(widgetValues))
      widgetValues[[w]] = input[[paste0("gc", w)]]
    gc = globalConfig()
    globalConfig(updateGlobalConfig(gc, widgetValues))
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
      gc = globalConfig()
      globalConfig(updateGlobalConfig(gc, list(courseId = trimws(cid))))
      #roster = getRoster(cid, gc)
    }
  })

} # end server function
