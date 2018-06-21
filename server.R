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
  
  ###############################
  ### Complete User Interface ###
  ###############################
  
  updateTextInput(session=session, inputId="courseIdText",
                  value=staticGlobalConfig[["courseId"]])
  updateTextInput(session=session, inputId="gcrosterDirectory",
                  value=staticGlobalConfig[["rosterDirectory"]])
  shinyjs::html(id="currentRoster", 
                paste0("<strong>", staticRosterBaseName, "</strong>"))
  if (!is.null(staticRoster)) {
    shinyjs::html(id="rosterSize", 
                  paste0("(",  nrow(staticRoster), " rows)"))
  }
  
  ########################
  ### Create reactives ###
  ########################
  
  # Store working directory and allow observers to know when it changes
  wd = reactiveVal(getwd())

  # Store global configuration object and allow observers to know when it changes
  globalConfig = reactiveVal(staticGlobalConfig)
  
  # Store roster filename and allow observers to know when it changes 
  rosterFileName = reactiveVal(staticRosterFileName)
  
  # Store roster and allow observers to know when it changes
  roster = reactiveVal(staticRoster)
  
  # Store parsed files and allow observers to know when it changes
  parsedFiles = reactive({
    #browser()
    gc = globalConfig()
    parseFilenames(list.files(), globalConfig=gc)
  })
  
  # Store coding files and allow observers to know when it changes
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
  
  ########################
  ### Create observers ###
  ########################
  
  observeEvent(input$changeRoster, {
    f = try(file.choose(), silent=TRUE) # "Cancel" is an error
    if (!is(f, "try-error")) {
      rosterFileName(f)
    }
  })
  
  observeEvent(rosterFileName(), {
    #browser
    rname = rosterFileName()
    if (!is.null(rname)) {
      gc = globalConfig()
      newDir = dirname(rname)
      #browser()
      globalConfig(updateGlobalConfig(gc, list(rosterDirectory=newDir)))
      updateTextInput(session, "gcrosterDirectory", value=newDir)
      newRoster = getRoster(rname, gc)
      roster(newRoster)
    }
  })
  
  observeEvent(roster(), {
    if (!is.null(roster())) {
      shinyjs::html(id="currentRoster", 
                    paste0("<strong>", rosterFileName(), " (",
                           nrow(roster()), " students)</strong>"))
    } else {
      shinyjs::html(id="currentRoster", "")
    }
  })
  
  observeEvent(wd(), {
    wd = wd()
    shinyjs::html(id="currentFolder", 
                  paste0("<strong>", wd, "</strong>"))
  })
  
  observeEvent(codingFiles(), {
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
    #req(input$changeFolder)
    #browser()
    f = try(file.choose(), silent=TRUE)
    if (!is(f, "try-error")) {
      wd(dirname(f))
      #browser()
      gc = initializeGlobalConfig(globalLoc)
      #browser()
      globalConfig(updateGlobalConfig(gc, gc))
      cid = gc[["courseId"]]
      updateTextInput(session, "courseIdText", value=cid)
      roster(getRoster(findRoster(cid, gc$rosterDirectory), gc))
    }
  })
  
  # Construct observer for all inputs related to global configuration
  observeEvent(
    eval(parse(text=paste0("c(",
                           paste0("input$gc", names(GLOBAL_CONFIG_IDS), collapse=", "),
                           ")"))), {
    widgetValues = vector("list", length(GLOBAL_CONFIG_IDS))
    names(widgetValues) = names(GLOBAL_CONFIG_IDS)
    for (w in names(widgetValues))
      widgetValues[[w]] = input[[paste0("gc", w)]]
    gc = globalConfig()
    #browser()
    if (!isTRUE(all.equal(gc[names(widgetValues)], widgetValues))) {
      globalConfig(updateGlobalConfig(gc, widgetValues))
    }
  })
  
  observeEvent(input$gccourseId, {
    browser()
    cid = input$gccourseId
    if (cid != globalConfig()[["courseId"]]) {
      gc = globalConfig()
      #browser()
      globalConfig(updateGlobalConfig(gc, list(courseId = trimws(cid))))
      rname = findRoster(cid)
      rosterFileName(rname)
    }
  })

    
  #########################
  ### Create renderings ###
  #########################
  
  output$rosterInfo = renderPrint({
    roster = roster()
    if (is.data.frame(roster)) {
      cat(nrow(roster), "students")
    } else {
      cat("No roster available")
    }
  })
  

  # output$codeFileList = renderPrint({
  #   cf = codingFiles()
  #   validate(need(cf), "No coding files in current folder")
  #   cat(paste(cf, collapse=", "))n
  
  # })
  
  
} # end server function
