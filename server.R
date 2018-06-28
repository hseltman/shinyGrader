# H. Seltman, Jan 2018

# Server file for shinyGrader app

library(shiny)
require(shinyjs, quietly=TRUE, warn.conflicts=FALSE)

# Server function
function(input, output, session) {
  
  # This app is intended to only be run locally.
  # Stop it if the browser window is closed.
  session$onSessionEnded(stopApp)
  
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
  shinyjs::html(id="currentFolder", 
                paste0("<strong>", getwd(), "</strong>"))
  
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
  
  # Store rubrics and allow observers to know when they change
  rubrics = reactiveVal(staticRubrics)
  
  # Store parsed files and allow observers to know when it changes
  parsedFiles = reactive({
    #browser()
    gc = globalConfig()
    parseFilenames(list.files())
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
    #browser()
    rname = rosterFileName()
    if (!is.null(rname)) {
      gc = globalConfig()
      newDir = dirname(rname)
      #browser()
      globalConfig(updateGlobalConfig(gc, list(rosterDirectory=newDir)))
      updateTextInput(session, "gcrosterDirectory", value=newDir)
      if (rname == "") {
        shinyalert("No roster file", "No roster found in the usual places!",
                   type = "warning")
        roster(NULL)
      } else {
        newRoster = getRoster(rname)
        roster(newRoster)
      }
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
  }, ignoreNULL=FALSE)
  
  observeEvent(input$changeFolder, {
    #req(input$changeFolder)
    f = try(file.choose(), silent=TRUE)
    if (!is(f, "try-error")) {
      newWd = dirname(f)
      wd(newWd)
    }
  })
  
  observeEvent(wd(), {
    newDir = wd()
    rslt = try(setwd(newDir))
    if (is(rslt, "try-error")) {
      shinyalert("Can't change directory", "Can't change directory")
    }
    shinyjs::html(id="currentFolder", 
                  paste0("<strong>", newDir, "</strong>"))
    #browser()
    # Make or read global configuration, always starting with
    # initializeGlobalConfig() in case user edits are bad,
    gc = initializeGlobalConfig(globalLoc)
    text = try(suppressWarnings(readLines(GLOBAL_CONFIG_NAME)))
    if (!is(text, "try-error")) {
      heregc = textToConfigList(text, names(GLOBAL_CONFIG_IDS),
                                source=GLOBAL_CONFIG_NAME)
      if (!is.null(heregc)) {
        gc = updateGlobalConfig(gc, heregc)
        globalConfig(gc)
      }
    }
    
    # Update courseId    
    cid = gc[["courseId"]]
    updateTextInput(session, "courseIdText", value=cid)
    
    # Update rubrics
    rubNew = getRubrics()
    rubrics(rubNew)
  }, ignoreInit=TRUE)

  observeEvent(rubrics(), {
    eval(parse(text=probUpdateCode))
  })
    
  observeEvent(codingFiles(), {
    cf = codingFiles()
    req(cf)
    #updateCheckboxGroupInput(session, "codeFileCheckboxes",
    #                         choices=cf, inline=TRUE)
  })

  observeEvent(input$codeFileCheckboxes, {
    cfSel = input$codeFileCheckboxes
    updateRadioButtons(session, "codefile",
                             choices=cfSel, inline=TRUE)
  })
    
  # Construct observer for all inputs related to global configuration
  observeEvent(
    eval(parse(text=paste0("c(",
                           paste0("input$gc", names(GLOBAL_CONFIG_IDS), collapse=", "),
                           ")"))), {
    browser()
    widgetValues = vector("list", length(GLOBAL_CONFIG_IDS))
    names(widgetValues) = names(GLOBAL_CONFIG_IDS)
    for (w in names(widgetValues))
      widgetValues[[w]] = input[[paste0("gc", w)]]
    gc = globalConfig()
    if (!isTRUE(all.equal(gc[names(widgetValues)], widgetValues))) {
      globalConfig(updateGlobalConfig(gc, widgetValues))
    }
  }, ignoreInit=TRUE)
  
  # Construct observer for all inputs in each problem configuration tab
  for (problem in 1:PROBLEM_COUNT) {
    eval(parse(text=paste0("observeEvent(c(",
                           paste0("input$", problemInputIds[[problem]], collapse=", "),
                           "), {",
                           "shinyjs::enable('submitProblemRubric", problem, "');",
                           "}, ignoreInit=TRUE)")))
  }

  # Construct observer for each 'submitProblemRubric'
  for (problem in 1:PROBLEM_COUNT) {
    assigns = paste0("lst[['", problemInputIds[[problem]], "']] = ",
                           "input$", problemInputIds[[problem]], collapse="\n")
    eval(parse(text=paste0("observeEvent(",
                           paste0("input$", 'submitProblemRubric', problem),
                           ", {",
                           "shinyjs::disable('", paste0('submitProblemRubric', problem), "');",
                           "n = length(problemInputIds[[", problem, "]]);",
                           "lst = vector('list', n);",
                           "names(lst) = problemInputIds[[", problem, "]];",
                           assigns,
                           ";saveRubric(", problem, ", lst)",
                           "}, ignoreInit=TRUE)")))
  }
  
  observeEvent(input$gccourseId, {
    #browser()
    cid = input$gccourseId
    if (cid != globalConfig()[["courseId"]]) {
      gc = globalConfig()
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
