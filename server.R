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
  
  updateTextInput(session=session, inputId="courseId",
                  value=staticGlobalConfig[["courseId"]])
  updateTextInput(session=session, inputId="assignmentName",
                  value=staticGlobalConfig[["assignmentName"]])
  updateTextInput(session=session, inputId="instructorEmail",
                  value=staticGlobalConfig[["instructorEmail"]])
  shinyjs::html(id="currentRoster", 
                paste0("<strong>", staticRosterBaseName, "</strong>"))
  if (!is.null(staticRoster)) {
    shinyjs::html(id="rosterSize", 
                  paste0("(",  nrow(staticRoster), " rows)"))
  }
  shinyjs::html(id="currentFolder", 
                paste0("<strong>", getwd(), "</strong>"))
  shinyjs::html(id="totalPoints", 
                paste0("<strong>", "Total points: ",
                       sum(sapply(1:PROBLEM_COUNT, {
                         function(p) staticRubrics[[p]][["initialPoints"]]
                       })),
                       "</strong>"))
  
  if (length(staticCurrentProblem) == 0) shinyjs::disable("currentProblem")
  
  if (length(grep("[.]zip$", list.files())) > 0) {
    shinyjs::enable("unzip")
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
  # Note: observeEvent() id not called if the new roster is the same as the old,
  # even if it is for a different class, but we need to update the name of the
  # roster, so a serial number is used.
  roster = reactiveValues()
  roster$roster = staticRoster
  roster$serialNum = 0
  
  # Store student "name (email)" and allow observers to know when it changes
  if (is.null(staticRoster)) {
    students = reactiveVal("(none)")
  } else {
    shortEmail = gsub("(.*)(@.*)", "\\1", staticRoster[["Email"]])
    students = reactiveVal(paste0(staticRoster[["Name"]],
                                  " (", staticRoster[["CanvasName"]],
                                  "; ", shortEmail, ")"))
  }
  
  # Store rubrics and allow observers to know when they change
  rubrics = reactiveVal(staticRubrics)
  
  # Store parsed and other files and allow observers to know when it changes
  allFiles = reactiveVal(parseFileNames(list.files.only(), staticCanvasRE))
  
  # Flag indicating that global configuration needs to be updated
  gcDirty = reactiveVal(FALSE)
  
  
  # Vector of active problem numbers (rubric sufficient to allow execution)
  activeProblems = reactiveVal(which(sapply(staticRubrics, isProblemActive)))

  # Files for current problem
  currentFiles = reactiveVal(staticCurrentFiles)
    
  # Store coding files and allow observers to know when it changes
  # codingFiles = reactive({
  #   pf = parsedFiles()
  #   fileList = c()
  #   if (!is.null(pf)) {
  #     temp = pf[tolower(pf$fileExtension) %in% c(".r", ".rmd", ".sas", ".py"), ]
  #     fileList = apply(temp, 1, function(r) paste0(r["baseFileName"],
  #                                                  r["fileExtension"]))
  #   }
  #   return(unique(fileList))
  # })
  
  # Logical reactive values for each rubric's status
  for (problem in 1:PROBLEM_COUNT) {
    eval(parse(text=paste0("rubric", problem, "Dirty = reactiveVal(FALSE)")))
  }

  
    
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
    # A supposed roster file was found, but it may not be a valid roster file,
    # so first try to read it.
    rostName = rosterFileName()
    newRoster = getRoster(rostName, globalConfig()[["instructorEmail"]])

    if (is.null(newRoster)) {
      if (rostName != "") rosterFileName("")
      #shinyalert("No roster file", "No roster found in the usual places!",
      #           type = "warning")
    } else {
      # Update global config's 'rosterDirectory' to point to this roster
      gc = globalConfig()
      rostDir = dirname(rostName)
      globalConfig(updateGlobalConfig(gc, list(rosterDirectory=rostDir)))
      # Update Assignment tab to show this roster's location
      
    }
    
    # Let the app know that there is a new roster (or none)
    roster$roster = newRoster
    roster$serialNum = roster$serialNum + 1
  }, ignoreInit=TRUE)
  
  # Handle new roster
  observeEvent(roster$serialNum, {
    if (is.null(roster$roster)) {
      shinyjs::html(id="currentRoster", "")
      students("(none)")
    } else {
      rost = roster$roster
      shinyjs::html(id="currentRoster", 
                    paste0("<strong>", rosterFileName(), " (",
                           nrow(rost) - 1, " students)</strong>"))
      shortEmail = gsub("(.*)(@.*)", "\\1", rost[["Email"]])
      students(paste0(rost[["Name"]], " (", rost[["CanvasName"]],
                      "; ", shortEmail, ")"))
    }
  }, ignoreNULL=FALSE)
  
  observeEvent(students(), {
    st = students()
    if (length(st) == 1 && st == "(none)") {
      updateSelectInput(session, "selectStudent", label="0 Students (Canvas name; email)",
                        choices=st, selected="(none)")
      shinyjs::disable("selectStudent")
      shinyjs::disable("runOne")
    } else {
      updateSelectInput(session, "selectStudent", 
                        label=paste(length(st) - 1, "Students (Canvas name; email)"),
                        choices=st, selected=st[1])
      shinyjs::enable("selectStudent")
      shinyjs::enable("runOne")
    }
  })
  
  
  observeEvent(input$changeFolder, {
    f = try(file.choose(), silent=TRUE)
    if (!is(f, "try-error")) {
      newWd = dirname(f)
      wd(newWd)
    }
  }, ignoreInit=TRUE)
  
  
  observeEvent(input$fileRefresh, {
    allFiles(parseFileNames(list.files.only(), staticCanvasRE))
    if (length(grep("[.]zip$", list.files())) > 0) {
      shinyjs::enable("unzip")
    } else {
      shinyjs::disable("zip")
    }
  })
  
  
  observeEvent(input$unzip, {
    f = input$unzip$datapath
    unzip(f, overwrite=FALSE, junkpaths=TRUE)
    allFiles(parseFileNames(list.files.only(), staticCanvasRE))
  })
  
  
  observeEvent(wd(), {
    newDir = wd()
    rslt = try(setwd(newDir))
    if (is(rslt, "try-error")) {
      shinyalert("Can't change directory", "Can't change directory")
    }
    shinyjs::html(id="currentFolder", 
                  paste0("<strong>", newDir, "</strong>"))
    
    # parse files in new directory
    allFiles(parseFileNames(list.files.only(), staticCanvasRE))
    
    # Make or read global configuration
    gc = initializeGlobalConfig(globalLoc)

    # Update courseId    
    cid = gc[["courseId"]]
    if (cid != input$courseId)
      updateTextInput(session, "courseId", value=cid)
    
    # Update assignment name and instructor email
    aid = gc[["assignmentName"]]
    if (aid != input$assignmentName)
      updateTextInput(session, "assignmentName", value=aid)
    eid = gc[["instructorEmail"]]
    if (eid != input$instructorEmail)
      updateTextInput(session, "instructorEmail", value=eid)
    
    # Update rubrics
    rubNew = getRubrics()
    rubrics(rubNew)
    if (!any(sapply(rubNew, isProblemActive)))
      shinyjs::disable("currentProblem")
    
  }, ignoreInit=TRUE)

  observeEvent(rubrics(), {
    rubNow = rubrics()
    eval(parse(text=probUpdateCode))
    #source(textConnection(probUpdateCode), local=TRUE)
    total = 0
    for (problem in 1:PROBLEM_COUNT) {
      total = total + rubNow[[problem]][['initialPoints']]
    }
    total = paste0('<strong>Total points: ', total, '</strong>')
    shinyjs::html(id='totalPoints', total)
    
    # It's hard to figure out a way to make the 'submitProblemRubric#' buttons
    # be enabled by a change to any element on the page, and also be disabled
    # after a new set of values is read from a file and the widgets are updated.
    # This half second delay works!
    for (problem in 1:PROBLEM_COUNT) {
      code = paste0("shinyjs::delay(500, shinyjs::disable('submitProblemRubric",
                    problem,"'))")
      eval(parse(text=code))
    }
    
    # Update activeProblems
    activeProblems(which(sapply(rubNow, isProblemActive)))
  })

  # Update input$currentProblem based on activeProblems()
  observeEvent(activeProblems(), {
    ap = activeProblems()
    if (length(ap) > 0) {
      updateRadioButtons(session, "currentProblem",
                        choices=paste("Problem", (1:PROBLEM_COUNT)[ap]),
                        inline=TRUE)
      shinyjs::enable("currentProblem")
    } else {
      updateRadioButtons(session, "currentProblem", choices="Problem 1",
                         inline=TRUE)
      # Note: it seems disable does not work for length(choices)==1
      shinyjs::disable("currentProblem")
    }
  }, ignoreInit=TRUE)
  
  
  observeEvent(c(roster$serialNum, input$selectStudent, input$currentProblem,
                 rubrics(), allFiles()), {
    rubNow = rubrics()
    id = selectStudentToId(input$selectStudent, roster$roster)
    if (is.null(id) || is.null(rubNow) || !any(sapply(rubNow, isProblemActive))) {
      currentFiles(NULL)
    } else {
      prob = as.numeric(substring(input$currentProblem, 9))
      currentFiles(findCurrentFiles(id, allFiles(), rubNow[[prob]]))
    }
  }, ignoreInit=TRUE)
    
    

  # observeEvent(codingFiles(), {
  #   cf = codingFiles()
  #   req(cf)
  #   #updateCheckboxGroupInput(session, "codeFileCheckboxes",
  #   #                         choices=cf, inline=TRUE)
  # })

  # ????
  # observeEvent(input$codeFileCheckboxes, {
  #   cfSel = input$codeFileCheckboxes
  #   updateRadioButtons(session, "codefile",
  #                            choices=cfSel, inline=TRUE)
  # }, ignoreInit=TRUE)
    
  # Handle change in courseId
  observeEvent(input$courseId, {
    cid = input$courseId
    gcDirty(TRUE)
    rname = findRoster(trimws(cid))
    rosterFileName(rname)
  }, ignoreInit=TRUE)
  
  # Construct observers for inputs related to global configuration
  observeEvent(c(input$assignmentName, input$instructorEmail), {
    gcDirty(TRUE)
  }, ignoreInit=TRUE)
  
  # Update global configuration if values change
  observeEvent(gcDirty(), {
    if (gcDirty()) {
      widgetValues = vector("list", length(GLOBAL_CONFIG_IDS))
      names(widgetValues) = names(GLOBAL_CONFIG_IDS)
      gc = globalConfig()
      for (w in names(widgetValues)) {
        if (w == "rosterDirectory") {
          widgetValues[[w]] = gc[[w]]
        } else {
          widgetValues[[w]] = trimws(input[[w]])
        }
      }
      if (!isTRUE(all.equal(gc[names(widgetValues)], widgetValues))) {
        globalConfig(updateGlobalConfig(gc, widgetValues))
      }
      gcDirty(FALSE)
    }
  }, ignoreInit=TRUE)
  
  # Construct observer for all inputs in each problem configuration tab
  for (problem in 1:PROBLEM_COUNT) {
    eval(parse(text=c("observeEvent(c(",
                      paste0("input$", problemInputIds, problem, collapse=", "),
                      "), {",
                      paste0("shinyjs::enable('submitProblemRubric", problem, "')"),
                      "}, ignoreInit=TRUE)")))
  }

  # Construct observer for each 'submitProblemRubric'
  for (problem in 1:PROBLEM_COUNT) {
    assigns = paste0("lst[['", problemInputIds, "']] = ",
                     "input$", problemInputIds, problem)
    eval(parse(text=c(paste0("observeEvent(input$", 'submitProblemRubric', problem, ", {"),
                      paste0("shinyjs::disable('submitProblemRubric", problem, "')"),
                      paste0("n = length(problemInputIds)"),
                      "lst = vector('list', n)",
                      paste0("names(lst) = problemInputIds"),
                      assigns,
                      paste0("rubric", problem, "Dirty(TRUE)"),
                      paste0("saveRubric(", problem, ", lst)"),
                      "}, ignoreInit=TRUE)")))
  }
  
  # Save rubrics if any are dirty
  eval(parse(text=c("observeEvent(",
                    paste0("c(", paste0("rubric", 1:PROBLEM_COUNT, "Dirty()", collapse=", "),
                           "), {"),
                    "rnames = problemInputIds",
                    "rubNew = vector('list', PROBLEM_COUNT)",
                    "n = length(problemInputIds)",
                    "for (problem in 1:PROBLEM_COUNT) {",
                    "  rubNew[[problem]] = vector('list', n)",
                    "  names(rubNew[[problem]]) = rnames",
                    "  for (i in 1:n) {",
                    "    widgNum = paste0(rnames[i], problem)",
                    "    eval(parse(text=paste0('rubNew[[problem]][[rnames[i]]] = input$', widgNum)))",
                    "  }",
                    "}",
                    "rubrics(rubNew)",
                    "}, ignoreInit=TRUE)")
  ))
  
  

    
  #########################
  ### Create renderings ###
  #########################
  
  output$rosterInfo = renderPrint({
    roster = roster$roster
    if (is.data.frame(roster)) {
      n = nrow(roster)
      cat(n, " student", ifelse(n==1, "", "s"))
    } else {
      cat("No roster available")
    }
  })
  

  output$filesForOne = renderPrint({
    req(currentFiles())
    cf = currentFiles()
    
    runDf = cf$runDf
    if (is.null(runDf)) {
      cat("No run file found.\n")
    } else if (nrow(runDf) == 2) {
      cat("Error: Multiple run files found: ", paste(runDf$inName, sep=", "), "\n")
    } else {
      cat("Run file: ", runDf$inName,
          ifelse(runDf$caseFlag, "(case error)\n",
                 ifelse(runDf$looseFlag, "(naming error)\n", "\n")))
    }
    
    reqDf = cf$reqDf
    reqFiles = reqDf$inName
    if (length(reqFiles) > 0) {
      cat("\nRequired files:\n")
      for (ii in seq(along=reqFiles)) {
        cat(reqFiles[ii],
            ifelse(reqDf$caseFlag[ii], "(case error)\n",
                   ifelse(reqDf$looseFlag[ii], "(naming error)\n", "\n")))
      }
    }
    
    optDf = cf$optDf
    optFiles = optDf$inName
    if (length(optFiles) > 0) {
      cat("\nOptional files:\n")
      for (ii in seq(along=optFiles)) {
        cat(optFiles[ii],
            ifelse(optDf$caseFlag[ii], "(case error)\n",
                   ifelse(optDf$looseFlag[ii], "(naming error)\n", "\n")))
      }
    }
    
    if (length(cf$reqMissing) > 0) {
      cat("\nMissing requirements:\n")
      cat(cf$reqMissing, sep="\n")
    }
  })
  
  
  # output$codeFileList = renderPrint({
  #   cf = codingFiles()
  #   validate(need(cf), "No coding files in current folder")
  #   cat(paste(cf, collapse=", "))n
  # })
  
  
} # end server function
