# H. Seltman, August 2018

# Server file for shinyGrader app

library(shiny)
require(shinyjs, quietly=TRUE, warn.conflicts=FALSE)

# Server function
function(input, output, session) {
  addResourcePath("shinyGrader", path.expand("~"))
  
  # This app is intended to only be run locally.
  # Stop it if the browser window is closed.
  session$onSessionEnded(function() {
    # Check rubric#Dirty() and gcDirty() and save values if dirty.
    for (probNum in 1:PROBLEM_COUNT) {
      if (eval(parse(text=paste0("isolate(rubric", probNum, "Dirty())")))) {
        rubNow = rubricToList(probNum)
        saveRubric(probNum, rubNow)
      }
    }
    if (isolate(gcDirty())) {
      widgetValues = vector("list", length(GLOBAL_CONFIG_IDS))
      names(widgetValues) = names(GLOBAL_CONFIG_IDS)
      gc = isolate(globalConfig())
      for (w in names(widgetValues)) {
        if (w == "rosterDirectory") {
          widgetValues[[w]] = gc[[w]]
        } else {
          widgetValues[[w]] = trimws(isolate(input[[w]]))
        }
      }
      if (!isTRUE(all.equal(gc[names(widgetValues)], widgetValues))) {
        globalConfig(updateGlobalConfig(gc, widgetValues))
      }
    }
    
    stopApp()
  })
  
  # Convert UI values for one problem rubric to a list
  rubricToList = function(probNum) {
    n = length(problemInputIds)
    lst = vector('list', n)
    names(lst) = problemInputIds
    for (id in problemInputIds) {
      val = eval(parse(text=paste0("isolate(input[['", id, probNum, "']])")))
      if (is.character(val)) {
        eval(parse(text=paste0("lst[['", id, "']] = trimws(val)")))
      } else {
        eval(parse(text=paste0("lst[['", id, "']] = val")))
      }
    }
    return(lst)
  }
  
  # Cheat for dualAlert() in "helpers.R"
  assign("shinyIsRunning", TRUE, env=.GlobalEnv)
  
  
  ### Setup for current directory (things that will change if ###
  ### the user changes directories).                         ###
  
  
  # Create initial status object
  # noTime = numeric(0)
  # class(noTime) = "POSIXct"
  # status = data.frame(student=I(character(0)),
  #                     problem=integer(0),
  #                     lastRun=noTime,
  #                     newestFile=noTime,
  #                     configTime=noTime,
  #                     status=integer(0))
  # 
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
  
  # Path for current student
  thisPath = reactiveVal(staticThisPath)
    
  # Logical reactive values for each rubric's status
  for (problem in 1:PROBLEM_COUNT) {
    eval(parse(text=paste0("rubric", problem, "Dirty = reactiveVal(FALSE)")))
  }
  rm(problem)

  # Keep track of last tab
  lastTab = reactiveVal("Assignment")

  # Possible html output
  htmlFile = reactiveVal(NULL)

  # Current input$gradeViewChoices is more up-to-date than
  # input$gradeViewChoices when updateRadioButtons() is used
  # for "gradeViewChoices".  This is needed to make the
  # render work for "gradeViewOutput" after updating "gradeViewChoices".
  currentGradeViewChoices = reactiveVal("(none)")
  
  
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
    if (rostName != "fakeRoster") {
      newRoster = getRoster(rostName, globalConfig()[["instructorEmail"]])
      
      
      if (is.null(newRoster)) {
        if (rostName != "") rosterFileName("")
      } else {
        if (attr(newRoster, "file") == file.path(getwd(), "fakeRoster")) {
          rostDir = ""
          rosterFileName("fakeRoster")
        } else {
          rostDir = dirname(rostName)
        }
        # Update global config's 'rosterDirectory' to point to this roster
        gc = globalConfig()
        globalConfig(updateGlobalConfig(gc, list(rosterDirectory=rostDir)))
        # Update Assignment tab to show this roster's location
        
      }

      choices = as.character(1:nrow(newRoster))
      names(choices) = newRoster$selectText
      updateSelectInput(session, "selectStudent", 
                        label=paste(length(choices) - 1, "Students (Canvas name; email)"),
                        choices=choices)
      
      # Let the app know that there is a new roster
      roster$roster = newRoster
      roster$serialNum = roster$serialNum + 1
    }
  }, ignoreInit=TRUE)

  
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
    
    # Update activeProblems
    activeProblems(which(sapply(rubNow, isProblemActive)))
  }, ignoreInit=TRUE)

  # Update input$currentProblem based on activeProblems()
  observeEvent(activeProblems(), {
    ap = activeProblems()
    if (length(ap) > 0) {
      choices = as.character((1:PROBLEM_COUNT)[ap])
      updateRadioButtons(session, "currentProblem",
                        choiceNames=paste("Problem", choices),
                        choiceValues=choices,
                        selected = choices[1],
                        inline=TRUE)
      shinyjs::enable("currentProblem")
    } else {
      updateRadioButtons(session, "currentProblem", choiceNames="Problem 1",
                         choiceValues="1",
                         selected="Problem 1", inline=TRUE)
      # Note: it seems disable does not work for length(choices)==1
      shinyjs::disable("currentProblem")
    }
  }, ignoreInit=TRUE)
  
  observeEvent(input$debug, browser())

  
  # Note allFiles()->rubrics()
  observeEvent(rubrics(), {
    rubNow = rubrics()
    if (is.null(rubNow) || !any(sapply(rubNow, isProblemActive))) {
      currentFiles(NULL)
    } else {
      probNum = as.numeric(input$currentProblem)
      studentInfo = roster$roster[as.numeric(input$selectStudent), ]
      cf = findCurrentFiles(studentInfo$ID, allFiles(), rubNow[[probNum]])
      currentFiles(cf)
    }
  }, ignoreInit=TRUE)
  
    
  # Note roster$serialNum->input$selectStudent
  observeEvent(c(input$selectStudent, input$currentProblem), {
    rubNow = rubrics()
    probNum = as.numeric(input$currentProblem)
    rostNow = roster$roster
    studentInfo = rostNow[as.numeric(input$selectStudent), ]
    
    cf = findCurrentFiles(studentInfo$ID, allFiles(), rubNow[[probNum]])
    currentFiles(cf)
    if (is.null(rubNow) || !any(sapply(rubNow, isProblemActive))) {
      browser()
      currentFiles(NULL)
      path = NULL
      shinyjs::disable("analyzeCode")
      shinyjs::disable("runCode")
      shinyjs::disable("analyzeOutput")
      htmlFile(NULL)
    } else {
      path = setupSandbox(studentInfo$shortEmail, cf, probNum)
      shinyjs::html(id="sandboxVersion", 
                    paste("<strong>Version =", basename(path), "</strong>"))
      checks = checkEnables(path, cf, probNum)
      # Note: toggleState() does not allow 'condition' to have names
      shinyjs::toggleState(id="analyzeCode", condition=as.vector(checks["analyzeCode"]))
      shinyjs::toggleState(id="runCode", condition=as.vector(checks["runCode"]))
      shinyjs::toggleState(id="analyzeOutput", condition=as.vector(checks["analyzeOutput"]))
      runFile = cf$runDf[1, "outName"]
      if (is.null(runFile)) {
        htmlFile(NULL)
      } else {
        outFile = file.path(path, changeExtension(runFile, "html"))
        if (file.exists(outFile)) {
          htmlFile(outFile)
        } else {
          htmlFile(NULL)
        }
      }
    }

    thisPath(path)
    updateGradeViewChoice(cf, path)
  }, ignoreInit=TRUE)


  # Task to be done when tabs are selected/deselected
  # Assure that when user moves away from the problem tabs, any dirty
  # rubrics are saved.
  observeEvent(input$outerTabs, {
    this = input$outerTabs
    last = lastTab()
    studentInfo = roster$roster[as.numeric(input$selectStudent), ]

    if (last == "Problems") {
      isDirty = sapply(1:PROBLEM_COUNT,
                       function(p) eval(parse(text=paste0("rubric", p, "Dirty()"))))
      if (any(isDirty)) {
        rubNow = rubrics()
        for (probNum in which(isDirty)) {
          rubNow[[probNum]] = rubricToList(probNum)
        }
        rubrics(rubNow)

        for (probNum in which(isDirty)) {
          saveRubric(probNum, rubNow[[probNum]])
          eval(parse(text=paste0("rubric", probNum, "Dirty(FALSE)")))
        }
      } # end if any(isDirty)
    } else if (last == "Assignment") { # end if leaving the Problems tab
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
        
        # Handle change in instructorEmail (affects first line of roster)
        rost = roster$roster
        email = trimws(input$instructorEmail)
        if (email != rost[1, "Email"]) {
          if (email == "") {
            rost[1, "Email"] = "solution@fake.edu"
          } else {
            rost[1, "Email"] = email
          }
          studentEmail = rost[1, "Email"]
          roster$serialNum = roster$serialNum + 1
          roster$roster = rost
        }
        
        gcDirty(FALSE)
      }
    }
    
    if (this == "Grading") {
      rubNow = rubrics()
      probNum = as.numeric(input$currentProblem)
      cf = findCurrentFiles(studentInfo$ID, allFiles(), rubNow[[probNum]])
      if (is.null(rubNow) || !any(sapply(rubNow, isProblemActive))) {
        currentFiles(NULL)
        path = setupSandbox(studentInfo$shortEmail, cf, probNum)
        thisPath(path)
        shinyjs::disable("analyzeCode")
        shinyjs::disable("runCode")
        shinyjs::disable("analyzeOutput")
      } else {
        currentFiles(cf)
        path = setupSandbox(studentInfo$shortEmail, cf, probNum)
        thisPath(path)
        checks = checkEnables(path, cf, probNum)
        # Note: 'condition' cannot have names
        shinyjs::toggleState(id="analyzeCode", condition=as.vector(checks["analyzeCode"]))
        shinyjs::toggleState(id="runCode", condition=as.vector(checks["runCode"]))
        shinyjs::toggleState(id="analyzeOutput", condition=as.vector(checks["analyzeOutput"]))
      }
      shinyjs::html(id="sandboxVersion", 
                    paste("<strong>Version =", basename(path), "</strong>"))
      updateGradeViewChoice(cf, path)
    }
    
    lastTab(this)
  }, ignoreNULL=TRUE, ignoreInit=TRUE, priority=10)
  
  
  updateGradeViewChoice = function(currentFiles, path) {
    if (is.null(currentFiles)) {
      browser()
      updateRadioButtons("gradeViewChoice", choiceNames="(none)", choiceValues="(none)",
                         selected="")
    } else {
      runFile = currentFiles$runDf$outName
      files = c(runFile,
                currentFiles$reqDf$outName[currentFiles$reqDf$directory == "."],
                currentFiles$optDf$outName[currentFiles$optDf$directory == "."])
      if (file.exists(file.path(path, "codeProblems.RData"))) {
        files = c(files, "Code Analysis")
      }
      if (!is.null(runFile)) {
        htmlFile = changeExtension(runFile, "html")
        if (file.exists(file.path(path, htmlFile))) {
          files = c(files, htmlFile)
        } else {
          outFile = changeExtension(runFile, "out")
          if (file.exists(file.path(path, outFile))) {
            files = c(files, outFile)
          }
        }
      }
      if (file.exists(file.path(path, "outputProblems.RData"))) {
        files = c(files, "Output Analysis")
      }
      if (is.null(files)) files = "(none)"
      currentGradeViewChoices(files)
      updateRadioButtons(session, "gradeViewChoice", choices=files, 
                         selected = files[1], inline=TRUE)
    }
  }
  
  
  # Handle change in courseId
  observeEvent(input$courseId, {
    cid = input$courseId
    gcDirty(TRUE)
    rname = findRoster(trimws(cid))
    rosterFileName(rname)
  }, ignoreInit=TRUE)
  
  # Construct observer for assignmentName
  observeEvent(input$assignmentName, {
    gcDirty(TRUE)
  }, ignoreInit=TRUE)

  # Construct observer for instructorEmail
  observeEvent(input$instructorEmail, {
    gcDirty(TRUE)
  }, ignoreInit=TRUE)
  
  # Construct observer for all inputs in each problem configuration tab
  for (problem in 1:PROBLEM_COUNT) {
    eval(parse(text=c("observeEvent(c(",
                      paste0("input$", problemInputIds, problem, collapse=", "),
                      "), {",
                      paste0("shinyjs::enable('submitProblemRubric", problem, "')"),
                      "}, ignoreInit=TRUE)")))
  }
  rm(problem)

  # For each problem rubric, construct observer for all elements
  for (problem in 1:PROBLEM_COUNT) {
    obsList = paste(paste0("input$", problemInputIds, problem), collapse=", ")
    eval(parse(text=c(paste0("observeEvent(c(", obsList, "), {"),
                      paste0("rubric", problem, "Dirty(TRUE)"),
                      "}, ignoreInit=TRUE)")))
  }
  rm(problem)
  
  # Check one student's code
  observeEvent(input$analyzeCode, {
    req(thisPath(), currentFiles())
    path = thisPath()
    cf = currentFiles()
    probNum = as.numeric(input$currentProblem)
    rubric = rubrics()[[probNum]]
    cc = checkCode(path, cf, rubric)
    if (!is.null(cc)) {
      shinyjs::disable("analyzeCode")
      updateGradeViewChoice(cf, path)
    }
    print(cc)
    print(attr(cc, "extra"))
  }, ignoreInit=TRUE)
    
  # Run one student's code
  observeEvent(input$runCode, {
    req(thisPath(), currentFiles(), roster$roster)
    path = thisPath()
    cf = currentFiles()
    #studentInfo = roster$roster[as.numeric(input$selectStudent), ]
    probNum = as.numeric(input$currentProblem)
    rubric = rubrics()[[probNum]]
    rtn = runCode(path, cf$runDf$outName)
    if (rtn) {
      shinyjs::disable("runCode")
      shinyjs::enable("analyzeOutput")
      outFile = attr(rtn, "outFile")
      exitCode = attr(rtn, "exitCode")
      if (substring(outFile, nchar(outFile) - 3) == "html") {
        htmlName = file.path(path, outFile)
        htmlFile(htmlName)
      } else {
        htmlFile(NULL)
      }
      updateGradeViewChoice(cf, path)
      print(file.info(file.path(path, outFile)))
    }
  }, ignoreInit=TRUE)
  
  # Check one student's output
  observeEvent(input$analyzeOutput, {
    req(thisPath(), currentFiles())
    path = thisPath()
    cf = currentFiles()
    probNum = as.numeric(input$currentProblem)
    rubric = rubrics()[[probNum]]
    co = checkOutput(path, cf, rubric)
    if (!is.null(co)) shinyjs::disable("analyzeOutput")
    updateGradeViewChoice(cf, path)
    print(co)
  }, ignoreInit=TRUE)

  # Run all students
  observeEvent(input$runAllStudents, {
    probNum = as.numeric(input$currentProblem)
    rubNow = rubrics()[[probNum]]
    rostNow = roster$roster
    for (studNum in 1:nrow(rostNow)) {
      studInfo = rostNow[studNum, ]
      cf = findCurrentFiles(studInfo$ID, allFiles(), rubNow)
      path = setupSandbox(studInfo$shortEmail, cf, probNum)

      # Check code
      cc = checkCode(path, cf, rubNow)
      print(cc)
      print(attr(cc, "extra"))
      
      # Run code
      if (is.null(cc) || is.null(cf$runDf$outName)) {
        rc = FALSE
      } else {
        rc = runCode(path, cf$runDf$outName)
      }
      
      if (rc) {
        outFile = attr(rc, "outFile")
        exitCode = attr(rc, "exitCode")
        if (substring(outFile, nchar(outFile) - 3) == "html") {
          htmlName = file.path(path, outFile)
          htmlFile(htmlName)
        } else {
          htmlFile(NULL)
        }
        print(file.info(file.path(path, outFile)))
      }
      
      # Check output
      if (rc) {
        co = checkOutput(path, cf, rubNow)
        print(co)
      }
    } # end for each student
  }) # end observe runAllStudents
  
  
  
  # Save default rubrics  
  for (probNum in 1:PROBLEM_COUNT) {
    eval(parse(text=c(paste0("observeEvent(input$saveRubric", probNum, "AsDefault, {"),
                      paste0("rubNow = rubricToList(", probNum, ")"),
                      paste0("saveRubric(", probNum, ", rubNow, home=TRUE)"),
                      "})")))
  }
  rm(probNum)
  
  # input$selectStudent's next and prior buttons
  observeEvent(input$priorStudent, {
    stNum = as.numeric(input$selectStudent)
    if (stNum > 1) {
      #freezeReactiveVal(thisPath)
      updateSelectInput(session, "selectStudent", selected=as.character(stNum - 1))
    }
    shinyjs::toggleState("priorStudent", condition=(stNum > 1))
  }, ignoreInit=TRUE)
  
  observeEvent(input$nextStudent, {
    N = nrow(roster$roster)
    stNum = as.numeric(input$selectStudent)
    if (stNum < N) {
      #freezeReactiveVal(thisPath)
      #freezeReactiveValue(input, "selectStudent")
      updateSelectInput(session, "selectStudent", selected=as.character(stNum + 1))
    }
    shinyjs::toggleState("nextStudent", condition=(stNum < N))
  }, ignoreInit=TRUE)
  
  
  
  
  
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
    validate(need(currentFiles(), "Rubric does not have a run file"))
    cf = currentFiles()
    
    runDf = cf$runDf
    if (is.null(runDf)) {
      cat("No run file (", cf$runMissing, ") found.\n", sep="")
    } else if (nrow(runDf) == 2) {
      cat("Error: Multiple run files found: ", paste(runDf$inName, sep=", "), "\n")
    } else {
      cat("Run file: ",
          ifelse(runDf$directory == ".", "", paste0(runDf$directory, "/")),
                 runDf$inName,
          ifelse(runDf$caseFlag, " (case error)\n",
                 ifelse(runDf$looseFlag, " (naming error)\n", "\n")),
          sep="")
    }
    
    reqDf = cf$reqDf
    reqFiles = reqDf$inName
    if (length(reqFiles) > 0) {
      cat("\nRequired files:\n")
      for (ii in seq(along=reqFiles)) {
        cat(ifelse(reqDf$directory[ii] == ".", "", paste0(reqDf$directory[ii], "/")),
            reqFiles[ii],
            ifelse(reqDf$caseFlag[ii], " (case error)\n",
                   ifelse(reqDf$looseFlag[ii], " (naming error)\n", "\n")),
            sep="")
      }
    }
    
    optDf = cf$optDf
    optFiles = optDf$inName
    if (length(optFiles) > 0) {
      cat("\nOptional files:\n")
      for (ii in seq(along=optFiles)) {
        cat(ifelse(optDf$directory[ii] == ".", "", paste0(optDf$directory[ii], "/")),
            optFiles[ii],
            ifelse(optDf$caseFlag[ii], " (case error)\n",
                   ifelse(optDf$looseFlag[ii], " (naming error)\n", "\n")),
            sep="")
      }
    }
    
    if (length(cf$reqMissing) > 0) {
      cat("\nMissing requirements:\n")
      cat(cf$reqMissing, sep="\n")
    }
  })

  output$canvasFiles = renderPrint({
    cf = allFiles()$Canvas$submitName
    if (length(cf) == 0) {
      cat("No Canvas files\n")
    } else {
      cat("Canvas files:\n")
      ucf = split(cf, tolower(cf))
      for (fnames in ucf) {
        oneTable = table(fnames)
        if (length(oneTable) == 1) {
          cat(names(oneTable), " (", as.vector(oneTable), ")\n", sep="")
        } else {
          oneTable = oneTable[order(oneTable, decreasing=TRUE)]
          otNames = names(oneTable)
          otCounts = as.vector(oneTable)
          cat(otNames[1], " (", otCounts[1], ")  [", 
              paste(paste0(otNames[-1], " (", otCounts[-1], ")"), collapse=", "),
              "]\n", sep="")
        }
      }
    }
  })
    
  # View files in "Grader" tab
  output$gradeViewOutput = renderUI({
    path = thisPath()
    cgvc = currentGradeViewChoices()
    what = input$gradeViewChoice
    if (! what %in% cgvc) what = cgvc[1]
    validate(need(what != "(none)", "(nothing to view)"))
    extension = gsub("(.*)([.])(.*)", "\\3", what)
    if (what == "Code Analysis") {
      tgs = codeAnalysisToTags(path, what)
      return(div(tgs, p(paste("dock", attr(tgs, "dock")))))
    } else if (what == "Output Analysis") {
      tgs = outputAnalysisToTags(path, what)
      return(div(tgs, p(paste("dock", attr(tgs, "dock")))))
    } else if (extension == "html") {
      # Note: this depends on "addResourcePath("shinyGrader", file.expand("~"))
      tgs = tags$iframe(src = file.path("/shinyGrader", 
                                        substring(wd(), nchar(path.expand("~")) + 2),
                                        path, what),
                                          style="width:100%;",
                        id="iframe", height = "500px")
      return(tgs)
    } else {
      text = try(suppressWarnings(readLines(file.path(path, what))), silent=TRUE)
      if (is(text, "try-error")) {
        dualAlert("Grading View Error", paste0(file.path(path, what), " not readable text"))
        return(p("not viewable"))
      } else {
        tgs = do.call(shiny::tags$pre, as.list(text))
        return(tgs)
      }
    } # end codeAnalysis vs. outputAnalysis vs. html vs. plain text
  }) # end render "gradeViewOutput"
  
} # end server function
