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
  
  updateTextInput(session=session, inputId="gccourseId",
                  value=staticGlobalConfig[["courseId"]])
  updateTextInput(session=session, inputId="gcrosterDirectory",
                  value=staticGlobalConfig[["rosterDirectory"]])
  updateTextInput(session=session, inputId="gcassignmentName",
                  value=staticGlobalConfig[["assignmentName"]])
  updateTextInput(session=session, inputId="gcinstructorEmail",
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
                         function(p) staticRubrics[[p]][[paste0("initialPoints", p)]]
                       })),
                       "</strong>"))

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
  
  # Store parsed files and allow observers to know when it changes
  allCanvasFiles = reactive({
    wd = wd()  # control when this runs
    return(parseFilenames(list.files()))
  })
  
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
    if (rostName == "") {
      newRoster = NULL
    } else {
      newRoster = getRoster(rostName, globalConfig()[["instructorEmail"]])
    }
    
    
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
      updateTextInput(session, "gcrosterDirectory", value=rostDir)
    }
    
    # Let the app know that there is a new roster (or none)
    roster(newRoster)
  })
  
  observeEvent(roster(), {
    if (is.null(roster())) {
      shinyjs::html(id="currentRoster", "")
      students("(none)")
    } else {
      shinyjs::html(id="currentRoster", 
                    paste0("<strong>", rosterFileName(), " (",
                           nrow(roster()), " students)</strong>"))
      rost = roster()
      shortEmail = gsub("(.*)(@.*)", "\\1", rost[["Email"]])
      students(paste0(rost[["Name"]], " (", rost[["CanvasName"]],
                      "; ", shortEmail, ")"))
    }
  }, ignoreNULL=FALSE)
  
  observeEvent(students(), {
    st = students()
    if (length(st) == 1 && st == "(none)") {
      updateSelectInput(session, "selectStudent", choices=st, selected="(none)")
      shinyjs::disable("selectStudent")
      shinyjs::disable("runOne")
    } else {
      updateSelectInput(session, "selectStudent", choices=st, selected=st[1])
      shinyjs::enable("selectStudent")
      shinyjs::enable("runOne")
    }
  })
  
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

    # Make or read global configuration
    gc = initializeGlobalConfig(globalLoc)

    # Update courseId    
    cid = gc[["courseId"]]
    updateTextInput(session, "courseIdText", value=cid)
    
    # Update rubrics
    rubNew = getRubrics()
    rubrics(rubNew)
    
  }, ignoreInit=TRUE)

  observeEvent(rubrics(), {
    rubNow = rubrics()
    eval(parse(text=probUpdateCode))
    #eval(parse(text=totalPointsCode))
    #source(textConnection(probUpdateCode), local=TRUE)
    #source(textConnection(totalPointsCode), local=TRUE)
    total = 0
    total = total + rubNow[[1]][['initialPoints1']]
    total = total + rubNow[[2]][['initialPoints2']]
    total = total + rubNow[[3]][['initialPoints3']]
    total = paste0('<strong>Total points: ', total, '</strong>')
    shinyjs::html(id='totalPoints', total)
    # It's hard to figure out a way to make the 'submitProblemRubric#' buttons
    # be enabled by a change to any element on the page, and also be disabled
    # after a new set of values is read from a file and the widgets are updated.
    # This half second delay works!
    for (problem in 1:PROBLEM_COUNT)
      eval(parse(text=paste0("shinyjs::delay(500, shinyjs::disable('submitProblemRubric", problem,"'))")))
  })
    
  # observeEvent(codingFiles(), {
  #   cf = codingFiles()
  #   req(cf)
  #   #updateCheckboxGroupInput(session, "codeFileCheckboxes",
  #   #                         choices=cf, inline=TRUE)
  # })

  observeEvent(input$codeFileCheckboxes, {
    cfSel = input$codeFileCheckboxes
    updateRadioButtons(session, "codefile",
                             choices=cfSel, inline=TRUE)
  }, ignoreInit=TRUE)
    
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
    if (!isTRUE(all.equal(gc[names(widgetValues)], widgetValues))) {
      globalConfig(updateGlobalConfig(gc, widgetValues))
    }
  }, ignoreInit=TRUE)
  
  # Construct observer for all inputs in each problem configuration tab
  for (problem in 1:PROBLEM_COUNT) {
    eval(parse(text=c("observeEvent(c(",
                      paste0("input$", problemInputIds[[problem]], collapse=", "),
                      "), {",
                      paste0("shinyjs::enable('submitProblemRubric", problem, "')"),
                      "}, ignoreInit=TRUE)")))
  }

  # Construct observer for each 'submitProblemRubric'
  for (problem in 1:PROBLEM_COUNT) {
    assigns = paste0("lst[['", problemInputIds[[problem]], "']] = ",
                     "input$", problemInputIds[[problem]]) #, collapse="\n")
    eval(parse(text=c(paste0("observeEvent(input$", 'submitProblemRubric', problem, ", {"),
                      paste0("shinyjs::disable('submitProblemRubric", problem, "')"),
                      paste0("n = length(problemInputIds[[", problem, "]])"),
                      "lst = vector('list', n)",
                      paste0("names(lst) = problemInputIds[[", problem, "]]"),
                      assigns,
                      paste0("rubric", problem, "Dirty(TRUE)"),
                      paste0("saveRubric(", problem, ", lst)"),
                      "}, ignoreInit=TRUE)")))
  }
  
  # Save rubrics if any are dirty
  eval(parse(text=c("observeEvent(",
                    paste0("c(", paste0("rubric", 1:PROBLEM_COUNT, "Dirty()", collapse=", "),
                           "), {"),
                    "rnames = names(rubricDefaults)",
                    "rubNew = vector('list', PROBLEM_COUNT)",
                    "n = length(rubricDefaults)",
                    "for (problem in 1:PROBLEM_COUNT) {",
                    "  rubNew[[problem]] = vector('list', n)",
                    "  names(rubNew[[problem]]) = paste0(rnames, problem)",
                    "  for (i in 1:n) {",
                    "    widgNum = paste0(rnames[i], problem)",
                    "    eval(parse(text=paste0('rubNew[[problem]][[i]] = input$', widgNum)))",
                    "  }",
                    "}",
                    "rubrics(rubNew)",
                    "}, ignoreInit=TRUE)")
  ))
  
  
  observeEvent(input$gccourseId, {
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
  

  output$filesForOne = renderPrint({
    who = input$selectStudent
    if (is.null(who) || who == "(none)") {
      cat("(none)")
    } else {
      acf = isolate(allCanvasFiles())
      rost = isolate(roster())
      who = gsub("(.*)([ ][(].*)", "\\1", who)
      id = rost$ID[rost$Name == who]
      if (length(id) != 1) {
        shinyjs::shinyalert("No match in roster,",
                            paste0(who, "is not in the roster"))
        cat("(none)")
      } else {
        files = acf$original[acf$studentIdNumber == id]
        if (length(files) == 0) {
          cat("This student has no files.")
        } else {
          cat("Files:\n")
          cat(files, sep="\n")
        }
      }
    }
  })
  
  
  # output$codeFileList = renderPrint({
  #   cf = codingFiles()
  #   validate(need(cf), "No coding files in current folder")
  #   cat(paste(cf, collapse=", "))n
  # })
  
  
} # end server function
