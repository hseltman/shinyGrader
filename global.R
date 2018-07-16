# H. Seltman, Jan 2018

# Perform setup
source("helpers.R") # many misc. functions
source("setup.R") # constants and global config functions
source("genProblemTabs.R") # code to generate UI "Problems" tab

shinyIsRunning=FALSE

# Set variables to match pre-defined environmental variables
envLoc = Sys.getenv("SHINYGRADER_GLOBAL_CONFIG")
if (envLoc != "" && is.na(file.info(envLoc)$mode)) {
  cat(paste("Environmental variable 'SHINYGRADER_GLOBAL_CONFIG' has",
            "an invalid value (not a valid directory):", envLoc))
  envLoc = ""
}
globalLoc = if (envLoc == "") Sys.getenv("HOME") else envLoc
if (globalLoc == "") stop("cannot get name of HOME directory")

# Complex logic is best accomplished by calling initializeGlobalConfig()
# and storing these initial results in 'staticGlobalConfig' here, and
# then using it in the 'server.R' and 'ui.R' files to initialize a
# reactiveVal().  The same applies to the initial roster.
staticGlobalConfig = initializeGlobalConfig(globalLoc)
staticRosterFileName = findRoster(staticGlobalConfig[["courseId"]],
                                  staticGlobalConfig[["rosterDirectory"]])
if (staticRosterFileName == "") {
  staticRosterDirectory = ""
  staticRoster = NULL
  staticGlobalConfig = updateGlobalConfig(staticGlobalConfig,
                                          list(rosterDirectory=getwd()))
  staticRosterBaseName = ""
} else {
  staticRosterDirectory = dirname(staticRosterFileName)
  staticRoster = getRoster(staticRosterFileName, staticGlobalConfig[["instructorEmail"]])
  if (is.null(staticRoster)) {
    staticRosterDirectory = ""
    staticGlobalConfig = updateGlobalConfig(staticGlobalConfig, list(rosterDirectory=""))
    staticRosterBaseName = ""
    fake = FAKE_INSTRUCTOR_ROSTER
    fake[["Email"]] = instructorEmail
    staticRoster = fake
  } else {
    if (staticRosterDirectory != staticGlobalConfig[["rosterDirectory"]]) {
      staticGlobalConfig = updateGlobalConfig(staticGlobalConfig,
                                              list(rosterDirectory=staticRosterDirectory))
    }
    staticRosterBaseName = basename(attr(staticRoster, "file"))
  }
}

staticRubrics = getRubrics()
staticActiveProblems = which(sapply(staticRubrics, isProblemActive))
if (length(staticActiveProblems) > 0) {
  staticCurrentProblem = paste("Problem", (1:PROBLEM_COUNT)[staticActiveProblems])
} else {
  staticCurrentProblem = "Problem 1"
}


staticCanvasRE = createCanvasRE()


staticCurrentFiles = findCurrentFiles(id=0, 
                                      allFiles=parseFileNames(list.files.only(), 
                                                              staticCanvasRE),
                                      rubric=staticRubrics[[1]])