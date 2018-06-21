# H. Seltman, Jan 2018

# Perform setup
source("helpers.R") # many misc. functions
source("setup.R") # constants and global config functions
source("genProblemTabs.R") # code to generate UI "Problems" tab

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
# here, and then using it in the server.R file to initialize a reactiveVal().
# The same applies to the initial roster.
staticGlobalConfig = initializeGlobalConfig(globalLoc)
initialGCValues = as.character(staticGlobalConfig[names(GLOBAL_CONFIG_IDS)])
staticRosterFileName = findRoster(staticGlobalConfig$courseId,
                                  staticGlobalConfig$rosterDirectory)
if (staticRosterFileName == "") {
  staticRosterDirectory = ""
  staticRoster = NULL
  staticGlobalConfig = updateGlobalConfig(staticGlobalConfig,
                                          list(rosterDirectory=getwd()))
  staticRosterBaseName = ""
} else {
  staticRosterDirectory = dirname(staticRosterFileName)
  staticRoster = getRoster(staticRosterFileName, staticGlobalConfig)
  if (staticRosterDirectory != initialGCValues[match("rosterDirectory",
                                                     names(GLOBAL_CONFIG_IDS))]) {
    staticGlobalConfig = updateGlobalConfig(staticGlobalConfig,
                                            list(rosterDirectory=staticRosterDirectory))
  }
  staticRosterBaseName = basename(attr(staticRoster, "file"))
}



