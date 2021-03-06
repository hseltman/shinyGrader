# H. Seltman, August 2018

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

# Find SAS
SasLoc = Sys.getenv("SAS_LOCATION")
if (SasLoc == "") SasLoc = "C:\\Program Files\\SasHome\\SASFOUNDATION\\9.4"
SasProg = file.path(SasLoc, "sas.exe")

# Construct the contents of the selectInput widget for selecting students
makeSelectText = function(names, shortEmails, CanvasNames) {
  return(paste0(names, " (", CanvasNames, "; ", shortEmails, ")"))
}

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
  staticRosterBaseName = basename(attr(staticRoster, "file"))
}

if (is.null(staticRoster)) {
  staticRosterDirectory = ""
  staticGlobalConfig = updateGlobalConfig(staticGlobalConfig, list(rosterDirectory=""))
  staticRosterBaseName = ""
  fake = FAKE_INSTRUCTOR_ROSTER
  if (staticGlobalConfig[["instructorEmail"]] != "")
    fake[["Email"]] = staticGlobalConfig[["instructorEmail"]] != ""
  staticRoster = fake
}
# else {
#   if (staticRosterDirectory != staticGlobalConfig[["rosterDirectory"]]) {
#     staticGlobalConfig = updateGlobalConfig(staticGlobalConfig,
#                                             list(rosterDirectory=staticRosterDirectory))
#   }
#   staticRosterBaseName = basename(attr(staticRoster, "file"))
# }

staticSelectStudent = as.character(1:length(staticRoster$selectText))
names(staticSelectStudent) = staticRoster$selectText

staticRubrics = getRubrics()
staticActiveProblems = which(sapply(staticRubrics, isProblemActive))
if (length(staticActiveProblems) > 0) {
  staticCurrentProblem = as.character((1:PROBLEM_COUNT)[staticActiveProblems])
  names(staticCurrentProblem) = paste("Problem", (1:PROBLEM_COUNT)[staticActiveProblems])
} else {
  staticCurrentProblem = c("Problem 1"="1")
}

staticCanvasRE = createCanvasRE()


staticCurrentFiles = findCurrentFiles(id=0, 
                                      allFiles=parseFileNames(list.files.only(), 
                                                              staticCanvasRE),
                                      rubric=staticRubrics[[1]])


staticStudentEmail = staticGlobalConfig[["instructorEmail"]]
if (staticStudentEmail == "") staticStudentEmail = "solution@fake.edu"
staticThisPath = setupSandbox(gsub("@.*", "", staticStudentEmail), staticCurrentFiles, 
                              probNum = min(1, staticActiveProblems))


# Code to set initial values of rubric widgets.
#
# This is difficult and obtuse because we must avoid updating problem rubric
# widgets after initialization to prevent the date on the 'rubric#.RData' 
# files from updating whenever the app is first run.
#
# This relies on the value of 'PROBLEM_COUNT', set in 'setup.R'.
#
probPanelCode = lapply(1:PROBLEM_COUNT,
                      function(prob) {
                        txt = probPanelCodeOne
                        N = length(txt)
                        # 'rubricDefaults' is dynamically defined in "genProblemTabs.R"
                        for (ii in 1:length(rubricDefaults)) {
                          id = names(rubricDefaults)[ii]
                          defaultVal = staticRubrics[[prob]][[id]]
                          if (is.null(defaultVal)) {
                            warning("missing default: ", id)
                            next
                          }
                          
                          if (is.character(defaultVal)) {
                            defaultVal = gsub('"', '\\\\\\"',
                                             gsub("'", "\\\\\\'",
                                                  #gsub("[\\]([0-9])", "\\\\\\\\2",
                                                  gsub("[\\]", "\\\\\\\\",
                                                            defaultVal)))
                            defaultVal = paste0('"', defaultVal, '"')
                          }
                          DV = defaultVal
                          
                          line = grep(id, txt)
                          if (length(line) != 1) {
                            stop("bad construction of 'probPanelCodeOne'")
                          }
                          # Note: some widget definitions are over multiple lines
                          hit = grep("value=", txt[line:min(line+4, N)])
                          if (length(hit) == 0) {
                            warning("problem finding 'value=' for ", id)
                            next
                          }
                          line = line - 1 + hit[1]  # first hit
                          TL = txt[line]
                          preFill = "value="
                          preValue = regexpr(preFill, TL) + nchar(preFill) - 1
                          ending = substring(TL, preValue + 1)
                          postValue = regexpr(")", ending)
                          # Note: using the more obvious gsub() here has quoting problems
                          txt[line] = paste0(substring(TL, 1, preValue),
                                             DV, substring(ending, postValue))
                        }
                        txt = gsub("##", paste(prob), txt, fixed=TRUE)
                        return(paste(txt, collapse="\n"))
                       })

probPanelCode = paste0("tabsetPanel(",
                       paste(probPanelCode, collapse=",\n"), "\n)")

# Set user home directory
# Needed two places in server.R
staticUserHome = path.expand("~")
# "~" is not really user's home directory on Windows!
if (endsWith(staticUserHome, "/Documents")) {
  staticUserHome = substring(staticUserHome, 1, nchar(staticUserHome) - nchar("/Documents"))
}
