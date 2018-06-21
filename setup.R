# Read environmental variables and setup files for shinyGrader
# H. Seltman, Jun 2018

# Global file for shinyGrader app

# This works for the Canvas course management system.
# Many coding features are designed to (hopefully) allow
# for other course management systems and/or changes to
# Canvas.


### CONSTANTS ###
TIME_STAMP_NAME = "config_mod_time"
GLOBAL_CONFIG_NAME = "shinyGrader.config"
SYSTEM_SPECIFIC_CONFIG_NAME = "shinyGrader.specific.config"
ROSTER_LOCATION_FILE = "shinyGraderRosterLocations.txt"

# Wildcard file inputs
ALLOWED_STARS = c("*.R", "*.Rmd", "*.RRmd", "*.py", "*.sas")

# Flexible specification of altered filenames performed by
# a course management system
FILE_FORMAT_CODES = list(
  b = "baseFileName",
  e = "fileExtension",
  s = "studentName",
  g = "studentGivenName",
  l = "studentLastName",
  i = "studentIdNumber",
  u = "uniqueFileNumber",
  f = "lateFlag",  # optional
  n = "resubmitNumber"  # optional
)
#
CANVAS_LATE_RE = "late"
CANVAS_RESUBMIT_RE = "-[0-9]{1,2}"
#
FILE_FORMAT_RE = list(
  b = "[a-zA-Z0-9 ]+",
  e = "[.][a-zA-Z][a-zA-Z0-9]{0,3}",
  s = "[a-zA-Z]+",  # Can there be punctuation in names??
  g = "[a-zA-Z]+",
  l = "[a-zA-Z]+",
  i = "[0-9]+",
  u = "[0-9]+",
  f = CANVAS_LATE_RE,
  n = CANVAS_RESUBMIT_RE
)
#
OPTIONAL_FIELDS = c("f", "n")
NO_PRIOR_PUNCTUATION = c("n", "e")
#
CANVAS_FILENAME_FORMAT = "sfiubne"
# What to substitute when "Canvas" is found in
# a globalConfig RE entry.
CANVAS_RE_SUBSTITUTE = list(
  f = c("lateFlagRE", CANVAS_LATE_RE),
  n = c("resubmitRE", CANVAS_RESUBMIT_RE)
)

# Currently allowed global configuration names and text
GLOBAL_CONFIG_IDS = list(
  rosterDirectory = "Roster Directory",
  courseId = "Course Id",
  emailSuffix = "Email suffix",
  rosterRE = "Regular expression to find rosters",
  rosterIdCol = "Roster ID column",
  rosterNameCol = "Roster name column",
  rosterFirstnameCol = "Roster first name column",
  rosterLastnameCol = "Roster last name column",
  rosterEmailCol = "Roster email column",
  filenameFormat = "Filename format",
  filenameNameFormat = "Filename name format",
  lateFlagRE = "Late flag regular expression",
  resubmitRE = "Resubmit regular expression"
)

# Currently allowed global configuration defaults
GLOBAL_CONFIG_DEFAULTS = list(
  rosterDirectory = "",
  courseId = "",
  emailSuffix = "",
  rosterRE = "Canvas",
  rosterIdCol = "Canvas",
  rosterNameCol = "Canvas",
  rosterFirstnameCol = "Canvas",
  rosterLastnameCol = "Canvas",
  rosterEmailCol = "Canvas",
  filenameFormat = "Canvas",
  filenameNameFormat = "Canvas",
  lateFlagRE = "Canvas",
  resubmitRE = "Canvas"
)

ROSTER_COLUMN_NAMES = list(
  rosterIdCol = "ID",
  rosterNameCol = "Name",
  rosterFirstnameCol = "FirstName",
  rosterLastnameCol = "LastName",
  rosterEmailCol = "Email"
)

# Defaults for the Canvas Roster (from Grades / Export; best at start of course)
CANVAS_ROSTER_DEFAULTS = list(
  rosterRE = "^.*Grades-COURSEID-[0-9A-Z].csv$",
  rosterIdCol = "ID",
  rosterNameCol = "Student",
  rosterFirstnameCol = "",
  rosterLastnameCol = "",
  rosterEmailCol = "SIS Login ID"
)
#
CANVAS_FILENAME_NAME_FORMAT = "lastfirst"





# Create an intial global configuration from hardcoded defaults,
# a system-wide GLOBAL_CONFIG_NAME in the $HOME directory (if
# present) and a GLOBAL_CONFIG_NAME file in the current directory
# (if present)
# 'HOME' is where the function looks for the system-wide defaults.
initializeGlobalConfig = function(HOME) {
  # Start with hardcoded defaults
  gc = GLOBAL_CONFIG_DEFAULTS
  
  # Merge in any system-wide user defaults
  globalFileName = file.path(HOME, GLOBAL_CONFIG_NAME)
  if (file.exists(globalFileName)) {
    globalText = try(readLines(globalFileName), silent=TRUE)
    if (!is(globalText, "try-error")) {
      lst = textToConfigList(globalText,
                             c(names(GLOBAL_CONFIG_IDS), TIME_STAMP_NAME),
                             source=paste(GLOBAL_CONFIG_NAME, "in", HOME))
      if (!is.null(lst)) {
        gc = modifyList(gc, lst)
      }
    }
  }
  
  # Merge in user defaults from the current directory
  updated = FALSE
  gcHere = file.exists(GLOBAL_CONFIG_NAME)
  if (gcHere) {
    globalText = try(readLines(GLOBAL_CONFIG_NAME))
    if (!is(globalText, "try-error")) {
      lst = textToConfigList(globalText,
                             c(names(GLOBAL_CONFIG_IDS), TIME_STAMP_NAME),
                             source=paste(GLOBAL_CONFIG_NAME, "in", getwd()))
      if (!is.null(lst)) {
        updated = TRUE
        gc = modifyList(gc, lst)
      }
    } # end if text read from global config in current directory
  } # end if global config found in current directory
  
  # Expand 'rosterDirectory'
  if (any(names(gc) == "rosterDirectory")) {
    if (gc[['rosterDirectory']] == "") {
      updated = TRUE
      gc[['rosterDirectory']] = getwd()
    }
  }
  
  # Store local version of global configuration file (if new)
  if (updated) {
    writeConfig(gc, GLOBAL_CONFIG_NAME)
  }
  
  return(gc)
}


# Update global configuration from automatically generated
# widgets and save a new global config file.
updateGlobalConfig = function(gc, widgetValues) {
  if (!identical(widgetValues, gc[names(widgetValues)])) {
    gc = modifyList(gc, widgetValues)
    writeConfig(gc, GLOBAL_CONFIG_NAME)
  }
  invisible(gc)
}




# Do everything needed for an initial directory or when the user
# chooses a new directory
newDirectory = function() {
  
}

