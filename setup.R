# Read environmental variables and setup files for shinyGrader
# H. Seltman, August 2018

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
  l = "studentFamilyName", # "l"ast in US
  i = "studentIdNumber",
  u = "uniqueFileNumber",
  f = "lateFlag",  # optional
  n = "resubmitNumber"  # optional
)
#
# Canvas file format separation character (as a regular expression)
FILE_FORMAT_PUNCT = "[_]"
#
CANVAS_LATE_RE = "late"
CANVAS_RESUBMIT_RE = "-[0-9]{1,2}"
#
FILE_FORMAT_RE = list(
  b = "[-a-zA-Z0-9 _.]+",
  e = "[.][a-zA-Z][a-zA-Z0-9]{0,3}",
  s = "[a-zA-Z0-9']+",  # numbers appear in Canvas groups
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

# Currently allowed global configuration names and text
GLOBAL_CONFIG_IDS = list(
  courseId = "Course Id",
  assignmentName = "Assignment Name",
  rosterDirectory = "Roster Directory",
  instructorEmail = "Instructor Email"
)

# Currently allowed global configuration defaults
GLOBAL_CONFIG_DEFAULTS = list(
  courseId = "",
  assignmentName = "",
  rosterDirectory = "",
  instructorEmail = ""
)

# Element name is for shinyGrader; value is for Canvas Roster
CANVAS_ROSTER_NAMES = list(
  ID = "ID",
  Name = "Student",
  Email = "SIS Login ID"
)

# Fake roster for instructor
# Names must match CANVAS_ROSTER_NAMES plus 'CanvasName'
FAKE_INSTRUCTOR_ROSTER = data.frame(ID=0,
                                    Name = I("Instructor"),
                                    Email = I("solution@fake.edu"),
                                    CanvasName = I("solution"))
attr(FAKE_INSTRUCTOR_ROSTER, "file") = file.path(getwd(), "fakeRoster")

# Defaults for the Canvas Roster (from Grades / Export; best at start of course)
CANVAS_ROSTER_DEFAULTS = list(
  rosterRE = "^.*Grades-COURSEID-[0-9A-Z][.]csv$",
  rosterCourseIdRE = "(^.*Grades-)([0-9]+)(-[0-9A-Z][.]csv$)",
  rosterIdCol = "ID",
  rosterNameCol = "Student",
  rosterFirstNameCol = "",
  rosterFamilyNameCol = "",
  rosterEmailCol = "SIS Login ID"
)
#
CANVAS_FILENAME_NAME_FORMAT = "lastfirst"

# Maximum number of problems in an assignment
PROBLEM_COUNT = 4


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
  gcHere = file.exists(GLOBAL_CONFIG_NAME)
  updated = TRUE
  if (gcHere) {
    updated = FALSE
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
  } else { # end if global config found in current directory
    cid = findRoster(courseId=NULL)
    if (cid != "") {
      updated = TRUE
      gc = modifyList(gc, list(courseId=cid))
    }
  } # end if global config not found in current directory
  
  # Change blank 'rosterDirectory' to current directory
  if (any(names(gc) == "rosterDirectory")) {
    if (gc[['rosterDirectory']] == "") {
      updated = TRUE
      gc[['rosterDirectory']] = getwd()
    }
  } else {
    cat("bug check")
    browser()
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

