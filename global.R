# H. Seltman, Jan 2018

# Global file for shinyGrader app

# This works for the Canvas course management system.
# Many coding features are designed to (hopefully) allow
# for other course management systems and/or changes to
# Canvas.

### CONSTANTS ###
TIME_STAMP_NAME = "config_mod_time"
GLOBAL_CONFIG_NAME = "shinyGrader.config"
SYSTEM_SPECIFIC_CONFIG_NAME = "shinyGrader.specific.config"

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
  assignmentFilenames = "Assignment filenames (';' separated, '*.ext' allowed)",
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
  assignmentFilenames = "*.RRmd",
  courseId = "",
  emailSuffix = "@andrew.cmu.edu",
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

# Convert a text file to a list where the text file
# has format "id:" followed by lines of text, and each
# of these id blocks is separated by at least two blank
# lines (to allow blocks with single embedded blank lines).
# Valid ids are made of only letter, numbers and underscores.
# Text lines are trimmed of initial and final blanks.
# Invalid input results in a NULL return.
# The first line must be an id line.
# varNames[numerics] are required to be one line long and
# are converted to numeric.
# The ids must be in 'varNames'; if not a warning is
# issued that reports the 'source'.
textToConfigList = function(text, varNames, numerics=numeric(0), source) {
  if (length(text) < 2) return(NULL)
  text = trimws(text)
  idLines = grep("^[a-zA-Z0-9_]+:$", text)
  if (length(idLines) == 0) return(NULL)
  if (idLines[1] != 1) return(NULL)
  if (length(idLines) > 1) {
    isBlank = nchar(text) == 0
    bad = sapply(idLines[2:length(idLines)],
                 function(n) {!isBlank[n-1] || !isBlank[n-2]})
    if (any(bad)) return(NULL)
  }
  nIds = length(idLines)
  ids = sapply(1:nIds,
               function(n) {
                 pos = idLines[n]
                 substring(text[pos], 1, nchar(text[pos]) - 1)
               })
  badIds = is.na(match(ids, varNames))
  if (any(badIds)) {
    warning("Bad ids in ", source, ":\n",
            paste(ids[badIds], collapse=", "))
  }
  if (sum(!badIds) == 0) return(NULL)
  last = length(text)
  lst = vector("list", nIds)
  names(lst) = ids
  for (i in 1:nIds) {
    if (badIds[i]) next
    pos = idLines[i]
    end = last
    if (i < nIds) {
      end = idLines[i + 1] - 3
    }
    if (end == pos) {
      lst[[i]] = ""
    } else {
      txt = text[(pos+1):end]
      # Strip final blank lines
      while (length(txt) > 1 && nchar(txt[length(txt)]) == 0) {
        txt = txt[1:(length(txt) - 1)]
      }
      lst[[i]] = txt
    }
  } # end for each id block in the file
  if (length(numerics) > 0) {
    for (i in numerics) {
      id = varNames[i]
      txt = lst[[id]]
      if (!is.null(txt)) {
        if (length(txt) != 1) {
          warning("Bad numeric for config: \n",
                  id, " is:\n", paste(txt, collapse="\n"))
          return(NULL)
        }
        num = suppressWarnings(as.numeric(txt))
        if (is.na(num)) {
          warning("Bad numeric for config: \n",
                  id, " is:\n", paste(txt, collpase="\n"))
          return(NULL)
        }
      }
      lst[[i]] = num
    } # end for each numeric field
  } # end if any numeric fields
  return(lst)
} # end textToConfigList()


# Write a configuration list to a file in the format
# "id:", data lines, 2 blank lines
# May cause an error.
writeConfig = function(lst, filename) {
  if (length(lst) == 0) stop("'lst' is empty")
  if (is.null(names(lst)) || any(nchar(names(lst)) == 0))
    stop("all elements of 'lst' must have names")
  
  lst[[TIME_STAMP_NAME]] = format(Sys.time(), format="%a %b %d %X %Y")
  
  ids = names(lst)
  if (length(grep("[^a-zA-Z0-9_]", ids)) > 0)
    stop("config list ids cannot contain spaces or punctuation")
  
  ids = paste0(names(lst), ":")
  N = length(lst)
  notFirst = FALSE
  for (i in 1:length(lst)) {
    write(ids[i], filename, append=notFirst)
    notFirst = TRUE
    write(lst[[i]], filename, append=TRUE)
    if (i < N) write("\n", filename, append=TRUE) # 2 blank lines
  }
  return(NULL)
}


# Create an intial global configuration from hardcoded defaults,
# a system wide GLOBAL_CONFIG_NAME in the $HOME directory (if
# present) and a GLOBAL_CONFIG_NAME file in the current directory
# (if present)
initializeGlobalConfig = function() {
  # Hardcoded defaults
  gc = GLOBAL_CONFIG_DEFAULTS
  # System-wide user defaults
  HOME = Sys.getenv("HOME")
  globalFileName = file.path(HOME, GLOBAL_CONFIG_NAME)
  if (HOME != "" && file.exists(globalFileName)) {
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
  
  # Current directory user defaults
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
  if (!updated) {
    writeConfig(gc, GLOBAL_CONFIG_NAME)
  }
  
  return(gc)
}


# Read the class roster.
# For Canvas, use the Canvas "Grades / Export" action.
# This is important because it links the Canvas student id number
# ("ID") with the full student name "Student Name" and email
# ("SIS Login ID").  The Canvas filenames start with a "lastfirst" student
# id that is not unique and does not directly appear in the roster.
# Defaults to the Canvas Roster (from Grades / Export; best at start of course),
# Looks for rosters in $HOME if none are found in the working directory.
# If more than one roster matches, the most recently modified is used.
#
getRoster = function(courseId, Canvas=TRUE) {
  if (courseId == "") return(NULL)
  if (!Canvas) stop("Canvas=FALSE is not yet programmed")
  
  rosterRE = CANVAS_ROSTER_DEFAULTS[["rosterRE"]]
  rosterRE = sub("COURSEID", courseId, rosterRE)
  LOC = getwd()
  rosterFiles = file.info(grep(rosterRE, list.files(LOC), value=TRUE))
  if (nrow(rosterFiles) == 0) {
    LOC = Sys.getenv("HOME")
    rosterFiles = file.info(grep(rosterRE, list.files(LOC), value=TRUE))
    if (nrow(rosterFiles) == 0) return(NULL)
  }
  rosterFileName = rownames(rosterFiles)[which.max(rosterFiles$mtime)]

  # Get names of columns to read from globalConfig  
  rosterNames = c("rosterIdCol", "rosterNameCol", "rosterEmailCol")
  rosterCols = lapply(rosterNames, function(x) globalConfig[[x]])
  badCols = sapply(rosterCols, is.null)
  if (any(badCols)) {
    stop("missing from globalCongif: ",
         paste(rosterNames[badCols], collapse=", "))
  }
  # If names are just "Canvas", read in the Canvas defaults
  rosterCols = sapply(1:length(rosterCols),
                      function(n) {
                        val = rosterCols[[n]]
                        if (val == "Canvas") {
                          x = CANVAS_ROSTER_DEFAULTS[[rosterNames[[n]]]]
                          if (is.null(x)) stop(x, " is missing from CANVAS_ROSTER_DEFAULTS")
                        } 
                        return(x)
                      })
  
  # Read in the roster csv file
  # Stupid Canvas roster has "Points Possible" on line 2!!!
  # It also has an encoding string at the beginning.
  roster = try(readLines(file.path(LOC, rosterFileName), encoding="UTF-8"))
  if (is(roster, "try-error")) return(NULL)
  #
  if (length(grep("Points Possible", roster[2]) > 0)) roster = roster[-2]
  roster = try(read.csv(textConnection(roster), as.is=TRUE))
  if (is(roster, "try-error")) return(NULL)
  if (substring(names(roster)[1], 1, 9) == "X.U.FEFF.") {
    names(roster)[1] = substring(names(roster)[1], 10)
  }
  
  # Get the correct column names for shinyGrader
  rosterColumnNames = lapply(rosterNames, function(n) ROSTER_COLUMN_NAMES[[n]])
  badColNames = sapply(rosterColumnNames, is.null)
  if (any(badColNames)) {
    stop("missing from ROSTER_COLUMN_NAMES: ",
         paste(rosterNames[badColsNames], collapse=", "))
  }
  rosterColumnNames = unlist(rosterColumnNames)
  
  # Rename the columns to shinyGrader column names
  roster = try(roster[, make.names(rosterCols)], silent=TRUE)
  if (is(roster, "try-error")) {
    stop("Roster columns ", paste(rosterCols, collapse=", "),
         " not all in ", file.path(LOC, rosterFileName))
  }
  names(roster) = rosterColumnNames
  
  return(roster)
} # end getRoster()


# Parse filenames to get a portion of the filename from
# list.files() using both globalConfig[["filenameFormat"]]
# and "sfne".  The meanings of the codes come from
# FILE_FORMAT_CODES and FILE_FORMAT_RE.
# It is assumed that the filename code segments are separated
# by 'punct' (unless in NO_PRIOR_PUNCTUATION).
# 
parseFilenames = function(filenames=list.files(), punct="[_]") {
  ff = globalConfig[["filenameFormat"]][1]
  if (tolower(ff) == "canvas") ff = CANVAS_FILENAME_FORMAT
  ff = strsplit(ff, "")[[1]]
  
  # Construct regular expression by appending chosen RE sub-patterns
  re = "^"
  first = TRUE
  for (f in ff) {
    reSeg = FILE_FORMAT_RE[[f]]
    if (is.null(reSeg)) stop(f, " is an invalid 'filenameFormat' character")
    
    # Handle case where meaning of the codes are in the globalConig
    special = f %in% names(CANVAS_RE_SUBSTITUTE)
    if (special) {
      idSub = CANVAS_RE_SUBSTITUTE[[f]]
      reSeg = globalConfig[[idSub[1]]]
      if (tolower(reSeg) == "canvas") reSeg = idSub[2]
    }
    
    # Handle no prior punctuation
    p = punct
    if (f %in% NO_PRIOR_PUNCTUATION || first) p = ""
    
    # If optional: Convert 'foo' to '(pfoo)*'
    # otherwise Convert 'foo' to 'p(foo)
    # where 'p' is the punctuation string
    if (f %in% OPTIONAL_FIELDS) {
      reSeg = paste0("(", p, reSeg, ")*")
    } else {
      reSeg = paste0(p, "(", reSeg, ")")
    }
    re = paste0(re, reSeg)
    first = FALSE
  }
  re = paste0(re, "$")
  
  # Simple filenames like "solutions_HW.R"
  simpleRE = "^([a-zA-Z0-9 ]+)[_]([a-zA-Z0-9 ]+)([.][a-zA-Z][a-zA-Z0-9]{0,3})$"

  # Obtain detailed filename information for specified and simple filename formats  
  nonZeroLen = function(x) length(x) > 0
  matchFull = regmatches(filenames, regexec(re, filenames))
  matchFull = matchFull[sapply(matchFull, nonZeroLen)]
  matchDf = data.frame(do.call(rbind, matchFull)[, -1, drop=FALSE], stringsAsFactors=FALSE)
  names(matchDf) = sapply(ff, function(c) FILE_FORMAT_CODES[[c]])
  matchShort = regmatches(filenames, regexec(simpleRE, filenames))
  matchShort = matchShort[sapply(matchShort, nonZeroLen)]
  shortDf = data.frame(do.call(rbind, matchShort)[, -1, drop=FALSE], stringsAsFactors=FALSE)
  names(shortDf) = c("studentName", "baseFileName", "fileExtension")
  

  return(list(match=matchDf, short=shortDf))
} # end parseFilenames()


# Get current global configuration
globalConfig = initializeGlobalConfig()

roster = getRoster("36602")
roster = getRoster(globalConfig[["courseId"]])

# Get current list of codefiles
codefiles = strsplit(globalConfig[["assignmentFilenames"]], ";")[[1]]

# Add individual codefiles when "*" is in codefiles
starLocs = grep("[*]", codefiles)
browser()
if (length(starLocs) > 0) {
  stars = codefiles[starLocs]
  nonStar = setdiff(codefiles, stars)
  for (one in stars) {
    if (! one %in% ALLOWED_STARS) {
      warning("global config, 'codefiles' choice of ", one,
              " is not recognized")
    } else {
      files = parseFilenames("%f.%e")
      if (one == "*.RRmd") continue
    }
  }
}


