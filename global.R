# H. Seltman, Jan 2018

# Perform setup
source("helpers.R")
source("setup.R")

# Load code to generate UI "Problems" tab
source("genProblemTabs.R")


# Create initial status object
noTime = numeric(0)
class(noTime) = "POSIXct"
status = data.frame(student=I(character(0)),
                    problem=integer(0),
                    lastRun=noTime,
                    newestFile=noTime,
                    configTime=noTime,
                    status=integer(0))

# Read file and folder information, and construct status object
updateStatus = function(status) {
  # Assure roster is available
  # Get problem info
  # Get files for each problem
  # Write status records for each student/problem
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
  #browser()
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
  simpleRE = "^([a-zA-Z0-9 ]+)[_]([a-zA-Z0-9 ]+)(-[0-9]+)*([.][a-zA-Z][a-zA-Z0-9]{0,3})$"

  # Obtain detailed filename information for specified and simple filename formats
  nonZeroLen = function(x) length(x) > 0
  matchFull = regmatches(filenames, regexec(re, filenames))
  matchFull = matchFull[sapply(matchFull, nonZeroLen)]
  if (length(matchFull) == 0) {
    matchDf = NULL
  } else {
    matchDf = data.frame(do.call(rbind, matchFull)[, -1, drop=FALSE], stringsAsFactors=FALSE)
    names(matchDf) = sapply(ff, function(c) FILE_FORMAT_CODES[[c]])
  }
  matchShort = regmatches(filenames, regexec(simpleRE, filenames))
  matchShort = matchShort[sapply(matchShort, nonZeroLen)]
  shortCols = c("studentName", "baseFileName", "resubmitNumber", "fileExtension")
  if (length(matchShort) == 0) {
    shortDf = NULL
  } else {
    shortDf = data.frame(do.call(rbind, matchShort)[, -1, drop=FALSE], stringsAsFactors=FALSE)
    names(shortDf) = shortCols
  }
  if (is.null(shortDf) && is.null(matchDf)) return(NULL)
  
  # Merge data.frames
  if (!is.null(shortDf) && !is.null(matchDf)) {
    Sel = is.na(match(shortCols, names(matchDf)))
    if (any(Sel)) {
      warning("expected in 'matchDf': ", paste(shortCols[Sel], collapse=", "))
      return(NULL)
    }
    cols = vector("list", ncol(matchDf))
    names(cols) = names(matchDf)
    for (c in shortCols) cols[[c]] = shortDf[[c]]
    for (c in names(cols)) {
      if (is.null(cols[[c]])) cols[[c]] = rep(NA, nrow(shortDf))
    }
    matchDf = rbind(matchDf, do.call(data.frame, cols))
  }
  
  return(matchDf)
} # end parseFilenames()


# Get current global configuration
# globalConfig = initializeGlobalConfig()
# 
# roster = getRoster("36602")
# browser()
# roster = getRoster(globalConfig[["courseId"]])
# 
# # Get current list of codefiles
# codefiles = strsplit(globalConfig[["assignmentFilenames"]], ";")[[1]]

# Add individual codefiles when "*" is in codefiles
# starLocs = grep("[*]", codefiles)
# if (length(starLocs) > 0) {
#   stars = codefiles[starLocs]
#   nonStar = setdiff(codefiles, stars)
#   for (one in stars) {
#     if (! one %in% ALLOWED_STARS) {
#       warning("global config, 'codefiles' choice of ", one,
#               " is not recognized")
#     } else {
#       files = parseFilenames("%f.%e")
#       if (one == "*.RRmd") continue
#     }
#   }
# }


