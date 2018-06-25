# helper functions for shinyGrader
# H. Seltman, June 2018

# Functions in this file:
#   genUiCode()
#   textToConfigList()
#   writeConfig()
#   updateStatus()
#   findRoster()
#   getRoster()

# Create user interface code consisting of a set of Shiny 'textInput',
# calls created from vectors of names, labels, and values.
# The recommended 'prefix' reduces the chance of a name class, and
# is prepended to the 'inputId'.
#
# Specific variables may be dropped (specified as a string vector)
#
# To use, just call this function in the user interface code.
#
genUiCode = function(variables, labels, values, prefix="",
                     drop=NULL, perRow=1) {
  if (!perRow %in% c(1, 2, 3, 4, 6))
    stop("'perRow' must be one of 1, 2, 3, 4, 6")
  if (!is(variables, "character")) stop("'variables' must be 'character' data")
  if (!is(labels, "character")) stop("'labels' must be 'character' data")
  if (!is(values, "character")) stop("'values' must be 'character' data")
  n = length(variables)
  if (n < 1) stop("'variables' must have at least one element")
  if (length(labels) != n) stop("'labels' and 'variables' must have the same length")
  if (length(values) != n) stop("'values' and 'variables' must have the same length")
  
  # Drop specified variables
  if (is(drop, "character")) {
    toDrop = match(drop, variables)
    if (length(toDrop) > 0) {
      variables = variables[-toDrop]
      labels = labels[-toDrop]
      values = values[-toDrop]
      n = n - length(toDrop)
    }
  }
  
  width = 12 / perRow
  rows = ceiling(n / perRow)
  textList = vector("list", rows)
  row = 1
  for (line in 1:n) {
    lastOnRow = (line == n) || (line%%perRow == 0)
    firstOnRow = line%%perRow == 1
    if (firstOnRow) {
      at = 1 # text element we are writing at
      textList[[row]] = "fluidRow("
      at = at + 1
    }
    textList[[row]][at] = paste0("  column(", width, ", ",
                          "textInput(",
                          "inputId='", prefix, variables[line], "', ",
                          "label='", labels[line], "', ",
                          "value='", values[line], "'))"
                         )
    if (lastOnRow) {
      textList[[row]][at] = paste0(textList[[row]][at], ")")
      row = row + 1
    } else {
      textList[[row]][at] = paste0(textList[[row]][at], ",")
    }
    at = at + 1
  }

  return(lapply(textList, function(x) eval(parse(text=x))))
}


# Convert a special kind of text file into a named list.
#
# The text file must have format "id:" followed by lines of
# text, and each of these id blocks is separated by at least
# two blank lines (to allow blocks with single embedded blank
# lines).  Each one of the 'id's becomes a named element in
# the return value (a list), and the subsequent lines are the
# value of the element (possible converted to numeric; see below).
# Valid ids are made of only letters, numbers and underscores.
#
# Text lines are trimmed of initial and final blanks.
#
# Invalid input results in a NULL return.
#
# The first line must be an 'id' line.
#
# 'varNames' is a string vector containing the expected 'id's.
# 'numerics' is a numeric vector containing the indexes of
# 'varNames' which correspond to 'id's are to be converted to
# numeric.
#
# All 'id's' must be in 'varNames'; if not, a warning is
# issued that reports the 'source'.
#
textToConfigList = function(text, varNames, numerics=numeric(0),
                            source) {
  if (length(text) < 2) return(NULL)
  ignore = TIME_STAMP_NAME
  text = trimws(text)
  
  # Get id lines and verify correct input format
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
  badIds = is.na(match(ids, c(varNames, ignore)))
  if (any(badIds)) {
    warning("Bad ids in ", source, ":\n",
            paste(ids[badIds], collapse=", "))
  }
  if (sum(!badIds) == 0) return(NULL)
  
  # Construct output list and fill in associated text
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
  toDrop = names(lst) %in% ignore
  lst = lst[!toDrop]
  
  # Convert 'id's indicated in 'numeric' to numeric
  if (length(numerics) > 0) {
    for (i in numerics) {
      id = varNames[i]
      txt = lst[[id]]
      if (!is.null(txt)) {
        if (length(txt) != 1) {
          warning("Bad numeric for config: \n",
                  "'", id, "' is:\n", paste(txt, collapse="\n"))
          return(NULL)
        }
        num = suppressWarnings(as.numeric(txt))
        if (is.na(num)) {
          warning("Bad numeric for config: \n",
                  "'", id, "' is:\n", paste(txt, collpase="\n"))
          return(NULL)
        }
      }
      lst[[i]] = num
    } # end for each numeric field
  } # end if any numeric fields
  
  # Drop unneeded values
  lst = lst[!sapply(lst, is.null)]
  
  return(lst)
} # end addTextToList()



# # Convert multiple initial asterisks to spaces
# toConvert = grep("^[*]{2,}", txt)
# for (j in toConvert) {
#   nStar = rle(strsplit(txt[j], split="")[[1]])[["lengths"]][1]
#   txt[j] = paste0(paste(rep(" ", nStar), collapse=""),
#                   substring(txt[j], nStar + 1))
#}


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





# Read file and folder information, and construct status object
updateStatus = function(status) {
  # Assure roster is available
  # Get problem info
  # Get files for each problem
  # Write status records for each student/problem
}


# Obtain the appropriate roster file name.  A valid roster file name
# must contain the 'courseId' and the filename must correspond to
# the regular expression in 'rosterRD'.  We search for the roster first
# in the 'startingLoc' (if any), then in the current directory, and
# then in the 'HOME' directory, unless there is a ROSTER_LOCATION_FILE,
# in which case the directories listed in that file are searched.
#
# If more than one roster matches in the first location with multiple
# roster files, the most recently modified roster file is selected.
#
# Returns full path of the matching filename (or NULL).
#
findRoster = function(courseId, startingLoc=NULL, Canvas=TRUE) {
  if (courseId == "") return("")
  if (!Canvas) stop("Canvas=FALSE is not yet programmed")
  
  # Use a regular expression to find the rosters produced by Canvas
  rosterRE = CANVAS_ROSTER_DEFAULTS[["rosterRE"]]
  rosterRE = sub("COURSEID", courseId, rosterRE)
  
  # Start in the 'startingLoc'
  hasStart = !is.null(startingLoc) && trimws(startingLoc) != ""
  if (hasStart) {
    loc = getwd()
    rosterFiles = file.info(grep(rosterRE, list.files(loc), value=TRUE))
  }
  if (!hasStart || nrow(rosterFiles) == 0) {
    loc = getwd()
    rosterFiles = file.info(grep(rosterRE, list.files(loc), value=TRUE))
    if (nrow(rosterFiles) == 0) {
      LOCS = Sys.getenv("HOME")
      if (file.exists(file.path(LOCS, ROSTER_LOCATION_FILE))) {
        temp = try(suppressWarnings(readLines(file.path(LOCS, 
                                                        ROSTER_LOCATION_FILE))))
        if (!is(temp, "try-error")) LOCS = temp
      }
      for (loc in LOCS) {
        rosterFiles = file.info(file.path(loc, grep(rosterRE, list.files(loc),
                                                    value=TRUE)))
        if (nrow(rosterFiles) > 0) break
      }
      if (nrow(rosterFiles) == 0) return("")
    }
  }
  rosterFileName = rownames(rosterFiles)[which.max(rosterFiles$mtime)]
  return(rosterFileName)
}


# Read a class roster file and return a data frame.
#
# For Canvas, use the Canvas "Grades / Export" action to create the roster
# file.  It is important to use the Canvas roster rather than the SIO
# roster because it links the Canvas student id number ("ID") with the full
# student name "Student Name" and email ("SIS Login ID").  The Canvas filenames
# start with a "lastfirst" student id that is not unique and does not directly
# appear in the roster.
#
# The default is to read the roster produced by Canvas (from Grades / Export),
# in which case all of the details of reading rosters may be specified in the
# global configuration by just using the word "Canvas".
#
# The return value is a data.frame with columns "ID", "Name", and "Email"
# and a "file" attribute containing the roster location or NULL if no
# roster is found.
#
getRoster = function(rosterFileName, globalConfig) {
  if (is.null(rosterFileName) || rosterFileName == "") return(NULL)
  # Get names of columns to read from globalConfig  
  rosterNames = c("rosterIdCol", "rosterNameCol", "rosterEmailCol")
  rosterCols = lapply(rosterNames, function(x) globalConfig[[x]])
  badCols = sapply(rosterCols, is.null)
  if (any(badCols)) {
    msg = paste0("missing from globalConfig: ",
                 paste(rosterNames[badCols], collapse=", "))
    shinyalert("Bad roster file", msg, type = "warning")
    return(NULL)
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
  roster = try(suppressWarnings(readLines(rosterFileName, encoding="UTF-8")))
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
    msg = paste0("missing from ROSTER_COLUMN_NAMES: ",
                 paste(rosterNames[badColsNames], collapse=", "))
    shinyalert("Bad roster file", msg, type = "warning")
    return(NULL)
  }
  rosterColumnNames = unlist(rosterColumnNames)
  
  # Rename the columns to shinyGrader column names
  roster = try(roster[, make.names(rosterCols)], silent=TRUE)
  if (is(roster, "try-error")) {
    msg = paste0("Roster columns ", paste(rosterCols, collapse=", "),
                 " not all in ", rosterFileName)
    
    shinyalert("Bad roster file", msg, type = "warning")
    return(NULL)
  }
  names(roster) = rosterColumnNames
  attr(roster, "file") = rosterFileName
  
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
  ff = CANVAS_FILENAME_FORMAT
  ff = strsplit(ff, "")[[1]]

  # Construct regular expression by appending chosen RE sub-patterns
  re = "^"
  first = TRUE
  for (f in ff) {
    reSeg = FILE_FORMAT_RE[[f]]
    if (is.null(reSeg)) stop(f, " is an invalid 'filenameFormat' character")
    
    # Handle no prior punctuation
    p = punct
    if (f %in% NO_PRIOR_PUNCTUATION || first) p = ""
    
    # If optional: Convert 'foo' to '(pfoo)*'.
    # Otherwise Convert 'foo' to 'p(foo'
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
  
  # Obtain detailed filename information for specified filename formats
  nonZeroLen = function(x) length(x) > 0
  matchFull = regmatches(filenames, regexec(re, filenames))
  matchFull = matchFull[sapply(matchFull, nonZeroLen)]
  if (length(matchFull) == 0) {
    matchDf = NULL
  } else {
    matchDf = data.frame(do.call(rbind, matchFull)[, -1, drop=FALSE], stringsAsFactors=FALSE)
    names(matchDf) = sapply(ff, function(c) FILE_FORMAT_CODES[[c]])
  }
  if (is.null(matchDf)) return(NULL)
  
  # Cleanup data
  matchDf$resubmitNumber = gsub("-", "", matchDf$resubmitNumber)
  matchDf$lateFlat = gsub("_", "", matchDf$resubmitNumber)
  
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


