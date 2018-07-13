# helper functions for shinyGrader
# H. Seltman, June 2018

# Functions in this file:
#   getCurrentProblem()
#   textToConfigList()
#   writeConfig()
#   updateStatus()
#   findRoster()
#   getRoster()
#   saveRubric()
#   getRubrics()
#   createCanvasRE()
#   parseFileNames()
#   isProblemActive()
#   list.files.only()
#   findCurrentFiles()
#   selectStudentInfo()
#   matchFile()
#   dualAlert()
#   setupSandbox()

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


# Obtain the appropriate roster file name given a courseId or find the
# courseId from the latest roster in the current directory.
#
# In "alternate mode" (courseId=NULL), a courseId is returned.
#
# A valid roster file name must contain the 'courseId' and the filename
# must correspond to the regular expression in 'rosterRD'.  We search
# for the roster first in the 'startingLoc' (if any), then in the current
# directory, and then in the 'HOME' directory, unless there is a
# ROSTER_LOCATION_FILE, in which case the directories listed in that file
# are searched.
#
# If more than one roster matches in the first location with multiple
# roster files, the most recently modified roster file is selected.
#
# Returns full matching filename (or "").
#
findRoster = function(courseId=NULL, startingLoc=NULL, Canvas=TRUE) {
  #if (courseId == "") return("")
  if (!Canvas) stop("Canvas=FALSE is not yet programmed")

  # Crate a regular expression to find the rosters produced by Canvas
  rosterRE = CANVAS_ROSTER_DEFAULTS[["rosterRE"]]
  
  # Alternate functionality: return courseId to match rosters
  if (is.null(courseId)) {
    rosterRE = sub("COURSEID", "[0-9]+", rosterRE)
    rosterFiles = file.info(grep(rosterRE, list.files(), value=TRUE))
    if (nrow(rosterFiles) == 0) return("")
    rosterFileName = basename(rownames(rosterFiles)[which.max(rosterFiles$mtime)])
    courseId = gsub(CANVAS_ROSTER_DEFAULTS[["rosterCourseIdRE"]], "\\2", rosterFileName)
    return(courseId)
  }
  
  # Main functionality: find roster to match courseId
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
  rosterFileName = basename(rownames(rosterFiles)[which.max(rosterFiles$mtime)])
  return(file.path(loc, rosterFileName))
} # end findRoster()


# Read a class roster file and instructorEmail and return a data frame.
#
# If the 'rosterFileName' is blank, return the "fake" roster.
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
# and a "file" attribute containing the roster location or NULL if the
# file is not a valid roster file.  A student named "solution" with ID=0
# and 'Email' equal to instructorId from the global config is always added
# at the top.
#
getRoster = function(rosterFileName, instructorEmail="") {
  # Fake instructor email
  fake = FAKE_INSTRUCTOR_ROSTER
  fake[["Email"]] = instructorEmail
  
  #if (is.null(rosterFileName) || rosterFileName == "") {
  if (rosterFileName == "") {
      return(fake)
  }
  
  # Determine if the Shiny is running yet, to decide if shinyalert() will work
  inSession = exists("session", env=parent.env(parent.frame())) &&
              is(get("session", env=parent.env(parent.frame())), "ShinySession")
  
  # Get names of columns to read from a setup.R constant
  rosterNames = names(CANVAS_ROSTER_NAMES)
  canvasNames = as.character(CANVAS_ROSTER_NAMES)

  # Read in the roster csv file
  # Stupid Canvas roster has "Points Possible" on line 2!!!
  # It also has an encoding string at the beginning, so we must specify UTF-8.
  roster = try(suppressWarnings(readLines(rosterFileName, encoding="UTF-8")))
  if (is(roster, "try-error")) return(NULL)
  #
  if (length(grep("Points Possible", roster[2]) > 0)) roster = roster[-2]
  roster = try(suppressWarnings(read.csv(textConnection(roster), as.is=TRUE)), silent=TRUE)
  if (is(roster, "try-error")) {
    msg = paste("Invalid roster file:", rosterFileName)
    if (inSession) {
      shinyalert("Bad roster file", msg, type = "warning")
    } else {
      warning("Bad roster file: (", rosterFileName, ") ", msg)
    }
    return(NULL)
  }
  # Note: no usage of encoding="UTF-8" fixes the byte-order-mark that may be present
  if (substring(names(roster)[1], 1, 9) == "X.U.FEFF.")
    names(roster)[1] = substring(names(roster)[1], 10)

  # Get the correct column names for shinyGrader
  badColNames = !make.names(canvasNames) %in% names(roster)
  if (any(badColNames)) {
    msg = paste0("missing from roster: ",
                 paste(canvasNames[badColNames], collapse=", "))
    if (inSession) {
      shinyalert(paste("Bad roster file (", rosterFileName, ") ", msg), type = "warning")
    } else {
      warning("Bad roster file: (", rosterFileName, ") ", msg)
    }
    return(NULL)
  }
  
  # Rename the columns to shinyGrader column names
  roster = try(roster[, make.names(canvasNames)], silent=TRUE)
  if (is(roster, "try-error")) {
    msg = paste0("Roster columns ", paste(canvasNames, collapse=", "),
                 " not all in ", rosterFileName)
    if (inSession) {
      shinyalert("Bad roster file", msg, type = "warning")
    } else {
      warning("Bad roster file: ", msg)
    }
    return(NULL)
  }
  names(roster) = rosterNames
  
  # Add column that matches file naming in Canvas
  two = strsplit(roster$Name, ",")
  if (any(sapply(two, length) != 2)) {
    msg = "Names in Canvas roster not in last, first format!"
    if (inSession) {
      shinyalert("Bad roster file", msg, type = "warning")
    } else {
      warning("Bad roster file: ", msg)
    }
    return(NULL)
  }
  roster$CanvasName = I(sapply(two,
                               function(LF) {
                                 tolower(paste0(gsub(" ", "", LF[1]),
                                                gsub(" ", "", LF[2])))
                               }))
  attr(roster, "file") = rosterFileName

    return(rbind(fake, roster))
} # end getRoster()


# Create the regular expression for parsing Canvas file names.
#
# OUTPUT: a regular expression for use by parseFileNames().
#
# DETAILS:
# The regular expression is constracted based on codes in
# FILE_FORMAT_CODES, FILE_FORMAT_RE, and FILE_FORMAT_PUNCT that
# describe the Canvas file re-naming conventions.  The regular
# expression is used in parseFilenames() to parse various portions
# of the filename.
# It is assumed that the filename code segments are preceeded
# by 'punct' unless the element is first or its name in
# NO_PRIOR_PUNCTUATION.
#
# Hopefully, if the Canvas conventions change, simple changes
# can be made in "setup.R" to FILE_FORMAT_CODES, FILE_FORMAT_RE,
# and/or FILE_FORMAT_PUNCT without any actual reprogramming.
#
createCanvasRE = function() {
  ff = CANVAS_FILENAME_FORMAT
  ff = strsplit(ff, "")[[1]]
  
  # Construct regular expression by appending chosen RE sub-patterns
  re = "^"
  first = TRUE
  for (f in ff) {
    reSeg = FILE_FORMAT_RE[[f]]
    if (is.null(reSeg)) stop(f, " is an invalid 'filenameFormat' character")
    
    # Handle no prior punctuation
    p = FILE_FORMAT_PUNCT
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
  return(re)  
}


# A version of list.files() that excludes directories
list.files.only = function(path=".") {
  lf = list.files(path)
  return(lf[file.info(file.path(path, lf))$isdir == FALSE])
}


# Parse a set of filenames using the regular expression
# created by createCanvasRE().
#
# INPUT:
#   filenames: a character vector of filenames (not full paths; NO directories)
#   RE: the regular expression created by createCanvasRE()
#
# OUTPUT:
#   a list with elements
#     Canvas: a dataframe with columns: submitName, canvasName,
#             studentName, lateFlag, studentIdNumber, uniqueFileNumber,
#             baseFileName, resubmitNumber, fileExtension
#     other: a character vector of the remaining file names
#
# DETAILS:
# Use a regular expression to separate Canvas-created files
# from other files.
#
# The intention is that the user can name a file "solution_0_0_name.ext".
#
parseFileNames = function(filenames, RE) {
  ff = CANVAS_FILENAME_FORMAT
  ff = strsplit(ff, "")[[1]]
  nonZeroLen = function(x) length(x) > 0
  RER = regexec(RE, filenames)
  matchFull = regmatches(filenames, RER)
  isCanvas = sapply(matchFull, nonZeroLen)
  other = filenames[!isCanvas]
  matchFull = matchFull[isCanvas]
  if (length(matchFull) == 0) {
    matchDf = NULL
  } else {
    matchDf = data.frame(do.call(rbind, matchFull)[, , drop=FALSE], stringsAsFactors=FALSE)
    names(matchDf) = c("canvasName", sapply(ff, function(c) FILE_FORMAT_CODES[[c]]))
  }
  if (is.null(matchDf)) return(NULL)
  
  # Cleanup data
  matchDf$resubmitNumber = gsub("-", "", matchDf$resubmitNumber)
  matchDf$resubmitNumber[matchDf$resubmitNumber == ""] = "0"
  matchDf$resubmitNumber = as.numeric(matchDf$resubmitNumber)
  matchDf$lateFlag = gsub("_", "", matchDf$resubmitNumber)
  submitName = paste0(matchDf$baseFileName, matchDf$fileExtension)
  matchDf = cbind(submitName, matchDf)
  return(list(Canvas=matchDf, other=other))
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


# Save the rubric for a problem
#
# Save a rubricList in a .Rmd file called 'rubric#.RData'.
# The 'rubricList' is a named list of all widgets (except for
# 'submitProblemRubric#') on one Problem Rubric page. It is
# created by the 'observeInput()' for 'submitProblemRubric#'.
#
saveRubric = function(problem, rubricList) {
  save(rubricList, file=paste0("rubric", problem, ".RData"))
  invisible(NULL)
}


# Get the rubrics for all problems into a list of lists
#
# Each outer element is for a different problem within the assignment.
# Each inner element is name for its widget on the Problem tab, and
# has a value of the appropriate type to initialize the widget.
#
# Rubric data comes from rubricDefaults() or a saved rubric#.RData file.
#
getRubrics = function() {
  rubric = vector("list", PROBLEM_COUNT)
  for (problem in 1:PROBLEM_COUNT) {
    # Note: this loads 'rubricList'
    rslt = try(suppressWarnings(load(paste0("rubric", problem, ".RData"))),
               silent=TRUE)
    if (is(rslt, "try-error")) {
      rubric[[problem]] = rubricDefaults
      names(rubric[[problem]]) = names(rubric[[problem]])
    } else {
      if (length(rslt) != 1 || rslt != "rubricList")
        stop(paste0("rubric", problem, ".RData"), " was manually altered!")
      rubric[[problem]] = rubricList
    }
  }
  return(rubric)
}


# Test if problem is active
# 
# Active is currently defined as input$runFile not blank.
#
isProblemActive = function(problem) {
  np = names(problem)
  return(problem$runFile != "")
}


# Convert input$selectStudent to an ID number and email
selectStudentInfo = function(ss, roster) {
  if (ss == "(none)") return(NULL)
  if (substring(ss, 1, 10) == "Instructor") return(0)
  email = gsub("(^.+[;][ ])(.+)([)]$)", "\\2", ss)
  emails = gsub("@.*", "", roster$Email)
  index = which(email == emails)
  if (length(index) != 1) stop("Roster error")
  rtn = c(roster[index, "ID"], email)
  names(rtn) = c("id", "email")
  return(rtn)
}


# Given a file specification with the rules shown here and a
# copy of 'allFiles', return a data.frame with any matching files.
#
# The columns are:
#   inName: name of the original file in the working directory
#   outName: name in should have in the sandbox
#   directory: location relative to working diretory / sandbox
#   deleteFlag: should be deleted after use
#   caseFlag: case was wrong
#   looseFlag: file name convention not strictly followed
#
# The rules are:
#   An initial "@" indicates a non-Canvas file.
#   An initial "^" indicates case-insensitive.
#   Adding "{{reg. exp.}}" indicates secondary matching (with renaming).
#   Just a "{{reg. exp.}}" indicates matching (without renaming).
#   A final "-" indicates that the file should be deleted from
#     the sandbox after use.
#   A non-Canvas file can be in a specified directory, which will
#     be created as a subdirectory of the sandbox.
#
# If secondary matching finds multiple files, NULL is returned.
#
#
# INPUT:
#  fs: a files specification that follows the rules
#  studentFiles: a data.frame of parsed student files for a 
#                single student as defined in parseFileNames().
#  otherFiles: a character vector of non-student files
#
matchFile = function(fs, studentFiles, otherFiles) {
  fs = trimws(fs)
  if (fs == "") return(NULL)
  if (substring(fs, 1, 2) %in% c("@^", "^@")) {
    dualAlert("Bad rubric", 
              'File specification cannot use both "@" and "^"')
    return(NULL)
  }
  
  # Handle initial character
  first = substring(fs, 1, 1)
  if (first == "@") {
    Canvas = FALSE
    ignoreCase = FALSE
    fs = trimws(substring(fs, 2))
  } else if (first == "^") {
    Canvas = TRUE
    ignoreCase = TRUE
    fs = trimws(substring(fs, 2))
  } else {
    Canvas = TRUE
    ignoreCase = FALSE
  }
  
  # handle terminal character
  if (substring(fs, nchar(fs)) == "-") {
    delete = TRUE
    fs = trimws(substring(fs, 1, nchar(fs) -1))
  } else {
    delete = FALSE
  }
  
  # Break 'fs' info 'fs' and 'fsRE'
  if (substring(fs, nchar(fs) - 1) == "}}") {
    left = regexpr("[{]{2}", fs)
    if (left == -1) {
      dualAlert("Bad rubric", 'File specification has }} without {{')
      return(NULL)
    }
    fsRE = substring(fs, left + 2, nchar(fs) - 2)
    fs = substring(fs, 1, left - 1)
  } else {
    fsRE = ""
  }
  
  # Non-Canvas search must use no RE or RE, not both
  if (Canvas==FALSE && fs!="" && fsRE!="") {
    dualAlert("Bad rubric", 'Non-Canvas file specification has both RE and non-RE')
    return(NULL)
  }
  
  ############################################################
  ### Handle finding the file in "other" (non-Canvas file) ###
  ############################################################
  if (!Canvas) {
    directory = dirname(fs)
    fs = basename(fs)
    if (fs != "") { # Try direct match
      indices = grep(fs, otherFiles) # length is always 0 or 1
      if (length(indices) > 0) {
        # non-Canvas exact match
        return(data.frame(inName=I(fs),
                          outName=I(fs),
                          directory=I(directory),
                          deleteFlag=delete,
                          caseFlag=FALSE,
                          looseFlag=FALSE))
      }
      
    } else { # Try RE
      indices = grep(fsRE, otherFiles)
      if (length(indices) == 1) {
        # Non-Canvas RE "loosening" finds one match
        return(data.frame(inName=I(otherFiles[indices]),
                          outName=I(fs),
                          directory=I(directory),
                          deleteFlag=delete,
                          caseFlag=FALSE,
                          looseFlag=FALSE))
      }
      
      if (length(indices) > 0) {
        # Non-Canvas (sole) RE finds match(es)
        return(data.frame(inName=I(otherFiles[indices]),
                          outName=I(otherFiles[indices]),
                          directory=I(directory),
                          deleteFlag=delete,
                          caseFlag=FALSE,
                          looseFlag=FALSE))
      }
    } # end try RE for non-Canvas
    # Non-Canvas non-match
    return(NULL)
  }
  
  ###########################################
  ### Handle finding the file in 'Canvas' ###
  ###########################################
  if (nrow(studentFiles) == 0) return(NULL)
  directory = ""
  # First try exact target match
  if (fs != "") {
    # Note: may have multiple submissions (-1, etc.)
    indices = grep(fs, studentFiles$submitName, fixed=TRUE)
    matched = studentFiles[indices, ]
    justOne = sum(!duplicated(matched[indices, "submitName"])) == 1
    if (length(indices) > 0 && justOne) {
      # Canvas exact matches
      latest = which.max(matched$resubmitNumber)
      return(data.frame(inName=I(matched[latest, "canvasName"]),
                        outName=I(fs),
                        directory=I(directory),
                        deleteFlag=delete,
                        caseFlag=FALSE,
                        looseFlag=FALSE))
    }
    
    # Try a case-insensitive match
    indices = grep(toupper(fs), toupper(studentFiles$submitName), fixed=TRUE)
    matched = studentFiles[indices, ]
    justOne = sum(!duplicated(matched[indices, "submitName"])) == 1
    if (length(indices) > 0 && justOne) {
      # Canvas "loosening" using RE
      latest = which.max(matched$resubmitNumber)
      return(data.frame(inName=I(matched[latest, "canvasName"]),
                        outName=I(fs),
                        directory=I(directory),
                        deleteFlag=delete,
                        caseFlag=TRUE,
                        looseFlag=FALSE))
    }
  } # end if an exact target was supplied
  
  # Try RE after failure for an exact target ("loosening")
  if (fsRE != "") {
    indices = grep(fsRE, studentFiles$submitName)
    uMatches = unique(studentFiles[indices, "submitName"])
    
    # Handle Canvas with "loosening" (where an exact match fails)
    if (fs!="") {
      if (length(uMatches) > 1) {
        # Non-Canvas RE "loosening" finds multiple matches
        return(NULL)
      } else if (length(uMatches) == 1) {
        # Non-Canvas RE "loosening" finds one match
        matched = studentFiles[indices, ]
        latest = which.max(matched$resubmitNumber)
        return(data.frame(inName=I(matched[latest, "canvasName"]),
                          outName=I(matched[latest, "submitName"]),
                          directory=I(directory),
                          deleteFlag=delete,
                          caseFlag=FALSE,
                          looseFlag=TRUE))
      }
      # Try "loosening" with ignore.case=TRUE
      indices = grep(fsRE, studentFiles$submitName, ignore.case=TRUE)
      uMatches = unique(studentFiles[indices, "submitName"])
      if (length(uMatches) > 0) {
        rtn = NULL
        for (m in length(uMatches)) {
          matched = studentFiles[studentFiles$baseFileName == m, ]
          latest = which.max(matched$resubmitNumber)
          rtn = rbind(rtn, data.frame(inName=I(matched[latest, "canvasName"]),
                                      outName=I(fs),
                                      directory=I(directory),
                                      deleteFlag=delete,
                                      caseFlag=FALSE,
                                      looseFlag=TRUE))
        }
      }
      # Exact target supplied, not found, and loosened version not found
      return(NULL)
    }
    
    # Handle Canvas + RE when no exact file name is given (wildcard case)
    # Note: can only get here if (fs == "")
    if (length(uMatches) > 0) {
      # Canvas files have wildcard match(es)
      rtn = NULL
      for (m in length(uMatches)) {
        matched = studentFiles[studentFiles$baseFileName == m, ]
        latest = which.max(matched$resubmitNumber)
        rtn = rbind(rtn, data.frame(inName=I(matched[latest, "canvasName"]),
                                    outName=I(matched[latest, "submitName"]),
                                    directory=I(directory),
                                    deleteFlag=delete,
                                    caseFlag=FALSE,
                                    looseFlag=FALSE))
      }
    }
    
    # No wildcard matches; try wildcard with ignore case
    indices = grep(fsRE, studentFiles$submitName, ignore.case=TRUE)
    uMatches = unique(studentFiles[indices, "submitName"])
    if (length(uMatches) > 0) {
      # Canvas files have wildcard match(es)
      rtn = NULL
      for (m in length(uMatches)) {
        matched = studentFiles[studentFiles$baseFileName == m, ]
        latest = which.max(matched$resubmitNumber)
        rtn = rbind(rtn, data.frame(inName=I(matched[latest, "canvasName"]),
                                    outName=I(matched[latest, "submitName"]),
                                    directory=I(directory),
                                    deleteFlag=delete,
                                    caseFlag=TRUE,
                                    looseFlag=FALSE))
      }
    }
    # No wildcards found
    return(NULL)
  } # end if (fsRE != ""): RE given for Canvas files
  
  # No direct match and no RE
  return(NULL)
  
} # end matchFile()



# File list for a student and a problem
findCurrentFiles = function(idNum, allFiles, rubric) {
  if (rubric$runFile == "") return(NULL)
  studentFiles = allFiles$Canvas[allFiles$Canvas$studentIdNumber==idNum,]
  otherFiles = allFiles$other
  runFile = rubric$runFile
  reqFiles = trimws(strsplit(rubric$reqFiles, ";")[[1]])
  optFiles = trimws(strsplit(rubric$optFiles, ";")[[1]])

  # Get the one run file (may take two attempts if .RRmd is specified)
  if (length(grep("[.]RRmd", runFile)) > 0) {
    runFile = c(gsub("[.]RRmd", ".R", runFile), gsub("[.]RRmd", ".Rmd", runFile))
  }
  runDf = matchFile(runFile[1], studentFiles, otherFiles)
  if (is.null(runDf) && length(runFile) > 2) {
    runDf = matchFile(runFile[2], studentFiles, otherFiles)
  }
  if (is.null(runDf)) {
    runMissing = rubric$runFile
  } else {
    runMissing = NULL
  }
  
  # Get required and optional files
  reqDf = lapply(reqFiles, matchFile, studentFiles=studentFiles, otherFiles=otherFiles)
  missReq = sapply(reqDf, is.null)
  reqMissing = reqFiles[missReq]
  reqDf = do.call(rbind, reqDf[!missReq])
  optDf = lapply(optFiles, matchFile, studentFiles=studentFiles, otherFiles=otherFiles)
  optDf = do.call(rbind, optDf)
  return(list(runDf=runDf,
              runMissing=runMissing,
              reqMissing=reqMissing,
              reqDf=reqDf,
              optDf=optDf))
}


# Give a warning message in the console or as a shinyjs::alert
# based on whether or not Shiny is running.
#
dualAlert = function(title, msg) {
  # Determine if the Shiny is running yet, to decide if shinyalert() will work
  inSession = exists("session", env=parent.env(parent.frame())) &&
    is(get("session", env=parent.env(parent.frame())), "ShinySession")
  
  if (inSession) {
    shinyalert(title, msg, type = "warning")
  } else {
    warning(title, ": ", msg)
  }
  invisible(NULL)
}


# get current problem from "Problem #" text of "input$currentProblem"
getCurrentProblem = function(probString) {
  return(as.numeric(substring(probString, 9)))
}


# Setup sandbox or just return prior sandbox path if no files are new
#
# INPUT:
#  studentEmail: students full email address
#  currentFiles: result of calling getCurrentFiles()
#
# OUTPUT: path to sandbox (or NULL if unsuccessful)
#
# SIDE EFFECTS: create sandbox and copy files to it, if appropriate
#
# DETAILS:
# A sandbox directory is created for each student using the LHS of their email.
# Within this directory, each attempt is given a directory named "1", "2", etc.
#
# A new attempt is defined by at least one new student file in currentFiles.  This
# works because newly submitted files on Canvas are given new names ("-1", "-2", etc.).
#
# For new directories, all files are copied to the sandbox.  For existing directories,
# all sandbox files that are older than the ones in the working directory are replaced.
# This means that you can re-run analyses after amending non-student files.  In addition,
# you may manually correct student files (e.g., for a trivial error) in the working
# directory, and then any re-run will use the corrected file.
#
# The save() version of the 'currentFiles' object is used to identify what files
# define an attempt.  Do not erase "shinyGraderCF.RData" files!
#
setupSandbox = function(studentEmail, currentFiles) {
  studentEmail = gsub("(.*)(@.*)", "\\1", studentEmail)
  browser()
  
  # Set up top student directory and 'lastDir'
  if (! dir.exists(studentEmail)) {
    if (!dir.create(studentEmail, showWarnings=FALSE)) return(FALSE)
    if (!dir.create(file.path(studentEmail, "1"), showWarnings=FALSE)) return(FALSE)
    lastDir = 0  # most recently used attempt
  } else {
    dirs = basename(list.dirs(studentEmail, recursive=FALSE))
    dirs = grep("^[1-9][0-9]?$", dirs, value=TRUE)
    if (length(dirs) == 0) {
      if (!dir.create(file.path(studentEmail, "1"), showWarnings=FALSE)) return(FALSE)
      lastDir = 0      
    } else {
      lastDir = max(as.numeric(dirs))
    }
  } # end setting up top student directory and setting 'lastDir'
  
  # Determine if a new directory is needed
  useLastDir = FALSE
  cfPath = file.path(studentEmail, lastDir, "shinyGraderCF.RData")
  if (lastDir != 0 && !file.exists(cfPath)) {
    useLastDir = TRUE
    save(currentFiles, file=file.path(studentEmail, lastDir, "shinyGraderCF.RData"))
  }
  if (lastDir != 0 && file.exists(cfPath)) {
    thisCF = currentFiles
    what = try(suppressWarnings(load(cfPath)), silent=TRUE)
    if (!is(what, "try-error") && length(what) == 1 || what == "currentFiles") {
      if (!isTRUE(all.equal(thisCF$runFile$inName, currentFiles$runFile$inName)) ||
          !isTRUE(all.equal(thisCF$reqFiles$inName, currentFiles$reqFiles$inName)) ||
          !isTRUE(all.equal(thisCF$optFiles$inName, currentFiles$optFiles$inName))) {
        useLastDir = FALSE
        currentFiles = thisCF
      } else {
        useLastDir = TRUE
      }
    }
  }
  
  if (!useLastDir) {
    thisDir = lastDir + 1
    if (!dir.create(file.path(studentEmail, lastDir + 1), showWarnings=FALSE)) return(FALSE)
    save(currentFiles, file=file.path(studentEmail, thisDir, "shinyGraderCF.RData"))
  } else {
    thisDir = lastDir
  }
  path = file.path(studentEmail, thisDir)
  
  print(path)
} # end setupSandbox()
