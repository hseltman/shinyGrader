# helper functions for shinyGrader
# H. Seltman, August 2018

# Functions in this file:
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
#   splitCodeRubric()
#   checkCode()
#   testSpecs()
#   parseSpec()
#   runCode()
#   checkEnables()
#   colectErrors()

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

  # Create a regular expression to find the rosters produced by Canvas
  rosterRE = CANVAS_ROSTER_DEFAULTS[["rosterRE"]]
  
  # Alternate functionality: return courseId to match rosters
  if (is.null(courseId) || nchar(trimws(courseId)) == 0) {
    rosterRE = sub("COURSEID", "[0-9]+", rosterRE)
    rosterFiles = file.info(grep(rosterRE, list.files(), value=TRUE))
    if (nrow(rosterFiles) == 0) return("")
    rosterFileName = basename(rownames(rosterFiles)[which.max(rosterFiles$mtime)])
    courseId = gsub(CANVAS_ROSTER_DEFAULTS[["rosterCourseIdRE"]], "\\2", rosterFileName)
    #return(courseId)
    return(file.path(getwd(), rosterFileName))
  }
  
  # Main functionality: find roster to match courseId
  rosterRE = sub("COURSEID", courseId, rosterRE)
  
  # Start in the 'startingLoc'
  hasStart = !is.null(startingLoc) && trimws(startingLoc) != ""
  if (hasStart) {
    loc = startingLoc # getwd()
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
# The return value is a data.frame with columns "ID", "Name", "Email",
# "shortEmail", and "selectText" (what shows in the "selectStudent" widget).
# A "file" attribute contains the roster location or NULL if the
# file is not a valid roster file.  A student named "solution" with ID=0
# and 'Email' equal to instructorId from the global config is always added
# at the top.
#
getRoster = function(rosterFileName, instructorEmail="") {
  # Fake instructor email
  browser()
  fake = FAKE_INSTRUCTOR_ROSTER
  if (instructorEmail!="") {
    fake[["Email"]] = instructorEmail
    fake[["shortEmail"]] = gsub("@.*", "", fake[["Email"]])
  }
  fake$selectText = I(makeSelectText(fake$Name, fake$shortEmail, fake$CanvasName))
  
  #if (is.null(rosterFileName) || rosterFileName == "") {
  if (rosterFileName == "") {
      return(fake)
  }
  
  # Get names of columns to read from a setup.R constant
  rosterNames = names(CANVAS_ROSTER_NAMES)
  canvasNames = as.character(CANVAS_ROSTER_NAMES)

  # Read in the roster csv file
  # Stupid Canvas roster has "Points Possible" on line 2!!!
  # It also has an encoding string at the beginning, so we must specify UTF-8.
  roster = try(suppressWarnings(readLines(rosterFileName, encoding="UTF-8")))
  if (is(roster, "try-error")) {
    dualAlert("Bad roster", paste("Cannot read", rosterFileName))
    return(fake)
  }
  #
  if (length(grep("Points Possible", roster[2]) > 0)) roster = roster[-2]
  roster = try(suppressWarnings(read.csv(textConnection(roster), as.is=TRUE,
                                         encoding="UTF8-BOM")), silent=TRUE)
  if (!is(roster, "data.frame") || is.null(names(roster))) {
    msg = paste("Invalid roster file:", rosterFileName)
    dualAlert("Bad roster file format", msg)
    return(fake)
  }
  # Note: no usage of encoding="UTF-8" fixes the byte-order-mark that may be present
  #if (substring(names(roster)[1], 1, 9) == "X.U.FEFF.")
  #  names(roster)[1] = substring(names(roster)[1], 10)

  # Get the correct column names for shinyGrader
  badColNames = !make.names(canvasNames) %in% names(roster)
  if (any(badColNames)) {
    msg = paste0("Missing from roster: ",
                 paste(canvasNames[badColNames], collapse=", "))
    dualAlert("Bad roster file format", msg)
    return(fake)
  }
  
  # Rename the columns to shinyGrader column names
  roster = try(roster[, make.names(canvasNames)], silent=TRUE)
  if (is(roster, "try-error")) {
    msg = paste0("Roster columns ", paste(canvasNames, collapse=", "),
                 " not all in ", rosterFileName)
    dualAlert("Bad roster file", msg)
    return(fake)
  }
  names(roster) = rosterNames
  roster$shortEmail = gsub("@.*", "", roster$Email)
  
  # Drop Canvas's test student
  roster = roster[roster$Name != "Student, Test", ]
  
  # Add column that matches file naming in Canvas
  two = strsplit(roster$Name, ",")
  if (any(sapply(two, length) != 2)) {
    msg = "Names in Canvas roster not in last, first format!"
    dualAlert("Bad roster file format", msg)
    return(fake)
  }
  roster$CanvasName = I(sapply(two,
                               function(LF) {
                                 tolower(paste0(gsub(" ", "", LF[1]),
                                                gsub(" ", "", LF[2])))
                               }))
  roster$selectText = I(makeSelectText(roster$Name, roster$shortEmail,
                                       roster$CanvasName))
  roster = rbind(fake, roster)
  attr(roster, "file") = rosterFileName

  return(roster)
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
# The intention is that the user can name a file "solution_0_0_name.ext"
# to be used for testing.
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
  submitName = I(paste0(matchDf$baseFileName, matchDf$fileExtension))
  matchDf = cbind(submitName, matchDf)
  return(list(Canvas=matchDf, other=other))
} # end parseFilenames()


# Get current global configuration
# globalConfig = initializeGlobalConfig()
# 
# roster = getRoster("36602")
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
saveRubric = function(problem, rubricList, home=FALSE) {
  if (home) {
    file = file.path(globalLoc, "defaultRubric.RData")
  } else {
    file = paste0("rubric", problem, ".RData")
  }
  rtn = try(save(rubricList, file=file), silent=TRUE)
  if (is(rtn, "try-error")) {
    dualAlert("Error saving rubric",
              paste("Could not save", file))
  }
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
      defRubName = file.path(globalLoc, "defaultRubric.RData")
      if (file.exists(defRubName)) {
        dflt = try(load(defRubName), silent=TRUE)
        if (is(dflt, "try-error") || length(dflt) !=1 || dflt != "rubricList") {
          dualAlert("Bad default rubric",
                    paste("You should delete", defRubName))
        } else {
          rubric[[problem]] = modifyList(rubric[[problem]], rubricList)
        }
      }
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
# selectStudentInfo = function(ss, roster) {
#   email = gsub("(^.+[;][ ])(.+)([)]$)", "\\2", ss)
#   emails = gsub("@.*", "", roster$Email)
#   index = which(email == emails)
#   if (length(index) != 1) stop("Roster error")
#   rtn = c(roster[index, "ID"], email)
#   names(rtn) = c("id", "email")
#   return(rtn)
# }


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
#   canvasFlag: file came from Canvas (i.e., it is a student file)
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
  
  # Special case of "dir/file" with missing "@"
  if (length(grep("[{]", fs)) == 0 && dirname(fs) != ".") {
    fs = paste0("@", fs)
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
    if (directory != ".") otherFiles = list.files.only(directory)
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
                          looseFlag=FALSE,
                          canvasFlag=Canvas))
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
                          looseFlag=FALSE,
                          canvasFlag=Canvas))
      }
      
      if (length(indices) > 0) {
        # Non-Canvas (sole) RE finds match(es)
        return(data.frame(inName=I(otherFiles[indices]),
                          outName=I(otherFiles[indices]),
                          directory=I(directory),
                          deleteFlag=delete,
                          caseFlag=FALSE,
                          looseFlag=FALSE,
                          canvasFlag=Canvas))
      }
    } # end try RE for non-Canvas
    # Non-Canvas non-match
    return(NULL)
  }
  
  ###########################################
  ### Handle finding the file in 'Canvas' ###
  ###########################################
  if (nrow(studentFiles) == 0) return(NULL)
  directory = "."
  # First try exact target match
  if (fs != "") {
    # Note: may have multiple submissions (-1, etc.)
    indices = grep(paste0("^", fs, "$"), studentFiles$submitName)
    matched = studentFiles[indices, ]
    justOne = sum(!duplicated(matched[, "submitName"])) == 1
    if (length(indices) > 0 && justOne) {
      # Canvas exact matches
      latest = which.max(matched$resubmitNumber)
      return(data.frame(inName=I(matched[latest, "canvasName"]),
                        outName=I(fs),
                        directory=I(directory),
                        deleteFlag=delete,
                        caseFlag=FALSE,
                        looseFlag=FALSE,
                        canvasFlag=Canvas))
    }
    
    # Try a case-insensitive match
    indices = grep(paste0("^", toupper(fs), "$"), toupper(studentFiles$submitName))
    matched = studentFiles[indices, ]
    justOne = sum(!duplicated(matched[, "submitName"])) == 1
    if (length(indices) > 0 && justOne) {
      # Canvas "loosening" using RE
      latest = which.max(matched$resubmitNumber)
      return(data.frame(inName=I(matched[latest, "canvasName"]),
                        outName=I(fs),
                        directory=I(directory),
                        deleteFlag=delete,
                        caseFlag=TRUE,
                        looseFlag=FALSE,
                        canvasFlag=Canvas))
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
                          looseFlag=TRUE,
                          canvasFlag=Canvas))
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
                                      looseFlag=TRUE,
                                      canvasFlag=Canvas))
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
                                    looseFlag=FALSE,
                                    canvasFlag=Canvas))
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
                                    looseFlag=FALSE,
                                    canvasFlag=Canvas))
      }
    }
    # No wildcards found
    return(NULL)
  } # end if (fsRE != ""): RE given for Canvas files
  
  # No direct match and no RE
  return(NULL)
  
} # end matchFile()



# Compute information about the files for the current
# student and rubric (for the current problem).
#
# Input: 
#   idNum: the student's Canvas id number (second field in Canvas filenames)
#   allFiles: the allFiles() reactiveVal [see findCurrentFiles()]
#   rubric: the element of the rubrics() reactiveVal for the current problem
#
# Output:
#   runDf: a matchFile() type data.frame with a file.info()$mtime added
#          corresponding to the runFile, or NULL if no runFile
#   runMissing: NULL or the name of the missing runFile
#   reqMissing: NULL or a matchFile() data.frame describing the missing
#               required files
#   reqDf: NULL or a matchFile() data.frame describing the available
#          required files
#   optDf: NULL or a matchFile() data.frame describing the available
#          optional files
#
# This function adds modifyTime to the three data.frames
#
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
  if (is.null(runDf) && length(runFile) > 1) {
    runDf = matchFile(runFile[2], studentFiles, otherFiles)
  }
  if (is.null(runDf)) {
    runMissing = rubric$runFile
  } else {
    runMissing = NULL
    runDf$modifyTime = file.info(runDf$inName)$mtime
  }
  
  # Get required and optional files
  if (length(reqFiles) == 0) {
    reqDf = NULL
    missReq = NULL
    reqMissing = character(0)
  } else {
    reqDf = lapply(reqFiles, matchFile, studentFiles=studentFiles, otherFiles=otherFiles)
    missReq = sapply(reqDf, is.null)
    reqMissing = reqFiles[missReq]
    reqDf = do.call(rbind, reqDf[!missReq])
    reqDf$modifyTime = file.info(file.path(reqDf$directory, reqDf$inName))$mtime
  }
  if (length(optFiles) == 0) {
    optDf = NULL
  } else {
    optDf = lapply(optFiles, matchFile, studentFiles=studentFiles, otherFiles=otherFiles)
    optDf = do.call(rbind, optDf)
    if (!is.null(optDf)) {
      optDf$modifyTime = file.info(file.path(optDf$directory, optDf$inName))$mtime
    }
  }
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
  #inSession = exists("session", env=parent.env(parent.frame())) &&
  #  is(get("session", env=parent.env(parent.frame())), "ShinySession")
  
  # inSession = exists("shinyIsRunning", envir=.GlobalEnv, inherits=FALSE) &&
  #             get("shinyIsRunning", envir=.GlobalEnv, inherits=FALSE)
  
  if (isRunning()) {
    if (is(try(shinyalert(title, msg, type = "warning")), "try-error")) {
      browser()
    }
  } else {
    warning(title, ": ", msg)
  }
  invisible(NULL)
}


# Setup sandbox or just return prior sandbox path if no files are new
#
# INPUT:
#  studentEmail: students full email address
#  currentFiles: result of calling findCurrentFiles()
#  probNum: the current problem number
#
# OUTPUT: path to sandbox (or NULL if unsuccessful)
#
# SIDE EFFECTS: create sandbox and copy files to it, if appropriate
#
# DETAILS:
# A sandbox directory is created for each student using the LHS of their email.
# Within this directory, each problem is in a folder labeled "problem#", and
# each attempt is given a directory named "1", "2", etc.
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
setupSandbox = function(studentShortEmail, currentFiles, probNum) {
  probName = paste0("problem", probNum)

  # Set up top student directory and 'lastDir'
  if (! dir.exists(studentShortEmail)) {
    if (!dir.create(studentShortEmail, showWarnings=FALSE)) {
      dualAlert("Sandbox error", paste("Cannot create folder", studentShortEmail))
      return(NULL)
    }
  }
  probFolder = file.path(studentShortEmail, probName)
  if (! dir.exists(probFolder)) {
    if (!dir.create(probFolder, showWarnings=FALSE)) {
      dualAlert("Sandbox error", paste("Cannot create folder", probFolder))
      return(NULL)
    }
    lastDir = 0  # most recently used attempt
  } else {
    dirs = basename(list.dirs(probFolder, recursive=FALSE))
    dirs = grep("^[1-9][0-9]?$", dirs, value=TRUE)
    if (length(dirs) == 0) {
      lastDir = 0      
    } else {
      lastDir = max(as.numeric(dirs))
    }
  } # end setting up top student directory and setting 'lastDir'
  
  # Determine if a new directory is needed
  useLastDir = FALSE
  cfPath = file.path(probFolder, lastDir, "shinyGraderCF.RData")
  if (lastDir != 0) {
    if (!file.exists(cfPath)) {
      useLastDir = TRUE
      save(currentFiles, file=file.path(probFolder, lastDir, "shinyGraderCF.RData"))
    } else {
      thisCF = currentFiles
      what = try(suppressWarnings(load(cfPath)), silent=TRUE)
      runFile = thisCF$runDf$inName
      if (!is(what, "try-error") && length(what) == 1 || what == "currentFiles") {
        diffFiles = !isTRUE(all.equal(runFile,
                                      currentFiles$runDf$inName)) ||
                    !isTRUE(all.equal(thisCF$reqDf$inName,
                                      currentFiles$reqDf$inName)) ||
                    !isTRUE(all.equal(thisCF$optDf$inName,
                                      currentFiles$optDf$inName))
        files = list.files(file.path(studentShortEmail, probName, lastDir))
        startedAnalysis = "codeProblems.RData" %in% files ||
                          (!is.null(runFile) &&
                            (changeExtension(runFile, "html") %in% files ||
                            changeExtension(runFile, "out") %in% files)) ||
                          "outputProblems.RData" %in% files
        if (diffFiles && startedAnalysis) {
          useLastDir = FALSE
          currentFiles = thisCF
        } else {
          useLastDir = TRUE
        }
        currentFiles = thisCF # restore!!
      } # end if succesfully loaded 'currentFiles' from "shinyGraderCF.RData"
    } # end if "shinyGraderCF.RData" exists
  } # end if lastDir != 0
  
  # Create new subdirectory if needed
  if (!useLastDir) {
    thisDir = lastDir + 1
    dirPath = file.path(probFolder, thisDir)
    if (!dir.create(dirPath, showWarnings=FALSE)) {
      dualAlert("Sandbox error", paste("Cannot create", dirPath))
      return(NULL)
    }
    save(currentFiles, file=file.path(probFolder, thisDir, "shinyGraderCF.RData"))
  } else {
    thisDir = lastDir
  }
  sandbox = file.path(probFolder, thisDir)
  
  # Copy files to sandbox
  if (!is.null(currentFiles)) {
    if (is.null(currentFiles$runDf)) {
      runFile = ""
    } else {
      runFile = currentFiles$runDf[1, "outName"]
    }
    allDf = rbind(currentFiles$runDf, currentFiles$reqDf, currentFiles$optDf)
    inTime = file.info(file.path(allDf$directory, allDf$inName))$mtime
    outTime = file.info(file.path(sandbox, allDf$directory, allDf$outName))$mtime
    for (row in seq(along.with=allDf$outName)) {
      this = allDf[row, ]
      isRunFile = this$outName == runFile
      outDir = file.path(sandbox, this$directory)
      copy = FALSE
      if (is.na(outTime[row])) {
        copy = TRUE
        if (this$directory != "." && !dir.exists(outDir)) {
          if (!dir.create(file.path(outDir))) {
            dualAlert("Sandbox error", paste("Cannot create", outDir))
            return(NULL)
          }
        }
      } else {
        if (inTime[row] > outTime[row])
          copy = TRUE
      }
      if (copy) {
        from = file.path(this$directory, this$inName)
        to = file.path(outDir, this$outName)
        if (!copyWithPrejudice(from, to, isRunFile)) {
          dualAlert("Sandbox error", 
                    paste("Cannot copy", from, "to", outDir))
          return(NULL)
        }
      }
    }
  }
  return(sandbox)
} # end setupSandbox()


# Copy a file, with specific edits based on file type,
# preserving the modify time
#
copyWithPrejudice = function(from, to, isRunFile) {
  extension = tolower(gsub("(.*)([.])(.*)", "\\3", from))
  if (! extension %in% c("py", "r", "rmd", "sas")) {
    return(file.copy(from, to, overwrite=TRUE, copy.date=TRUE))
  }
  
  # Students tend to put dir() or ? in Python code.
  # We want to remove that.
  if (extension == "py") {
    code = try(suppressWarnings(readLines(from)), silent=TRUE)
    if (is(code, "try-error")) return(FALSE)
    code = gsub("((^|\\n)[:blank:]*)([?])",
                  "\\1### ?", code)
    code = gsub("((^|\\n)[:blank:]*)help[(]",
                  "\\1### dir(", code)
    if (is(try(write(code, to), silent=TRUE), "try-error")) return(FALSE)
  
  # Students tend to put help() or ? in R code.
  # We want to remove that.
  } else if (extension == "r") {
    code = try(suppressWarnings(readLines(from)), silent=TRUE)
    if (is(code, "try-error")) return(FALSE)
    code = gsub("((^|\\n)[:blank:]*)([?])",
                  "\\1### ?", code)
    code = gsub("((^|\\n)[:blank:]*)help[(]",
                  "\\1### help(", code)
    if (is(try(write(code, to), silent=TRUE), "try-error")) return(FALSE)
    return(TRUE)
    
  # Students tend to put help() or ? in Rmd code.
  # We want to remove that and assure output is to html.
  } else if (extension == "rmd") {
    code = try(suppressWarnings(readLines(from)), silent=TRUE)
    if (is(code, "try-error")) return(FALSE)
    code = gsub("((^|\\n)[:blank:]*)([?])",
                  "\\1### ?", code)
    code = gsub("((^|\\n)[:blank:]*)help[(]",
                  "\\1### help(", code)
    code = gsub("pdf_document",
                  "html_document", code)
    code = gsub("(w|W)ord_document",
                  "html_document", code)
    if (is(try(write(code, to), silent=TRUE), "try-error")) return(FALSE)
    
  # Our convention for SAS is to use '%LET wd=.;' to specify
  # the working directory (to allow an alternate version when
  # on University Edition).  Here we change the alternate
  # version to the standard (native SAS) version.
  } else if (extension == "sas") {
    code = try(suppressWarnings(readLines(from)), silent=TRUE)
    if (is(code, "try-error")) return(FALSE)
    code = gsub("((^|\\n)\\s*)(%LET\\s+WD\\s*=.*;)",
                  "\\1%LET WD=.;", code, ignore.case=TRUE)
    if (isRunFile) {
      code = c("ODS HTML CLOSE;",
               paste0("ODS HTML FILE='", changeExtension(basename(to), "html"), "';"),
               "ODS GRAPHICS ON;\n",
               code,
               "ODS HTML CLOSE;")
    }
    if (is(try(write(code, to), silent=TRUE), "try-error")) return(FALSE)
  } else {
    stop("coding error")
  }
  
  return(TRUE)
}

# Run a user's code
runCode = function(path, runFile) {
  # Move to sandbox
  wd = getwd()
  if (is(try(setwd(path), silent=TRUE), "try-error")) {
    dualAlert("Error running code", paste("Cannot change director to ", path))
    return(1)
  }
  on.exit(setwd(wd))
  
  # Get execution type
  ext = gsub("(.*)([.][^.]+)", "\\2", runFile)

  if (ext == ".R") {
    rtn = runR(runFile)
  } else if (ext == ".Rmd") {
    rtn = runRmd(runFile)
    # Convert crash (e.g., missing library, to output)
    if (rtn && attr(rtn, "exitCode")) {
      crashReport = try(readLines("runRmd.out", warn=FALSE), silent=TRUE)
      if (!is(crashReport, "try-error")) {
        crashReport = grep("^[ ]*Error in", crashReport, value=TRUE)
        if (length(crashReport) > 0) {
          crashReport = c("<html><head><title>Crash Report</title></head>",
                          paste("<body><p>Exit code =", attr(rtn, "exitCode"), "</p>"),
                          "<code>", paste(crashReport, collapse="\n"), "</code>",
                          "</head></html>")
          writeMsg = try(write(crashReport, file=attr(rtn, "outFile")), silent=TRUE)
          if (is(writeMsg, "try-error")) {
            rtn[1] = FALSE
          }
        }
      }
    }
  } else if (ext == ".py") {
    rtn = runPy(runFile)
  } else if (ext == ".sas") {
    rtn = runSas(runFile)
  } else {
    dualAlert("Run Code Error", paste(ext, "is not a valid code extension"))
    return(FALSE)
  }
  
  return(rtn)
}

# Split a specification string containing a set if code specification
# by section ([filename] at the beginning of a line).
# Return a named list based on section filename.  Call the first element "runfile"
# if present and unnamed.
splitCodeRubric = function(spec) {
  if (length(spec) == 0 || (length(spec)==1 && spec=="")) return(NULL)
  spec = strsplit(spec, split="\n")[[1]]
  boundaries = grep("^[[].+]$", spec)
  
  # Just an unnamed section:
  if (length(boundaries) == 0) return(list(runFile=spec))
  
  # Drop any terminal section name
  nLines = length(spec)
  nSec = length(boundaries)
  if (boundaries[nSec] == length(spec)) {
    boundaries = boundaries[-nSec]
    nSec = nSec - 1
    if (nSec == 0) {
      if (nLines == 1) {
        return(NULL)
      } else {
        return(list(runFile=spec[-nLines]))
      }
    }
  }
  
  # Store sections
  secNames = sub("(^[[])(.+)(]$)", "\\2", spec[boundaries])
  rtn = vector("list", nSec)
  names(rtn) = secNames
  start = boundaries + 1
  if (nSec==1) {
    end = nLines
  } else {
    end = c(boundaries[-1] - 1, nLines)
  }
  for (sec in 1:nSec) {
    if (end[sec] >= start[sec])
      rtn[[sec]] = spec[start[sec]:end[sec]]
  }
  
  # Add in runFile specification if present
  if (boundaries[1] > 1) {
    rtn = c(list(runFile=spec[1:boundaries[1] - 1]), rtn)
  }
  
  rtn = rtn[!sapply(rtn, is.null)]
  return(rtn)
}

# Check code (required, anathema, blanks, comments)
# 
# INPUT:
#   path: relative location of sandbox
#   cf: current files (see findCurrentFiles())
#   rubric: full grading rubric (see getProblemTabs.R)
#
# OUTPUT:
#   A "problems" data.frame with columns for file, pts,
#     msg, pattern, fixed, found, badRE, anatmea, mention,
#     and dock.  The 'extra' attribute has additional code
#     information in the from of a table with columns for each 
#     code file and rows for "blanks" and "comments".
#
# SIDE EFFECTS:
#   output object is stored in codeProblems.RData
checkCode = function(path, cf, rubric) {
  # Split rubric code requirements and anathemas by code file
  inputReq = splitCodeRubric(rubric$inputReq)
  inputAnath = splitCodeRubric(rubric$inputAnath)
  if(length(cf$runMissing) == 0) {
    runName = cf$runDf$outName
    runNonCanvas = cf$runDf$canvasFlag == FALSE
  } else {
    runName = cf$runMissing
    runNonCanvas = FALSE
  }
  m = match("runFile", names(inputReq))
  if (!is.na(m)) {
    if (runNonCanvas) {
      dualAlert("Bad rubric",
                "Rubric's code requirements cannot refer to a non-Canvas file")
      inputReq['runFile'] = NULL
    } else {
      names(inputReq)[m] = runName
    }
  }
  m = match("runFile", names(inputAnath))
  if (!is.na(m)) {
    if (runNonCanvas) {
      dualAlert("Bad rubric",
                "Rubric's code anathema cannot refer to a non-Canvas file")
      inputAnath['runFile'] = NULL
    } else {
      names(inputAnath)[m] = runName
    }
  }

  # Setup for analyis by code file
  allSectionNames = unique(c(names(inputReq), names(inputAnath)))
  allCodeNames = c(runName, cf$reqDf$outName[cf$reqDf$canvasFlag])
  if (length(cf$reqMissing) > 0) {
    cleanNames = gsub("(.*)([{]{2}.+[}]{2})", "\\1", cf$reqMissing)
    allCodeNames = c(allCodeNames, cleanNames)
  }
  
  # Check that rubric refers only to files for this problem
  secNotInCode = allSectionNames %in% allCodeNames == FALSE
  if (any(secNotInCode)) {
    badSecs = allSectionNames[secNotInCode]
    dualAlert("Rubric error",
              paste("Code input and/or anathema refers to missing code:",
                    paste(badSecs, collapse=", ")))
    
    inputReq = inputReq[names(inputReq) %in% badSecs == FALSE]
    if (length(inputReq) == 0) inputReq = NULL
    inputAnath = inputAnath[names(inputAnath) %in% badSecs == FALSE]
    if (length(inputAnath) == 0) inputAnath = NULL
  }
  
  # Setup to analyze anathemas and requirements in (nearly) the same way
  nReq = length(inputReq)
  nAnath = length(inputAnath)
  fromAnathema = rep(c(FALSE, TRUE), c(nReq, nAnath))
  both = c(inputReq, inputAnath)
  # Get a list of data.frames of specification results, one per combo
  # of file and Req vs. Anath
  if (all(sapply(both, function(x) length(x)==0))) {
    return(NULL)
  }
  codeProblems = lapply(seq(along.with=both), 
    function(index) {
      file = names(both)[index]
      anathema = fromAnathema[index]
      specs = both[[index]]
      txt = try(suppressWarnings(readLines(file.path(path, file), warn=FALSE)), silent=TRUE)
      if (is(txt, "try-error")) {
        fileFound = FALSE
        txt = ""
      } else {
        fileFound = TRUE
      }
      dtf = testSpecs(specs, txt)
      
      # Count blanks and comments
      if (!is.null(dtf)) {
        dtf = cbind(file=file, dtf, anathema=anathema, fileFound)
        if (fileFound) {
          attr(dtf, file) = c(blanks=length(grep("^[[:blank:]]*$", txt)))
          ext = tolower(gsub("(.*)([.])(.*)", "\\3", file))
          if (ext %in% c("r", "py")) {
            commentCnt = length(grep("^[[:blank:]]*[#][[:blank:]]*[^[:blank:]]+$",
                                     txt))
          } else if (ext == "rmd") {
            commentCnt = NA
          } else if (ext == "sas") {
            commentCnt = length(grep("^[[:blank:]]*[/][*]", txt))
          } else {
            commentCnt = 0
            dualAlert("Not yet programmed",
                      paste0("No comment count code for ", ext))
          }
          attr(dtf, file) = c(blanks=length(grep("^[[:blank:]]*$", txt)),
                              comments=commentCnt)
        } else {
          attr(dtf, file) = c(blanks=0, comments=0)
        }
      }
      
      return(dtf)
    }
  )

  # Compute extra information on number of blanks and comments
  extra = lapply(codeProblems,
                 function(dtf) {
                   x = attributes(dtf)
                   x = x[ ! names(x) %in% c("names", "class", "row.names")]
                   return(x)
                 })
  enames = lapply(extra, names)
  isNull = sapply(enames, is.null)
  extra = sapply(extra[!isNull], "[[", i=1)
  colnames(extra) = enames[!isNull]
  extra = extra[, ! duplicated(enames), drop=FALSE]

  # Compute additonal information
  problems = do.call(rbind, codeProblems)
  problems$mention = (problems$found == problems$anathema)  & !problems$badRE
  problems$dock = 0
  problems$dock[problems$mention] = problems$pts[problems$mention]
  
  # Store 'extra' as an attribute
  attr(problems, "extra") = extra
  
  # Both save and return the final data.frame
  save(problems, file=file.path(path, "codeProblems.RData"))
  return(problems)
}  # end checkCode()

# test specification for a text
testSpecs = function(specs, txt) {
  # if (length(specs) == 0) {
  #   return(setNames(data.frame(matrix(ncol=6, nrow=0)),
  #                   c("pts", "msg", "pattern", "fixed", "found", "badRE")))
  # }
  
  rslt = lapply(specs,
    function(spec) {
      pspec = parseSpec(spec)
      found = try(grep(pspec$pattern, txt, fixed=pspec$fixed), silent=TRUE)
      if (is(found, "try-error")) {
        err = as.character(attr(found, "condition"))
        err = sub('(.*)(, reason )(.*)', "\\3", err)
        nc = nchar(err)
        if (substring(err, nc) == "\n") {
          err = substring(err, 1, nc - 1)
          nc = nc - 1
        }
        if (substring(err, 1, 1) =="'" && substring(err, nc) =="'")
          err = substring(err, 2, nc - 1)
        dualAlert("Rubric error",
                  paste(pspec$pattern, "is not a valid regular expression\n", err))
        found = FALSE
        badRE = TRUE
      } else {
        found = length(found) > 0
        badRE = FALSE
      }
      return(cbind(pspec, found, badRE))
    }
  )
  
  return(do.call(rbind, rslt))
}


# parse a specification
parseSpec = function(spec) {
  spec = trimws(spec)
  # Compute points and message from the "{pts:message}" prefix
  preLoc = regexpr("(^[{].*?[}])", spec)
  if (preLoc[1] == -1) {
    pts = 0
    msg = ""
    pattern = spec
  } else {
    preLen = attr(preLoc, "match.length")
    prefix = substring(spec, 2, preLen - 1)
    if (prefix == "") {
      pts = 0
      msg = ""
    } else {
      pm = strsplit(prefix, ":")[[1]]
      pts = suppressWarnings(as.numeric(pm[1]))
      if (is.na(pts)) {
        pts = 0
        if (pm[1]!="") {
          dualAlert("Rubric error",
                    paste0("Invalid '{pts:msg}' prefix in ", prefix, "."))
        }
      }
      msg = ifelse(length(pm) > 1, trimws(paste(pm[-1], collapse=":")), "")
    }
    pattern = trimws(substring(spec, preLen+1))
  } # end dealing with the prefix
  
  
  # Set for 'fixed' or not
  c1 = substring(pattern, 1, 1)
  cN = substring(pattern, nchar(pattern))
  if (c1 == cN && c1 %in% c("'", '"')) {
    pattern = substring(pattern, 2, nchar(pattern) - 1)
    fixed = TRUE
  } else {
    fixed = FALSE
  }
  
  return(data.frame(pts, msg=I(msg), pattern=I(pattern), fixed))
}

# Run R Code
# Important note: on Windows, new libraries are probably installed in
# something like C:\\Users\\myUserName\\Documents\\R\\win-library\\3.4.
# To allow access to these libraries, set a system environmenal variable
# (Control Panel / System / Advanced / Environment) called R_LIBS_USER
# to that location, but with the final "#.#" replaced with "%v".
runR = function(runFile) {
  outFile = changeExtension(runFile, "out")
  args = paste("CMD BATCH --no-restore --no-save --quiet", runFile,
               outFile)
  exitCode = try(system2("R", args), silent=TRUE)
  if (is(exitCode, "try-error")) {
    return(FALSE)
  }
  rtn = TRUE
  attr(rtn, "exitCode") = exitCode
  attr(rtn, "outFile") = outFile
  return(rtn)
}

# Run Rmd Code
runRmd = function(runFile) {
  outFile = changeExtension(runFile, "html")
  write(c('library("knitr")',
          paste0('knit("', runFile,'")'),
          'library("markdown")',
          paste0('markdown::markdownToHTML("', changeExtension(runFile, "md"), '", ',
                 'output="', outFile, '")')),
        file="runRmd.R")
  args = "CMD BATCH --no-restore --no-save --quiet runRmd.R runRmd.out"
  exitCode = try(system2("R", args), silent=TRUE)
  if (is(exitCode, "try-error")) {
    return(FALSE)
  }
  rtn = TRUE
  attr(rtn, "exitCode") = exitCode
  attr(rtn, "outFile") = outFile
  return(rtn)
}

# Run Python Code
runPy = function(runFile) {
  outFile = changeExtension(runFile, "out")
  exitCode = try(system2("python", stdin=runFile, stdout=outFile,
                         stderr=changeExtension(runFile, "err")),
                 silent=TRUE)
  if (is(exitCode, "try-error")) {
    return(FALSE)
  }
  rtn = TRUE
  attr(rtn, "exitCode") = exitCode
  attr(rtn, "outFile") = outFile
  return(rtn)
}


# Run SAS Code
runSas = function(runFile) {
  outFile = changeExtension(runFile, "html")
  args = paste0("-SYSIN '", runFile,  "' -ICON -NOSPLASH -NONEWS -LOG '",
                changeExtension(runFile, "log"), "' -PRINT '",
                changeExtension(runFile, "lst"), "'")
  exitCode = try(system2(SasProg, args), silent=TRUE)
  if (is(exitCode, "try-error")) {
    return(FALSE)
  }
  rtn = TRUE
  attr(rtn, "exitCode") = exitCode
  attr(rtn, "outFile") = outFile
  return(rtn)
}

changeExtension = function(fname, ext) {
  return(gsub("(.*[.])(.*)", paste0("\\1", ext), fname))
}

# Check results
checkOutput = function(path, cf, rubric) {
  # get output requirements and anathemas from the rubric
  outputReq = splitCodeRubric(rubric$outputReq)
  outputAnath = splitCodeRubric(rubric$outputAnath)
  
  # Get the run name, and construct the output name
  if(length(cf$runMissing) == 0) {
    runName = cf$runDf$outName
    #outNonCanvas = cf$runDf$canvasFlag == FALSE
  } else {
    runName = changeExtension(cf$runMissing)
    #outNonCanvas = FALSE
  }
  extension = gsub("(.*)([.])(.*)", "\\3", runName)
  if (tolower(extension) %in% c("rmd", "sas")) {
    outName = changeExtension(cf$runDf$outName, "html")
  } else {
    outName = changeExtension(cf$runDf$outName, "out")
  }

  # Where no '[fname]' is supplied in the rubric, change the
  # list element names in outputReq and outputAnath from
  # 'runFile' to the contents of 'outName'.
  nReq = length(outputReq)
  nAnath = length(outputAnath)
  if (nReq > 0) {
    names(outputReq)[names(outputReq) == 'runFile'] = outName
  }
  if (nAnath > 0) {
    names(outputAnath)[names(outputAnath) == 'runFile'] = outName
  }
  
  # Setup to check output
  fromAnathema = rep(c(FALSE, TRUE), c(nReq, nAnath))
  both = c(outputReq, outputAnath)
  # Get a list of data.frames of specification results, one per combo of file and Req vs. Anath
  if (all(sapply(both, function(x) length(x)==0))) {
    return(NULL)
  }
  outputProblems = lapply(seq(along.with=both), 
                          function(index) {
                            file = names(both)[index]
                            anathema = fromAnathema[index]
                            specs = both[[index]]
                            txt = try(suppressWarnings(readLines(file.path(path, file), warn=FALSE)), silent=TRUE)
                            if (is(txt, "try-error")) {
                              fileFound = FALSE
                              txt = ""
                            } else {
                              fileFound = TRUE
                            }
                            dtf = testSpecs(specs, txt)
                            
                            if (!is.null(dtf)) dtf = cbind(file=file, dtf, anathema=anathema)
                            
                            return(dtf)
                          }
  )
  
  problems = do.call(rbind, outputProblems)
  problems$mention = (problems$found == problems$anathema)  & !problems$badRE
  problems$dock = 0
  problems$dock[problems$mention] = problems$pts[problems$mention]
  
  # Both save and return the final data.frame
  save(problems, file=file.path(path, "outputProblems.RData"))
  return(problems)
}  # end checkOutput()


# From a 'currentFiles' list, determine appropriateness of
# enabling analyzeCode, runCode, and analyzeOutput.
#
checkEnables = function(path, cf, probNum) {
  if(length(cf$runMissing) == 0) {
    runName = cf$runDf$inName
    outName = file.path(path, changeExtension(cf$runDf$outName, "out"))
    runIsCanvas = cf$runDf$canvasFlag
  } else {
    runName = cf$runMissing
    outName = file.path(path, changeExtension(runName, "out"))
    runIsCanvas = FALSE
  }
  rubricName = paste0("rubric", probNum, ".RData")
  codeProbName = file.path(path, "codeProblems.RData")
  outputProbName = file.path(path, "outputProblems.RData")
  earlyDate = as.POSIXct("1/1/1", format="%m/%d/%Y")
  
  analyzeCode = runCode = analyzeOutput = FALSE
  
  rubricTime = ifelse(file.exists(rubricName),
                      file.info(rubricName)$mtime,
                      earlyDate)
  hasRun = file.exists(runName)
  runFileTime = ifelse(hasRun, ifelse(length(cf$runMissing) > 0,
                                      file.info(runName)$mtime,
                                      cf$runDf$modifyTime),
                   earlyDate)
  hasUserCode = (hasRun && runIsCanvas) ||
                (!is.null(cf$reqDf) && any(cf$reqDf$canvasFlag)) ||
                (!is.null(cf$optDf) && any(cf$optDf$canvasFlag))
  missingReq = length(cf$reqMissing) > 0
  newestInput = max(runFileTime, cf$reqDf$modifyTime, cf$optDf$modifyTime)
  codeProblemsTime = ifelse(file.exists(codeProbName),
                            file.info(codeProbName)$mtime,
                            earlyDate)
  hasOutput = file.exists(outName)
  outTime = ifelse(hasOutput, file.info(outName)$mtime, earlyDate)
  outputProblemsTime = ifelse(file.exists(outputProbName),
                              file.info(outputProbName)$mtime,
                              earlyDate)
  
  analyzeCode = hasUserCode && 
                (codeProblemsTime == earlyDate ||
                 newestInput > codeProblemsTime ||
                 rubricTime > codeProblemsTime)
  
  runCode = hasRun && !missingReq &&
            (!hasOutput ||
             outTime < newestInput ||
             outTime < rubricTime)
  
  analyzeOutput = hasOutput && outTime >= runFileTime &&
                  (outputProblemsTime < outTime ||
                   outputProblemsTime < rubricTime)
  
  return(c(analyzeCode=analyzeCode,
           runCode=runCode,
           analyzeOutput=analyzeOutput))
}


codeAnalysisToTags = function(path, fname) {
  varLoaded = try(load(file.path(path, "codeProblems.RData")), silent=TRUE)
  if (is(varLoaded, "try-error") || length(varLoaded) != 1 || varLoaded != "problems") {
    dualAlert("Grading View Error", "Bad 'codeProblems.RData' file")
    tgs = p("not viewable")
    attr(tgs, "dock") = NA
    return(tgs)
  } else if (sum(problems$mention) == 0) {
      tgs = p("All good (nothing to mention)")
      attr(tgs, "dock") = 0 #sum(problems$dock)
      return(tgs)
  } else {
    probTags = apply(problems[problems$mention == TRUE, ], 1,
                     function(line) {
                       p(paste0(ifelse(line[['pts']] < 0, 
                                       paste0("Bonus of ", abs(line[['pts']]), " for"),
                                       ifelse(line[['pts']]==0, "Zero penalty for",
                                              paste0(line[['pts']], " points lost for"))),
                                ifelse(line[['anathema']], " code anathema (",
                                       " missing code ("),
                                line[['msg']], ") in '", line[['file']], "'."))
                     })
    names(probTags) = NULL # Needed!!!
    tgs = do.call(shiny::tags$div, probTags)
    attr(tgs, "dock") = sum(problems$dock)
    return(tgs)
  }
}


## Output analysis to tags
outputAnalysisToTags = function(path, fname) {
  varLoaded = try(load(file.path(path, "outputProblems.RData")), silent=TRUE)
  if (is(varLoaded, "try-error") || length(varLoaded) != 1 || varLoaded != "problems") {
    dualAlert("Grading View Error", "Bad 'outputProblems.RData' file")
    tgs = p("not viewable")
    attr(tgs, "dock") = NA
    return()
  } else if (sum(problems$mention) == 0) {
    tgs = p("All good (nothing to mention)")
    attr(tgs, "dock") = 0 # sum(problems$dock)
    return(tgs)
  } else {
    probTags = apply(problems[problems$mention == TRUE, ], 1,
                     function(line) {
                       p(paste0(ifelse(line[['pts']] < 0, 
                                       paste0("Bonus of ", abs(line[['pts']]), " for"),
                                       ifelse(line[['pts']]==0, "Zero penalty for",
                                              paste0(line[['pts']], " points lost for"))),
                                ifelse(line[['anathema']], " output anathema (",
                                       " missing output ("),
                                line[['msg']], ") in '", line[['file']], "'."))
                     })
    names(probTags) = NULL # Needed!!!
    tgs = do.call(shiny::tags$div, probTags)
    attr(tgs, "dock") = sum(problems$dock)
    return(tgs)
  }
}
# Un-escape HTML
# https://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

# Collect errors and warnings for R output
collectErrorsR = function(text, ignore=NULL) {
}

# Collect errors and warnings for Rmd output
collectErrorsRmd = function(text, ignore=NULL) {
  startAll = grep("^<pre><code", text)
  stopAll = grep("^</code></pre>", text)
  if (length(stopAll) != length(startAll)) {
    msg = "garbled html"
    attr(msg, "dock") = NA
    return(msg)
  }
  startErr = grep("^<pre><code>## Error", text)
  startWarn = grep("^<pre><code>## Warning", text)
  info = data.frame(start=startAll, stop=stopAll, warn=FALSE, err=FALSE,
                    message=I(NA_character_),
                    causeN=NA_integer_, cause=I(NA_character_))
  info$warn[info$start %in% startWarn] = TRUE
  info$err[info$start %in% startErr] = TRUE
  info$causeN[info$warn] = which(info$warn) - 1
  info$causeN[info$err] = which(info$err) - 1
  # If Error immediately follows Warning, the cause of the error
  # is one prior
  temp = which(info$warn)
  info$causeN[info$err][info$causeN[info$err] %in% temp] = 
    info$causeN[info$err][info$causeN[info$err] %in% temp] - 1
  nErr = sum(info$err)
  nWarn = sum(info$warn)
  if (nErr + nWarn == 0) {
    msg = "all OK"
    attr(msg, "dock") = 0
    return(msg)
  }
  stopAll = grep("^</code></pre>", text)
  stopErr = sapply(startErr,
                   function(start) {
                     stop = stopAll[stopAll > start]
                     if (length(stop) == 0) return(NA)
                     return(stop[1])
                   })
  EW = info$err | info$warn
  info$message[EW] = apply(info[EW, ], 1,
                           function(row) {
                             msg = paste(text[as.numeric(row["start"]):
                                                as.numeric(row["stop"])],
                                         collapse=" ")
                             return(substring(unescape_html(msg), 4))
                           })
  
  info$cause[EW] = sapply(info[EW, "causeN"],
                          function(n) {
                            row = info[n, ]
                            cause = paste(text[as.numeric(row["start"]):
                                                 as.numeric(row["stop"])],
                                          collapse="\n")
                            cause = unescape_html(cause)
                            #browser()
                            closest = try(suppressWarnings(parse(text=cause)), silent=TRUE)
                            if (!is(closest, "try-error") && length(closest) > 0)
                              cause = as.character(closest[length(closest)])
                            return(cause)
                          })
  info = info[EW, ]
  info$dock = 222
  return(info)
}

