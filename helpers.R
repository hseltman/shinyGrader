# helper functions for shinyGrader
# H. Seltman, June 2018

# Functions in this file:
#   genUiCode()
#   textToConfigList()
#   writeConfig()

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
  badIds = is.na(match(ids, varNames))
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
