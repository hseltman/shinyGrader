# Generate a "problem" tabset for the shinyGrader project
# H. Seltman, June 2018

# Generate code like:
#
# tabPanel("P1",
#          shinyjs::disabled(actionButton("submitProblemRubric##", "Save changes")),
#          radioButtons("codefile", label="Choose a codefile for Problem 1:",
#            choices="(None)"),
#          textInput("inputReq", "Input requirements", ""),
#          textInput("inputAnath", "Input anathemas", ""))
# )
#
# from a string vector containing the text with "##" for the problem number.

# This precludes writing UI code for each problem separately, but
# any changes to 'probPanelCodeOne' will require many other changes
# to ShinyGrader.


probPanelCodeOne = c(
  '  tabPanel("Problem ##",',
  '     h3("Problem ## Rubric"),',
  '    shinyjs::disabled(actionButton("submitProblemRubric##", "Save changes")),',
  '    HTML("&nbsp"),',
  '    numericInput("initialPoints##", "Initial points for this problem", 0),',
  '    fluidRow(column(2, textInput("runFileName##", "Name (or re) of code to run")),',
  '             column(10, selectInput("runFileType##", "Type of code to run",',
  '                                    c("(none)", ".R", ".Rmd", ".R or .Rmd",',
  '                                      ".py", ".sas")))),',
  '    textInput("filesToCopy##",',
  '              paste("Names (semicolon separated) or regular expressions of other",',
  '                    "files to copy to sandbox"), width="100%"),',
  '    textInput("filesToCopyAndDelete##",',
  '              paste("Names (semicolon separated) or regular expressions of other",',
  '                    "files to copy to sandbox and delete afterwards"), width="100%"),',
  '    HTML("&nbsp"),',
  '    checkboxInput("doPdf##", "Attempt to make pdf"),',
  '    HTML("&nbsp"),',
  '    p("Requirements and anathemas (anti-requirements) are interpreted as regular",',
  '      "expressions, unless quoted.  An optional initial {#} or {#: myComment} deducts",',
  '      "that many points for missing requirements or included anathemas.  (Negative",',
  '      "values can be used for bonus points)"),',
  '    textAreaInput("inputReq##", "Input requirements" ,width="720px", rows=6),',
  '    textAreaInput("inputAnath##", "Input anathemas", width="720px", rows=6),',
  '    textAreaInput("outputReq##", "Output requirements", width="720px", rows=6),',
  '    textAreaInput("outputAnath##", "Output anathemas", width="720px", rows=6),',
  '    textAreaInput("warnIgnore##", "Warning text to ignore",',
  '                  width="720px", rows=4),',
  '    textAreaInput("errIgnore##", "Error text to ignore",',
  '                  width="720px", rows=4),',
  '    HTML("&nbsp;"),',
  '    p("Enter numbers for the following:"),',
  '    fluidRow(column(4, numericInput("minComments##", "Minimum number of comment lines", 0)),',
  '             column(3, numericInput("commentPts##", "Point loss for below minimum", 0))),',
  '    fluidRow(column(4, numericInput("minBlanks##", "Minimum number of blank lines", 0)),',
  '             column(3, numericInput("blankPts##", "Point loss for below minimum", 0))),',
  '    fluidRow(column(4, numericInput("warnPtsLost##", "Points lost per warning", 0)),',
  '             column(3, numericInput("maxWarnPtsLost##", "Maximum warning points lost", 999))),',
  '    fluidRow(column(4, numericInput("errPtsLost##", "Points lost per error", 0)),',
  '             column(3, numericInput("maxErrPtsLost##", "Maximum error points lost", 999))))'
)


# Relys on the value of 'PROBLEM_COUNT', set in 'setup.R'.
probPanelCode = lapply(1:PROBLEM_COUNT,
                       function(index) {
                         txt = gsub("##", paste(index), probPanelCodeOne, fixed=TRUE)
                         return(paste(txt, collapse="\n"))
                       })

probPanelCode = paste0("tabsetPanel(",
                       paste(probPanelCode, collapse=",\n"), "\n)")

# Extract inputIds for each problem into a list
temp = regexpr('"[a-zA-Z0-9_]*##"', probPanelCodeOne)
problemInputIds = regmatches(probPanelCodeOne, temp)
problemInputIds = substring(problemInputIds, 2, nchar(problemInputIds) - 3)
temp = match("submitProblemRubric", problemInputIds)
if (is.na(temp)) stop("no 'submitProblemRubric' in 'genProblemTabs.R'")
problemInputIds = problemInputIds[-temp]
problemInputIds = lapply(1:PROBLEM_COUNT,
                         function(x) paste(problemInputIds, x, sep=""))


# Find defaults from a string vector like 'probPanelCodeOne' above.
getInputDefaults = function(code) {
  # Get code as a single string
  html = as.character(eval(parse(text=code)))
  
  # Handle textInput, numericInput, and checkboxInput
  gre = gregexpr('<input id="[a-zA-Z0-9_]*##" .*?/>', html)
  txt = regmatches(html, gre)[[1]]
  locs = regexpr('"[a-zA-Z0-9_]*', txt)
  ids = substring(regmatches(txt, locs), 2)
  locs = regexpr('type="[a-z]+', txt)
  types = substring(regmatches(txt, locs), 7)
  checkValues = rep(FALSE, sum(types == "checkbox"))
  checkValues[grepl('checked="checked"', txt[types == "checkbox"])] = TRUE
  # Note: 'value=' may be missing for checkbox
  locs = regexpr('value=".*"', txt)
  defaults = lapply(txt, function(x) {
    locs = regexpr('value=".*"', x)
    val = regmatches(x, locs)
    if (length(val) > 0) val = substring(val, 8, nchar(val)-1)
    return(val)
  })
  defaults[types == "checkbox"] = checkValues
  defaults[types == "number"] = as.numeric(defaults[types == "number"])
  names(defaults) = ids
  types[types=="number"] = "numeric"
  for (i in 1:length(defaults)) attr(defaults[[i]], "type") = types[i]
  
  # Handle selectInput
  gre = gregexpr('<select id="[a-zA-Z0-9_]*##".*?</option>', html)
  txt = regmatches(html, gre)[[1]]
  if (length(txt) > 0) {
    locs = regexpr('"[a-zA-Z0-9_]*', txt)
    ids = substring(regmatches(txt, locs), 2)
    locs = regexpr(' selected>.*?</option>', txt)
    values = regmatches(txt, locs)
    values = as.list(substring(values, 11, nchar(values) - 9))
    names(values) = ids
    for (i in 1:length(values)) attr(values[[i]], "type") = "select"
    defaults = modifyList(defaults, values)
  }
  
  # Handle textAreaInput
  gre = gregexpr('<textarea id="[a-zA-Z0-9_]*##".*?</textarea>', html)
  txt = regmatches(html, gre)[[1]]
  if (length(txt) > 0) {
    locs = regexpr('"[a-zA-Z0-9_]*', txt)
    ids = substring(regmatches(txt, locs), 2)
    locs = regexpr('>.*</textarea>', txt)
    values = regmatches(txt, locs)
    values = as.list(substring(values, 2, nchar(values) - 11))
    names(values) = ids
    for (i in 1:length(values)) attr(values[[i]], "type") = "textArea"
    defaults = modifyList(defaults, values)
  }

  return(defaults)
}

# store rubric defaults
rubricDefaults = getInputDefaults(probPanelCodeOne)
# (note: could be updated here from a user global file)
