# Generate a "problem" tabset for the shinyGrader project
# H. Seltman, June 2018

# Generate code like:
#
# tabPanel("P1",
#          radioButtons("codefile", label="Choose a codefile for Problem 1:",
#            choices="(None)"),
#          textInput("inputReq", "Input requirements", ""),
#          textInput("inputAnath", "Input anathemas", ""))
# )
#
# from a string vector containing the text with "##" for the problem number.
#
# Also store the widget ids in 'problemInputIds'.  This is used throughout
# the code so that a change to PROBLEM_COUNT will require no downstream
# code changes.
#
# This precludes writing UI code for each problem separately, but
# any changes to 'probPanelCodeOne' will require many other changes
# to the ShinyGrader app.
# Note that actionButton()s are allowed but not included in 'problemInputIds'.
#
# Important: "value=" must be supplied and must be last.
#
# Current code required:
#  actionButton("saveRubric##AsDefault", "Save as defaults for all new rubrics"))


probPanelCodeOne = c(
  '  tabPanel("Problem ##",',
  '    h3("Problem ## Rubric"),',
  '    HTML("&nbsp"),',
  '    numericInput("initialPoints##", "Initial points for this problem", value=0),',
  '    p(strong("File naming rules:"), br(),',
  '      "An initial \\"@\\" indicates a non-Canvas file.  ", ',
  '      "An initial \\"^\\" indicates case-insensitive.", br(),',
  '      "Adding \\"{{reg. exp.}}\\" indicates secondary matching (with renaming).", br(),',
  '      "Just a \\"{{reg. exp.}}\\" indicates matching (without renaming).", br(),',
  '      "A final \\"-\\" indicates that the file should be deleted from the sandbox after use.",',
  '      br(), "The run file may be .R, .Rmd, .sas, .py, or no extension for a script file.",',
  '      br(), "The run file may use .RRmd to indicate either .R or .Rmd is acceptable."),',
  '    HTML("&nbsp;"),',
  '    textInput("runFile##", "File name of code to run", width="35%", value=""),',
  '    textInput("reqFiles##",',
  '              paste("Required files (semicolon separated)"), width="100%", value=""),',
  '    textInput("optFiles##",',
  '              paste("Optional files (semicolon separated)"), width="100%", value=""),',
  '    HTML("&nbsp"),',
  '    fluidRow(column(3, numericInput("runCaseViolPts##",',
  '                                    "Pts. lost for case violation", value=0, width="100%")),',
  '             column(9, textInput("runCaseViolText##", "Optional case violation text",',
  '                                  width="100%", value=""))),',
  '    fluidRow(column(3, numericInput("looseFileNamePts##",',
  '                                    "Pts. lost for using secondary matching",',
  '                                     value=0, width="100%")),',
  '             column(9, textInput("looseFileNameText##",',
  '                                  "Optional secondary matching text", width="100%",',
  '                                  value=""))),',
  '    HTML("&nbsp"),',
  '    p("Requirements and anathemas (anti-requirements) are interpreted as regular",',
  '      "expressions, unless quoted.  If you precede a regular expression with a tilde, the",',
  '      "test will be case insensitive.  An optional initial {#} or {#: myComment} deducts",',
  '      "that many points for missing requirements or included anathemas.  If the Code to Run",',
  '      "is not a student file, use [studentFileName] as a header line to indicate which",',
  '      "file(s) to check.  (Note: Negative values can be used for bonus points)"),',
  '    textAreaInput("inputReq##", "Input requirements" ,width="720px", rows=6, value=""),',
  '    textAreaInput("inputAnath##", "Input anathemas", width="720px", rows=6, value=""),',
  '    textAreaInput("outputReq##", "Output requirements", width="720px", rows=6, value=""),',
  '    textAreaInput("outputAnath##", "Output anathemas", width="720px", rows=6, value=""),',
  '    textAreaInput("warnIgnore##", "Warning text to ignore",',
  '                  width="720px", rows=4, value=""),',
  '    textAreaInput("errIgnore##", "Error text to ignore",',
  '                  width="720px", rows=4, value=""),',
  '    HTML("&nbsp;"),',
  '    p("Enter numbers for the following:"),',
  '    fluidRow(column(4, numericInput("minComments##", "Minimum number of comment lines", ',
  '                                    value=0)),',
  '             column(3, numericInput("commentPts##", "Point loss for below minimum", ',
  '                                    value=0))),',
  '    fluidRow(column(4, numericInput("minBlanks##", "Minimum number of blank lines", ,',
  '                                    value=0)),',
  '             column(3, numericInput("blankPts##", "Point loss for below minimum", ,',
  '                                    value=0))),',
  '    fluidRow(column(4, numericInput("warnPtsLost##", "Points lost per warning", value=0)),',
  '             column(3, numericInput("maxWarnPtsLost##", "Maximum warning points lost", ',
  '                                   value=999))),',
  '    fluidRow(column(4, numericInput("errPtsLost##", "Points lost per error", value=0)),',
  '             column(3, numericInput("maxErrPtsLost##", "Maximum error points lost", ',
  '                                    value=999))),',
  '    HTML("&nbsp;"),',
  '    actionButton("saveRubric##AsDefault", "Save as defaults for all new rubrics"))'
)



# Extract inputIds for each problem into a list
temp = regexpr('"[a-zA-Z0-9_]*##"', probPanelCodeOne)
problemInputIds = regmatches(probPanelCodeOne, temp)
problemInputIds = substring(problemInputIds, 2, nchar(problemInputIds) - 3)


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

# Code for updating Problem tabs' rubric widgets in an observer
probUpdateCode = character(0)
properCase = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
indexedNames = names(rubricDefaults) # (to avoid complex quoting)
for (index in 1:length(rubricDefaults)) {
  name = indexedNames[index]
  type = properCase(attr(rubricDefaults[[index]], "type"))
  for (problem in 1:PROBLEM_COUNT) {
    nameNum = paste0(name, problem)
    if (type == "Select") {
      code = paste0("update", type, "Input(",
                    "session=session, ",
                    "inputId='", nameNum, "', ",
                    "select=rubNow[[", problem, "]]$", indexedNames[index], ")")
    } else {
      code = paste0("update", type, "Input(",
                      "session=session, ",
                      "inputId='", nameNum, "', ",
                      "value=rubNow[[", problem, "]]$", indexedNames[index], ")")
    }
    probUpdateCode = c(probUpdateCode, code)
  }
}
