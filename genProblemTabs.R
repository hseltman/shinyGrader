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


probPanelCodeOne = c(
  '  tabPanel("Problem ##",',
  '    fluidRow(column(2, selectInput("runFileType##", "Type of code to run",',
  '                                   c("(none)", ".R", ".Rmd", ".R or .Rmd",',
  '                                     ".py", ".sas"))),',
  '             column(10, textInput("runFileName##", "Name (or re) of code to run"))),',
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
  '      "that many points for missing requirement or included anathemas.  (Negative values",',
  '      "can be used for bonus points)"),',
  '    textAreaInput("inputReq##", "Input requirements" ,width="720px", rows=6),',
  '    textAreaInput("inputAnath##", "Input anathemas", width="720px", rows=6),',
  '    textAreaInput("outputAnath##", "Output anathemas", width="720px", rows=6),',
  '    textAreaInput("outputAnath##", "Output anathemas", width="720px", rows=6),',
  '    HTML("&nbsp;"),',
  '    p("An optional initial {#} or {#: myComment} is allowed for the following:"),',
  '    textInput("minComments##", "Minimum number of comment lines", width="100%"),',
  '    textInput("minBlanks##", "Minimum number of blank lines", width="100%"))'
)


problemCount = 3
probPanelCode = lapply(1:problemCount,
                       function(index) {
                         txt = gsub("##", paste(index), probPanelCodeOne, fixed=TRUE)
                         return(paste(txt, collapse="\n"))
                       })

probPanelCode = paste0("tabsetPanel(",
                       paste(probPanelCode, collapse=",\n"), "\n)")



