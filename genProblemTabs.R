# Generate a "problem" tabset for the shinyGrader project
# H. Seltman, June 2018

# Generate code like:
# tabPanel("P1",
#          radioButtons("codefile", label="Choose a codefile for Problem 1:",
#            choices="(None)"),
#          textInput("inputReq", "Input requirements", ""),
#          textInput("inputAnath", "Input anathemas", ""),
#          textInput("outputReq", "Output requirements", ""),
#          textInput("outputAnath", "Output anathemas", "")
# )

# from a string vector containing the text with "##" for the problem number.

probPanelCodeOne = c(
  '  tabPanel("Problem ##",',
  '    checkboxGroupInput("codefiles##", label="Choose a codefile(s) for Problem ##:",',
  '      choices="(None)"),',
  '    textInput("inputReq##", "Input requirements", ""))'
)


problemCount = 3
probPanelCode = lapply(1:problemCount,
                       function(index) {
                         txt = gsub("##", paste(index), probPanelCodeOne, fixed=TRUE)
                         return(paste(txt, collapse="\n"))
                       })
probPanelCode = paste0("tabsetPanel(",
                       paste(probPanelCode, collapse=",\n"), "\n)")

