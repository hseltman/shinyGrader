# shinyGrader
Automatic grading of R, Pytnon, and SAS in a Shiny App (tuned to Canvas course management system)

This Shiny App (https://shiny.rstudio.com/) manages grading and regrading of homework assignments in R, Python or SAS.  Configuration files specify ancillary files, various options, and a grading rubric.  Code is run in a sandbox.  The rubric uses regular expression matching on code input and output.

The app understands the naming conventions of the Canvas course management system.  This includes converting student file names from "file.ext" to "lastFirst_[late_]studentNumber_fileNumber_file[-redo#].ext" where lastFirst is the lower case concatenation of the student's last and first names, "late" is inserted if the submission is past the due date, studentNumber is a number consistent for each student, fileNumber is a number unique to each submitted file, and "redo#" is "1" for the first resubmission, etc.  This means that for students with identical last and first names, the second field (studentNumber) is the unique identifier.  The app will also accept file names of the form "identifier_file.ext" where "identifier" might, e.g., be "solution".

Each directory is intended to contain one assignment, by that assignment may have several separate submitted code files per student, each with it's own setup and rubric.

The system honors a global configuration file, directory level global configuration files, a global specific configuration file, and a per-graded-assignment specific configuration file.  The global configuration file specifies which filenames are assignments to be grades, and the specific configurations files define how the assignment is setup (e.g., ancillary files) as well as the grading rubric.  Analyses are considered "stale" if the timestamp on the student input or output files are prior to the timestamp on the corresponding specific configuration file.

Ideally a roster is available in csv form to fully identify any students with the same first and last names and to link email addresses for automatic email critiques.

