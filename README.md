# shinyGrader
Automatic grading of R, Pytnon, and SAS in a Shiny App (tuned to Canvas course management system)

This Shiny App (https://shiny.rstudio.com/) manages grading and regrading of homework assignments in R, Python or SAS.  Configuration files specify ancillary files, various options, and a grading rubric.  Code is run in a sandbox.  The rubric uses regular expression matching on code input and output.

ShinyGrader uses Shiny as a GUI for a platform-independent interactive program that allows grading of files submitted via Canvas.  Currently R, Python, and SAS files can be graded.
Student files are downloaded from Canvas to a local folder.  These have the following Canvas filename format: the student name (in "lastfirst" format), an underscore, optionally "late_", the student Canvas ID number, another underscore, a unique file number, another underscore, the basename of the submitted file, optionally "-#" where "#" starts at 1, and finally the extension.  Solution files should be named "solutions_0_0_foo.ext" where foo.ext is the filename you asked the students to submit their work under.
Note that the Canvas filename format allows two students with the same name to have the same text in the name field.  ShinyGrader uses a "roster" from the Canvas "Grades / Export" feature (ideally before any grades are entered).  This roster has "Student", "ID", "SIS User ID", "SIS Login ID", "Section" as its first six columns.  "Student" is in "last, first" format (different from the Canvas filename format).  "ID" is the student Canvas ID number from the Canvas filename format.   "SIS User ID" is some long complicated format, perhaps linking to the CMU SIS.  "SIS Login ID" is the student's Andrew email (including @andrew.cmu.edu).  "Root Account" is always "canvas.cmu.edu".  "Section" is of the form "36602-A".  Use of this roster is needed to assure that two students with the same name are appropriately distinguished.  Becasue Canvas grade entry does NOT show the "ID", the email is the key unique feature.  Email is also used for email feedback from shinyGrader to the students.  The section could be used in the future for section-specific features.  The roster file is named something like "12_Jan_10_25_Grades-36602-A.csv".
The approach of ShinyGrader is to have one assignment with one or more problems in any given folder.  A global configuration file named "globalGrader.config" contains the name of the folder

The resubmission of files is the cause of a great deal of complexity for grading.  The Assignment Download file contains only the most recently submitted version of each file (assuming the student resubmits a file with exactly the same name) using the "-1", etc. suffixes.  ShinyGrader maintains all prior analyzed code to allow comparisons for estimation of progress.

Each directory is intended to contain one assignment, but that assignment may represent several problems.  Each problem may involve use several files from the student, and the file that is run may be either separate submitted code files per student, each with it's own setup and rubric.

The system honors a global configuration file which specifies your common choices, directory level global configuration files, a global specific configuration file, and a per-graded-problem specific configuration file.  The global configuration file specifies the course ID and location of the roster, and the specific configurations files define the problems as well as the grading rubrics.  Analyses are considered "stale" if the timestamp on the student input or output files are prior to the timestamp on the corresponding specific configuration file.



Directions for Use:

First you must use Canvas Assignment Download to place the student files (or a zip file containing the student files) into a folder.  Then load Shiny with library(shiny) and start the program with runApp().  If needed, use the Change Folder button to navigate to the directory containing the assignment.
