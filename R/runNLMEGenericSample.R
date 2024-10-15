runNLMEGenericSample <- function(jobIndx, SharedWorkingDir = "") {
  if (SharedWorkingDir == "") {
    SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
  }

  source(file.path(SharedWorkingDir, "myglobaldefs.r"), local = TRUE)
  extraArgsFile <- getExtraArgumentFilename(control_lines[jobIndx])
  if (dirname(extraArgsFile) == ".") {
    extraArgsFile <- file.path(SharedWorkingDir, extraArgsFile)
  }

  extraArgsFileIndx <-
    getExtraArgumentFilenameIndex(control_lines[jobIndx])

  if (!is.na(extraArgsFileIndx)) {
    lines <- readLines(extraArgsFile, warn = FALSE)
    extraArgsFile <-
      sprintf("%s.%s", extraArgsFile, extraArgsFileIndx)
    idx <- as.integer(extraArgsFileIndx)
    CommentedLines <- grepl("^\\W*#", lines)
    lines <- lines[!CommentedLines]
    numLines <- length(lines)
    # Due to these records not all being used in one run, we need a different
    # method to find num lines
    #  numLinesPerRecord=numLines/num_samples
    numLinesPerRecord <- numLines / length(grep("-rtol", lines))
    appendFlag <- FALSE
    for (i in 1:numLinesPerRecord) {
      cat_filesWarnLong(lines[(idx - 1) * numLinesPerRecord + i],
                        file = extraArgsFile,
                        sep = "\n",
                        append = appendFlag)
      appendFlag <- TRUE
    }
  }
  outputFileName <- getRunSuccessFilename(control_lines[jobIndx])
  ep <- getExePostfix(control_lines[jobIndx])

  runNLMESample(
    jobIndx,
    extraArgsFile,
    outputFileName,
    exePostfix = ep,
    SharedWorkingDir = SharedWorkingDir
  )

  if (!is.na(extraArgsFileIndx)) {
    file.remove(extraArgsFile)
  }
}
