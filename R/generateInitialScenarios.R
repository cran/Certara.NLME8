# This function Generates a control file to run initial scenarios for
# stepwise covariate search
#
# Returns : Name of the output files to generate
#
generateInitialScenarios <- function(controlFilename, nlmeArgsFile, modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray, nxtScenario, localDir) {
  outputFilenames <- c()
  indexes <- c()
  scenarios <- get("scenarios", envir = nlmeEnv)
  numTodo <- as.integer(numCovariates)
  submodels <- c()
  for (i in 1:numTodo) {
    submodels <- c(submodels, FALSE)
  }
  #
  # Generate all possible results for 0-N covariates
  #
  nxtScenario <- 0
  appendFlag <- FALSE
  lines <- c()

  cat_filesWarnLong(modelFilename, file = controlFilename, sep = "\n")
  for (ifn in inputFileArray) {
    cat(ifn, file = controlFilename, sep = " ", append = TRUE)
  }
  cat("", file = controlFilename, sep = "\n", append = TRUE)
  cat("*.csv *.txt *.log *.LOG", file = controlFilename, sep = "\n", append = TRUE)

  cat((numTodo + 1), file = controlFilename, sep = "\n", append = TRUE)

  indx <- 1
  for (i in 0:numTodo) {
    if (i > 0) {
      submodels[i] <- TRUE
    }
    ret <- generateSelCovarSearchArgsLine(controlFilename, nlmeArgsFile, modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray, submodels, nxtScenario, appendFlag, indx, localDir)
    if (length(scenarios[[ret$key]]) != 0) {
      if (scenarios[[ret$key]]$status == "Completed") {
        return(scenarios[[ret$key]])
      }
    }
    line <- ret$line
    outputFilename <- ret$outputFilename
    outputFilenames <- c(outputFilenames, outputFilename)
    indexes <- c(indexes, ret$index)
    scenarios[[ret$key]] <- ret
    if (i > 0) {
      submodels[i] <- FALSE
    }
    appendFlag <- TRUE
    lines <- c(lines, line)
    nxtScenario <- nxtScenario + 1
    cat(line, file = controlFilename, sep = "\n", append = TRUE)
    indx <- indx + 1
  }
  assign("nxtScenario", nxtScenario, envir = nlmeEnv)
  assign("scenarios", scenarios, envir = nlmeEnv)
  return(indexes)
}
