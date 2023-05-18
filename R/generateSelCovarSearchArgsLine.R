# function to transform binary string to hex string to short the names of the out files
.convertStrBinToStrHex <- function(binString) {
  binvec <- as.logical(as.integer(unlist(strsplit(as.character(binString), split = ""))))
  # better to treat somehow NAs
  binvec[is.na(binvec)] <- FALSE
  length.of.binvec <- length(binvec)
  addpositions <- 4 - length.of.binvec %% 4
  positionsby4 <- (length.of.binvec + addpositions) / 4
  hexvec <- c(0:9, "a", "b", "c", "d", "e", "f")
  hexcodelist <- list(
    `0` = numeric(4),
    `1` = c(0, 0, 0, 1),
    `2` = c(0, 0, 1, 0),
    `3` = c(0, 0, 1, 1),
    `4` = c(0, 1, 0, 0),
    `5` = c(0, 1, 0, 1),
    `6` = c(0, 1, 1, 0),
    `7` = c(0, 1, 1, 1),
    `8` = c(1, 0, 0, 0),
    `9` = c(1, 0, 0, 1),
    a = c(1, 0, 1, 0),
    b = c(1, 0, 1, 1),
    c = c(1, 1, 0, 0),
    d = c(1, 1, 0, 1),
    e = c(1, 1, 1, 0),
    f = c(1, 1, 1, 1)
  )

  incl <- c(numeric(addpositions), binvec)
  dim(incl) <- c(4, positionsby4)
  paste(hexvec[crossprod(incl, 2L^(3:0)) + 1], collapse = "")
}

#  Generate a line for the selected list of scenarios
#
# Returns : A list structure for this scenario
#
#    Scenario data structure
#    line                : Line that will be used with NLME args file
#    key                 : 0000000  key for this scenario
#    outputFilename      : Name of the output file
#    scenarioDescription :
#    scenarioName        :
#    status              :
#    logLikelihood       :
#
generateSelCovarSearchArgsLine <- function(controlFilename, nlmeArgsFile, modelFilename, nlmeArgsFilename, inputFileArray, numCovariates, covarNamesArray, submodels, nxtScenario, appendFlag, argsIndex, localDir) {
  if (dirname(nlmeArgsFilename) == ".") {
    nlmeArgLines <- readLines(paste0(localDir, "/", nlmeArgsFilename))
  } else {
    nlmeArgLines <- readLines(nlmeArgsFilename)
  }
  numTodo <- as.integer(numCovariates)
  scenarioBase <- "cstep"
  outputBase <- "out"
  scenarios <- get("scenarios", envir = nlmeEnv)
  {
    nxtScenarioName <- sprintf("%s%03d", scenarioBase, nxtScenario)
    suffix <- ""
    nxtScenarioDescription <- ""
    for (i in 1:numTodo) {
      if (submodels[i] == TRUE) {
        suffix <- paste0(suffix, "1")
      } else {
        suffix <- paste0(suffix, "0")
      }
    }
    if (length(scenarios[[suffix]]) != 0) {
      if (scenarios[[suffix]]$status == "Completed") {
        return(scenarios[[suffix]])
      }
    }
    argFlag <- ""
    na <- unlist(strsplit(covarNamesArray, split = " "))

    for (i in 1:numTodo) {
      if (submodels[i] == TRUE) {
        nxtScenarioDescription <- paste0(nxtScenarioDescription, " ", na[i])
        argFlag <- paste0(argFlag, "_", (i - 1))
      }
    }
    argFlag <- paste0(argFlag, "_")
    outputFile <- paste0(outputBase, .convertStrBinToStrHex(suffix), ".txt")
    for (i in 1:numTodo) {
      submodels[i] <- !submodels[i]
      if (submodels[i]) {
        break
      }
    }
    line <- paste(
      nxtScenarioName,
      nxtScenarioDescription,
      sprintf(",%s:%d,", nlmeArgsFile, argsIndex),
      sprintf(",%s,%s,progress.txt", outputFile, outputFile)
    )

    cat_filesWarnLong(sprintf("/xe %s", argFlag), file = nlmeArgsFile, sep = "\n", append = appendFlag)
    appendFlag <- TRUE
    for (l in nlmeArgLines) {
      cat(l, file = nlmeArgsFile, sep = "\n", append = appendFlag)
    }

    cat(paste(" /out_file", outputFile),
      file = nlmeArgsFile, sep = "\n", append = appendFlag
    )
  }
  return(list(
    line = line,
    index = nxtScenario + 1,
    key = suffix,
    outputFilename = outputFile,
    scenarioDescription = nxtScenarioDescription,
    scenarioName = nxtScenarioName,
    status = "Initialized",
    returnCode = 0,
    logLike = 0.0,
    logLikelihood = 0.0,
    shrinkage = 0.0,
    nParam = 0,
    nObs = 0,
    nSub = 0,
    AIC = 0.0,
    BIC = 0.0
  ))
}
