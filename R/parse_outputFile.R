# function returns a string
.parse_outputFile <- function(outputFileName, sortValues, scenarioNames, nxtScenario) {
  overallDF <- .get_outputDF(outputFileName)
  if (nrow(overallDF) == 0) {
    returnString <- sprintf("%s%s,,,,,,,,,,", sortValues, scenarioNames[nxtScenario])
  } else {
    returnString <-
      sprintf(
        "%s%s,%d,%f,%f,%f,%f,%d,%d,%d,%f,%f",
        sortValues, scenarioNames[nxtScenario],
        overallDF$RetCode,
        overallDF$LogLik,
        overallDF$`-2LL`,
        overallDF$AIC,
        overallDF$BIC,
        overallDF$nParm,
        overallDF$nObs,
        overallDF$nSub,
        overallDF$EpsShrinkage,
        overallDF$Condition
      )
  }

  returnString
}

.get_outputDF <- function(outputFileName) {
  lines <- .get_outtxt(outputFileName)

  ReturnCode_line <- grep("^ReturnCode", lines)
  if (length(ReturnCode_line) == 0) {
    data.frame()
  } else {
    returnCode <- as.integer(gsub(
      "^ReturnCode\\s*=\\s*",
      "",
      lines[ReturnCode_line]
    ))

    Column <- "LogLikelihood"
    logLik <- as.double(gsub(
      paste0("^", Column, "\\s*=\\s*"),
      "",
      lines[grep(paste0("^", Column), lines)][1]
    ))

    Column <- "EpsShrinkage"
    shrinkage <- gsub(
      paste0("^", Column, "\\s*=\\s*"),
      "",
      lines[grep(paste0("^", Column), lines)][1]
    )

    if (shrinkage != "NA") {
      shrinkage <- as.double(shrinkage)
    } else {
      shrinkage <- NA
    }

    Column <- "NParm"
    nParam <- as.integer(gsub(
      paste0("^", Column, "\\s*=\\s*"),
      "",
      lines[grep(paste0("^", Column), lines)][1]
    ))

    Column <- "NObs"
    nObs <- as.integer(gsub(
      paste0("^", Column, "\\s*=\\s*"),
      "",
      lines[grep(paste0("^", Column), lines)][1]
    ))

    Column <- "NSub"
    nSub <- as.integer(gsub(
      paste0("^", Column, "\\s*=\\s*"),
      "",
      lines[grep(paste0("^", Column), lines)][1]
    ))

    Column <- "condition"
    if (length(grep(paste0("^", Column, "\\s*=\\s*"), lines)) == 0) {
      condition <- NA
    } else {
      condition <- as.double(gsub(
        paste0("^", Column, "\\s*=\\s*"),
        "",
        lines[grep(paste0("^", Column), lines)][1]
      ))
    }

    twoLL <- -2 * logLik
    AIC <- twoLL + nParam * 2
    BIC <- twoLL + nParam * log(nObs)
    cbind.data.frame(
      RetCode = returnCode,
      LogLik = logLik,
      `-2LL` = twoLL,
      AIC = AIC,
      BIC = BIC,
      nParm = nParam,
      nObs = nObs,
      nSub = nSub,
      EpsShrinkage = shrinkage,
      Condition = condition
    )
  }
}
