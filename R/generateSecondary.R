generateSecondary <- function(outputFilenames, unique_sorted_values, scenarioNames, localWorkingDir) {
  num_sort_columns <- get("num_sort_columns", envir = nlmeEnv)
  sort_column_names <- get("sort_column_names", envir = nlmeEnv)
  jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
  jobsBaseDirectory <- getBatchDirectoryLocation(jobsDirectoryRoot)

  secondaryTable <- c()
  sc <- 1
  uIndex <- 1
  filesAreMissing <- TRUE
  GlobalSummaryLine2Reported <- FALSE
  for (fileToRead in outputFilenames) {
    if (file.exists(fileToRead) && file.info(fileToRead)$size != 0) {
      filesAreMissing <- FALSE
      lines <- readLines(fileToRead)
      secondaryIndex <- grep("secondary", lines)
      if (length(secondaryIndex) == 1) {
        # if there's something to prepare in secondary - report it
        if (!GlobalSummaryLine2Reported) {
          GlobalSummaryLine2 <- sprintf("Generating Secondary.csv")
          assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
          UpdateProgressMessages()
          GlobalSummaryLine2Reported <- TRUE
        }

        pos <- grep("NParm", lines)
        nparam <- unlist(strsplit(lines[pos], split = "="))[2]
        pos <- grep("NObs", lines)
        nobs <- unlist(strsplit(lines[pos], split = "="))[2]
        degOfFreedom <- as.numeric(nobs) - as.numeric(nparam)
        if (degOfFreedom < 1) {
          depOfFreedom <- 1
        }
        secondaryStderrIndex <- grep("stderrSecondary", lines)
        omega5Index <- grep("omega5", lines)
        if (length(secondaryStderrIndex) == 0) {
          secondaryStderrIndex <- omega5Index[1]
        }
        # parse out secondary names/values and stderr
        numSecondaryVars <- secondaryStderrIndex - secondaryIndex - 2
        fileToRead <- file.path(localWorkingDir, "secondaryVariableNames.txt")
        units <- c()
        if (file.exists(fileToRead) && file.info(fileToRead)$size != 0) {
          names <- readLines(fileToRead)
          for (n in names) {
            u <- unlist(strsplit(n, split = " "))[2]
            if (is.na(u)) {
              u <- ""
            }
            units <- c(units, u)
          }
        }
        for (l in 1:numSecondaryVars) {
          secondaryVarLine <- lines[secondaryIndex + l]
          secondaryVarStderrLine <- lines[secondaryStderrIndex + l]
          vals <- unlist(strsplit(secondaryVarLine, split = "\t"))
          secondaryVal <- as.double(vals[2])
          secondaryName <- sub("#\\s+", "", vals[3])
          if (secondaryStderrIndex == omega5Index[1]) {
            secondaryStderr <- ""
          } else {
            secondaryStderr <- as.double(unlist(strsplit(secondaryVarStderrLine, split = "\t"))[2])
          }
          if (l <= length(units)) {
            unit <- units[l]
          } else {
            unit <- ""
          }
          rowcol <- c()
          # Add all the sort columns to the row
          if (num_sort_columns > 0) {
            for (c in 1:num_sort_columns) {
              nam <- sort_column_names[c]
              val <- unlist(unique_sorted_values[[c]])[uIndex]
              if (is.na(val)) {
                val <- unlist(unique_sorted_values[[c]])[uIndex + 1]
              }
              val <- sub("^\\s+", "", val)
              rowcol[eval(nam)] <- val
            }
          }
          # Add the empty scenario
          rowcol["Scenario"] <- scenarioNames[sc]
          rowcol["Secondary"] <- secondaryName
          rowcol["Estimate"] <- secondaryVal
          rowcol["Units"] <- unit
          rowcol["Stderr"] <- secondaryStderr
          confidenceLevel <- 95
          frac <- (100 - confidenceLevel) / 200
          xcilow <- qt(frac, degOfFreedom)

          if (secondaryStderr == "") {
            low <- ""
            high <- ""
            cv <- ""
          } else {
            low <- secondaryVal + xcilow * as.double(secondaryStderr)
            high <- secondaryVal - xcilow * as.double(secondaryStderr)
            cv <- (100 * as.double(secondaryStderr)) / secondaryVal
          }
          rowcol["CV%"] <- cv
          rowcol["2.5%CI"] <- low
          rowcol["97.5%CI"] <- high
          rowcol["Var.Inf.factor"] <- ""
          # Add the row to secondary table
          secondaryTable <- rbind(secondaryTable, rowcol)
        }
      }
    }
    sc <- sc + 1
    if (sc > length(scenarioNames)) {
      sc <- 1
      uIndex <- uIndex + 1
    }
  }
  if (filesAreMissing == FALSE) {
    write.csv(secondaryTable,
      file = file.path(localWorkingDir, "Secondary.csv"),
      row.names = FALSE, quote = FALSE
    )
  }
}
