.get_IDcols <- function(cols1Text) {
  IDlineNo <- grep("^\\W*id\\W*\\(.*)", cols1Text)
  IDcol <-
    unlist(
      strsplit(
        cols1Text[IDlineNo],
        "(((^\\W*id\\W*\\(\\W*))*|([\"\']\\W*,\\W*)*)[\"\']\\W*\\)*"
      )
    )
  IDcol <- IDcol[IDcol != ""]
  IDcol
}

.rename_IDs <- function(cols1Text, DataFrame) {
  if (nrow(DataFrame) == 0) return(DataFrame)

  IDcol <- .get_IDcols(cols1Text)
  # special case of individual runs
  if (length(IDcol) == 1 && IDcol == "zzzDummyId") {
    return(DataFrame[,-c(1:5)])
  }

  if (length(IDcol) < 5) {
    DataFrame <- DataFrame[,-c(1:(5 - length(IDcol)))]
  }

  colnames(DataFrame)[1:length(IDcol)] <- IDcol
  DataFrame
}

.create_posthocStacked <- function(cols1Text, resultsDataframe) {
  resultsColNames <- colnames(resultsDataframe)
  # get IDs and sorts
  IDcols <- .get_IDcols(cols1Text)
  # we have sorts and scenario before
  IDposition <- which(IDcols[1] == resultsColNames)
  if (IDposition > 1) {
    id.vars <- resultsColNames[1:(IDposition - 1)]
  } else {
    id.vars <- c()
  }

  id.vars <- c(id.vars, IDcols)

  # get time
  timeLine <- cols1Text[grep("^\\W*time\\W*\\(\\W*", cols1Text)]
  if (length(timeLine) > 0 && "time" %in% resultsColNames) {
    id.vars <- c(id.vars, "time")
  }

  # get covariates
  covrLines <- cols1Text[grep("^\\W*covr\\W*\\(\\W*", cols1Text)]
  for (covrLine in covrLines) {
    covariatenameSplit <-
      unlist(strsplit(covrLine, "(^\\W*covr\\W*\\(\\W*)|(\\W*<-)"))
    if (length(covariatenameSplit) == 3) {
      covariate <- covariatenameSplit[2]
      if (covariate %in% resultsColNames) {
        id.vars <- c(id.vars, covariate)
      }
    }
  }

  # is 'keep' on?
  if ("TableSource" %in% resultsColNames) {
    id.vars <- c(id.vars, "TableSource")
  }

  # suppressing warnings regarding different colclasses in measure.vars
  suppressWarnings(
    posthocStacked <-
      data.table::melt(
        resultsDataframe,
        id.vars,
        variable.name = "Params",
        value.name = "Value"
      )
  )

  # move TableSource to the last position
  if ("TableSource" %in% resultsColNames) {
    data.table::setcolorder(resultsDataframe, c(resultsColNames[resultsColNames != "TableSource"], "TableSource"))
  }

  # sort by all but Scenario column
  data.table::setorderv(posthocStacked)
  posthocStacked
}

.add_SortScenario <- function(dataf, scenarioName, uIndex) {
  SortColumns <- .check_numSortColumns()
  num_sort_columns <- SortColumns$num_sort_columns
  unique_sorted_values <- SortColumns$unique_sorted_values
  sort_column_names <- SortColumns$sort_column_names

  # Add scenario column to the data table
  dataf <- cbind("Scenario" = scenarioName, dataf)
  if (num_sort_columns > 0) {
    for (SortColumnIndex in num_sort_columns:1) {
      nam <- sort_column_names[SortColumnIndex]
      val <- unlist(unique_sorted_values[[SortColumnIndex]])[uIndex]
      val <- sub("^\\s+", "", val)
      dataf <- cbind(nam = val, dataf)
      cn <- colnames(dataf)
      cn[1] <- nam
      colnames(dataf) <- cn
    }
  }

  dataf
}

collateTables <-
  function(jobList,
           scenarioNames,
           inputName,
           outputName,
           ReturnedFilesPattern) {
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <-
      getBatchDirectoryLocation(jobsDirectoryRoot)
    resultsDataframe <- NULL
    first <- TRUE
    missingFile <- TRUE

    # use win EOL if hash is used since it is called from Phoenix
    NLME_HASH <- as.integer(Sys.getenv("NLME_HASH"))
    write.csv.WinEOL <- write.csv
    if (!is.na(NLME_HASH) &&
        NLME_HASH > 0 && .Platform$OS.type != "windows") {
      formals(write.csv.WinEOL)$eol <- "\r\n"
    }

    posthocStackedRequired <-
      (inputName == "posthoc.csv") &&
      grepl(ReturnedFilesPattern, "posthocStacked.csv")

    for (job in jobList) {
      jobBaseIndx <- job %% 100
      fileToRead <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                inputName)

      if (file.exists(fileToRead) &&
          file.info(fileToRead)$size != 0) {
        dataf <- data.table::fread(fileToRead)

        if (nrow(dataf) > 0) {
          # removing repl
          dataf[, 1] <- NULL
          if (first) {
            rDumpFile <-
              sprintf("%s/jobs/%02d/%d/%s",
                      jobsBaseDirectory,
                      jobBaseIndx,
                      job,
                      "dmp.txt")
            dmp.txt <- .get_dmptxt(rDumpFile)
            cols1Text <- dmp.txt$cols1.txt
          }

          dataf <- .rename_IDs(cols1Text, dataf)

          SortScenarioDF <- .get_SortsScenarioDF(job)

          datafWithSortScenario <-
            cbind.data.frame(SortScenarioDF, dataf, row.names = NULL)
          if (inputName == "posthoc.csv") {
            datafStacked <- .create_posthocStacked(cols1Text, dataf)
            StackedWithSortScenario <-
              cbind.data.frame(SortScenarioDF, datafStacked, row.names = NULL)
          }

          if (first == TRUE) {
            resultsDataframe <- datafWithSortScenario
            if (posthocStackedRequired) {
              posthocStacked <- StackedWithSortScenario
            }
          } else {
            resultsDataframe <- rbind(resultsDataframe, datafWithSortScenario)
            if (posthocStackedRequired) {
              posthocStacked <- rbind(posthocStacked, StackedWithSortScenario)
            }
          }

          first <- FALSE
          missingFile <- FALSE
        }
      }
    }

    if (missingFile == FALSE) {
      write.csv.WinEOL(
        resultsDataframe,
        file = outputName,
        row.names = FALSE,
        quote = FALSE,
        na = ""
      )

      if (posthocStackedRequired) {
        write.csv.WinEOL(
          posthocStacked,
          file = file.path(dirname(outputName), "posthocStacked.csv"),
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }
    }
  }
