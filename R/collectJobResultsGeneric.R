.check_numSortColumns <- function() {
  if (exists("num_sort_columns", envir = nlmeEnv)) {
    num_sort_columns <-
      as.numeric(get("num_sort_columns", envir = nlmeEnv))
    if (num_sort_columns == 0) {
      unique_sorted_values <- NULL
      sort_column_names <- c()
    } else {
      nlmeEnvIsBroken <- FALSE
      if (!exists("sort_column_names", envir = nlmeEnv)) {
        warning(
          "NlmeEnv is broken, num_sort_columns = ",
          num_sort_columns,
          "but sort_column_names does not exist. Treating as no sort."
        )
        nlmeEnvIsBroken <- TRUE
      } else {
        sort_column_names <- get("sort_column_names", envir = nlmeEnv)
      }

      if (!exists("unique_sorted_values", envir = nlmeEnv)) {
        warning(
          "NlmeEnv is broken, num_sort_columns = ",
          num_sort_columns,
          "but unique_sorted_values does not exist. Treating as no sort."
        )
        nlmeEnvIsBroken <- TRUE
      } else {
        unique_sorted_values <- get("unique_sorted_values", envir = nlmeEnv)
      }

      if (nlmeEnvIsBroken) {
        num_sort_columns <- 0
        unique_sorted_values <- NULL
        sort_column_names <- c()
      }
    }
  } else {
    num_sort_columns <- 0
    unique_sorted_values <- NULL
    sort_column_names <- c()
  }

  assign("num_sort_columns", num_sort_columns, envir = nlmeEnv)
  assign("unique_sorted_values", unique_sorted_values, envir = nlmeEnv)
  assign("sort_column_names", sort_column_names, envir = nlmeEnv)
  sotrCols <- list(
    num_sort_columns = num_sort_columns,
    unique_sorted_values = unique_sorted_values,
    sort_column_names = sort_column_names
  )
  sotrCols
}

# copy/concatenate/collate NLME results file from all runs into one
collectJobResultsGeneric <-
  function(done, copyFilesFlag = FALSE, control_lines) {
    SharedWorkingDir <- get("SharedWorkingDir", envir = nlmeEnv)
    baseDirectory <- getBatchDirectoryLocation(SharedWorkingDir)
    hangAroundABitForTheStatusFile(SharedWorkingDir, done)

    # Files to return from shared directory
    # (logs)
    files_to_return <-
      list.files(
        path = SharedWorkingDir,
        pattern = paste0("(",
          unlist(strsplit(
            get("files_to_return", envir = nlmeEnv), " "
          )),
          "$)",
          collapse = "|"
        ),
        ignore.case = TRUE,
        all.files = TRUE
      )

    copy_filesWarnLong(
      file.path(SharedWorkingDir, files_to_return),
      dirname(SharedWorkingDir),
      overwrite = TRUE
    )

    if (!copyFilesFlag) return(TRUE)
    # Return files from individual runs.
    append <- FALSE

    for (job in done) {
      # Files that get copied back
      files <-
        unlist(strsplit(getOutputFilenames(control_lines[job]), split = " +"))

      baseIndx <- job %% 100
      wd <-
        file.path(baseDirectory, "jobs", sprintf("%02d", baseIndx), job)

      filesFrom <-
        list.files(
          path = wd,
          pattern = paste0("(",
            files,
            "$)",
            collapse = "|"
          ),
          ignore.case = TRUE,
          all.files = TRUE,
          full.names = TRUE
        )

      # removing input data and err2.txt
      errorFiles <- filesFrom[grepl("err2.txt", basename(filesFrom))]
      filesFrom <- filesFrom[!grepl("((cols)|(data)[1-4]\\.txt)|err2\\.txt", basename(filesFrom))]

      # adding only non-empty error files
      nonEmptyErrorFiles <- errorFiles[file.info(errorFiles)$size > 0]
      filesFrom <- c(filesFrom, nonEmptyErrorFiles)

      Scenario <- .get_scenarioName(job)
      Sorts <- .get_Sorts(job)

      if (Sorts != "" || Scenario != "WorkFlow") {
        headerRow <- paste0("### ")
        if (Sorts != "") {
          headerRow <- paste(headerRow, Sorts)
        }

        if (Scenario != "WorkFlow") {
          headerRow <- paste(headerRow, "Scenario =", Scenario)
        }

        filesTo <- file.path(dirname(SharedWorkingDir), basename(filesFrom))
        filesTo <- gsub(getRunSuccessFilename(control_lines[job]),
                        "out.txt",
                        filesTo,
                        fixed = TRUE)

        # do not need to add a header for csv
        filesToWOCSV <- filesTo[tools::file_ext(filesTo) != "csv"]
        sapply(filesToWOCSV,
               function(fileTo, headerRow) {
                 cat(headerRow, file = fileTo, append = append, sep = "\n")
               },
               headerRow)
        append = TRUE

        file.append(filesTo, filesFrom)
      } else {
        copy_filesWarnLong(filesFrom,
                           dirname(SharedWorkingDir),
                           overwrite = TRUE
        )
      }
    }

    if (.Platform$OS.type == "unix" && Sys.getenv("NLME_HASH") != "") {
      # need to change EOL to Windows format if started from Phoenix
      for (fileName in filesTo) {
        txt <- readLines(fileName)
        con <- file(fileName, open="wb")
        writeLines(txt, con, sep="\r\n")
        close(con)
      }

    }
  }

.get_scenarioName <- function(job) {
  scenarioNames <- unique(getScenarioName(get("control_lines", envir = nlmeEnv)))
  if (all(scenarioNames == "WorkFlow")) {
    scenario <- "WorkFlow"
  } else {
    scenario <- scenarioNames[(job - 1) %% length(scenarioNames) + 1]
  }

  scenario
}

.get_Sorts <- function(job, sep = " = ", collapse = " ") {
  scenarioNames <- unique(getScenarioName(get("control_lines", envir = nlmeEnv)))
  SortColumns <- .check_numSortColumns()
  num_sort_columns <- SortColumns$num_sort_columns
  unique_sorted_values <- SortColumns$unique_sorted_values
  sort_column_names <- SortColumns$sort_column_names
  if (num_sort_columns > 0) {
    scenariosIteration <- (job - 1) %/% length(scenarioNames) + 1
    Sorts <-
      paste(
        sort_column_names,
        unique_sorted_values[scenariosIteration, ],
        sep = sep,
        collapse = collapse
      )
  } else {
    Sorts <- ""
  }

  Sorts
}

.get_SortsScenarioDF <- function(job) {
  scenarioNames <- unique(getScenarioName(get("control_lines", envir = nlmeEnv)))
  SortColumns <- .check_numSortColumns()
  num_sort_columns <- SortColumns$num_sort_columns
  unique_sorted_values <- SortColumns$unique_sorted_values
  sort_column_names <- SortColumns$sort_column_names
  scenario <- .get_scenarioName(job)
  DF <- data.frame(Scenario = scenario)
  if (num_sort_columns > 0) {
    scenariosIteration <- (job - 1) %/% length(scenarioNames) + 1
    sortsDF <- unique_sorted_values[scenariosIteration, , drop = FALSE]
    colnames(sortsDF) <- sort_column_names
    DF <- cbind.data.frame(sortsDF, DF)
  }

  DF
}
