#
# Generate a new control file from original by taking dataset and
# splitting it by unique column keys
#
#
sortByColumnAndGenerateControlFile <-
  function(inputFilename,
           numColumns,
           columnNamesArray,
           argsFile,
           controlFilename,
           jobType) {
    numRuns <- 0

    localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)

    # NLME combined arugments file
    combinedArgsFilename <- sprintf("%s_combined_args.txt",
                                    strsplit(basename(controlFilename), "[.]")[[1]][1])

    fullPathCombinedArgsFilename <-
      file.path(localWorkingDir, combinedArgsFilename)
    # Split the data set into multiple ones
    data <-
      data.table::fread(
        file.path(localWorkingDir, inputFilename),
        check.names = FALSE,
        fill = TRUE
      )

    cn <- colnames(data)
    cn[1] <- gsub("^##", "", cn[1], fixed = FALSE)
    colnames(data) <- cn

    fName2 <- "data2.txt"
    if (file.exists(file.path(localWorkingDir, fName2))) {
      data2 <-
        read.csv(file.path(localWorkingDir, fName2), check.names = FALSE)
      cn <- colnames(data2)
      cn[1] <- gsub("^##", "", cn[1], fixed = FALSE)
      colnames(data2) <- cn
    } else {
      data2 <- NULL
    }

    if (dirname(argsFile) == ".") {
      argsFile <- file.path(localWorkingDir, argsFile)
    }

    argsFileLines <- readLines(argsFile)

    if (numColumns == 0) {
      # No sort columns
      num_sort_columns <- numColumns
      sort_column_names <- ""
      unique_sorted_values <- NULL
      outTables <- list()
      outTables[[1]] <- data
      if (length(data2) != 0) {
        outTables2 <- list()
        outTables2[[1]] <- data2
      }

      numSortDatasets <- 1
    } else {
      spaceSplitColNames <- strsplit(columnNamesArray, split = " ")[[1]]
      if (all(grepl("^[A-Za-z0-9]+$", spaceSplitColNames))) {
        colNames <- spaceSplitColNames
      } else {
        tryCatch({
          colNames <- eval(parse(text = columnNamesArray))
        },
        error = function(cond) {
          stop(
            "Cannot recognize the sort columns given: ",
            columnNamesArray,
            "\nThe error produced by evaluation: ",
            cond
          )
        })
      }

      if (any(!colNames %in% colnames(data))) {
        stop("Some column(s) used for sorting are not found in the dataset: ",
             paste(colNames[which(!colNames %in% colnames(data))]))
      }

      sorted <-
        data[do.call(base::order, as.list(data[, colNames, drop = FALSE])), , drop = FALSE]
      if (length(data2) != 0) {
        sorted2 <-
          data2[do.call(base::order, as.list(data2[, colNames, drop = FALSE])), , drop = FALSE]
      }

      tmpTable <- split(sorted, f = sorted[, colNames], drop = TRUE)
      outTables <-
        tmpTable[order(unlist(names(tmpTable)), decreasing = FALSE)]
      keyNames <- names(outTables)

      num_sort_columns <- numColumns
      sort_column_names <- colNames
      unique_sorted_values <-
        data.frame(matrix(ncol = length(colNames), nrow = length(keyNames)))
      colnames(unique_sorted_values) <- colNames

      for (r in 1:length(keyNames)) {
        row <- c()
        key <- keyNames[r]
        tokens <- strsplit(key, ".", fixed = TRUE)
        for (t in unlist(tokens)) {
          row <- c(row, t)
        }
        unique_sorted_values[r, ] <- row
      }

      if (length(data2) != 0) {
        tmpTable <- split(sorted2, f = sorted2[, colNames], drop = TRUE)
        outTables2 <-
          tmpTable[order(unlist(names(tmpTable)), decreasing = FALSE)]
      }

      numSortDatasets <- length(outTables)
    }

    assign("num_sort_columns", num_sort_columns, envir = nlmeEnv)
    assign("sort_column_names", sort_column_names, envir = nlmeEnv)
    assign("unique_sorted_values", unique_sorted_values, envir = nlmeEnv)

    listOfProfileModels <- generateProfileModels(jobType)
    numProfileVariables <- length(listOfProfileModels)
    # How many scenarios do we have in the arguments file
    # We now know how many runs to create
    numScenarios <- as.integer(argsFileLines[4])
    numSimulationRuns <-
      numScenarios * numSortDatasets * numProfileVariables
    # 1st line
    cat_filesWarnLong(argsFileLines[1],
                      file = controlFilename,
                      sep = "\n",
                      append = FALSE)
    # second line needs to have all the new data files added to it
    cat(argsFileLines[2],
        file = controlFilename,
        sep = " ",
        append = TRUE)
    cat(" ",
        file = controlFilename,
        sep = " ",
        append = TRUE)
    cat(
      combinedArgsFilename,
      file = controlFilename,
      sep = " ",
      append = TRUE
    )
    tokens <- unlist(strsplit(argsFileLines[5], ","))
    if (numSortDatasets != 1) {
      for (indx in 1:numSortDatasets) {
        inputFile <- sprintf(" %s.%d ", inputFilename, indx)
        cat(inputFile,
            file = controlFilename,
            sep = " ",
            append = TRUE)
        if (length(data2) != 0) {
          cat(
            sprintf(" %s.%d ", fName2, indx),
            file = controlFilename,
            sep = " ",
            append = TRUE
          )
        }
      }
    }
    if (numProfileVariables > 0) {
      cat(" ",
          file = controlFilename,
          sep = " ",
          append = TRUE)
      for (indx in 1:numProfileVariables) {
        cat(
          paste0(listOfProfileModels[[indx]]$modelName, " "),
          file = controlFilename,
          sep = " ",
          append = TRUE
        )
      }
    }
    cat("",
        file = controlFilename,
        sep = "\n",
        append = TRUE)
    # 3rd line stays the same
    cat(argsFileLines[3],
        file = controlFilename,
        sep = "\n",
        append = TRUE)
    # 4th line is the number of runs
    cat(
      sprintf("%d", numSimulationRuns),
      file = controlFilename,
      sep = "\n",
      append = TRUE
    )

    numRuns <- numSortDatasets
    overwrite <- FALSE
    nxtSeq <- 1
    outfileSeq <- 1
    flag <- FALSE
    posthocTables <-
      getTableNames(paste0(localWorkingDir, "/cols1.txt"))
    for (indx in 1:numSortDatasets) {
      for (scenario in 1:numScenarios) {
        for (prof in 1:numProfileVariables) {
          originalLine <- argsFileLines[4 + scenario]
          tokens <- unlist(strsplit(originalLine, ","))
          scenarioName <- tokens[1]
          argsFileName <- tokens[2]
          returnedFilesPattern <- tokens[3]
          outBaseName <- tokens[4]
          outputFilename <-
            sprintf("%s.%d", outBaseName, outfileSeq)
          generalFilesToRetrieve <- tokens[5]
          jobFilesToRetrieve <- tokens[6]
          # Write out a record in new nlmearguments file
          nlmearguments <-
            readNlmeArgsFile(tokens[2], localWorkingDir)
          for (i in 1:(length(nlmearguments) - 1)) {
            # Lets make sure that we replace data2.txt with data2.txt.i , etc
            argsLine <- nlmearguments[i]
            if (length(data2) > 0 && numSortDatasets > 1) {
              argsLine <- gsub("data2\\.txt", sprintf("data2.txt.%d ", indx), argsLine)
            }

            cat_filesWarnLong(argsLine,
                              file = fullPathCombinedArgsFilename,
                              sep = "\n",
                              append = flag)
            flag <- TRUE
          }

          #  Change the original file line into  new specs
          if (listOfProfileModels[[prof]]$exePostfix == "") {
            postFix <- ""
          } else {
            postFix <- paste(",", listOfProfileModels[[prof]]$exePostfix)
          }

          generalFilesToRetrieve <-
            gsub(outBaseName,
                 outputFilename,
                 generalFilesToRetrieve,
                 fixed = TRUE)

          line <- sprintf(
            "%s,%s:%d,%s,%s,%s,%s %s %s %s",
            scenarioName,
            combinedArgsFilename,
            outfileSeq,
            returnedFilesPattern,
            outputFilename,
            generalFilesToRetrieve,
            jobFilesToRetrieve,
            posthocTables,
            outputFilename,
            postFix
          )
          cat(line,
              file = controlFilename,
              sep = "\n",
              append = TRUE)
          if (numSortDatasets > 1) {
            filename <-
              sprintf("%s/%s.%d", localWorkingDir, inputFilename, indx)
            if (length(data2) != 0) {
              filename2 <- sprintf("%s/%s.%d", localWorkingDir, fName2, indx)
            }

            names <- colnames(outTables[[indx]])
            if (length(grep("##", names[1])) == 0) {
              names[1] <- sprintf("##%s", names[1])
            }

            colnames(outTables[[indx]]) <- names
            write.csv(
              outTables[[indx]],
              file = filename,
              row.names = FALSE,
              quote = FALSE
            )

            if (length(data2) != 0) {
              names <- colnames(outTables2[[indx]])
              if (length(grep("##", names[1])) == 0) {
                names[1] <- sprintf("##%s", names[1])
              }

              colnames(outTables2[[indx]]) <- names
              write.csv(
                outTables2[[indx]],
                file = filename2,
                row.names = FALSE,
                quote = FALSE
              )
            }
          } else {
            filename <-
              sprintf(file.path(localWorkingDir, inputFilename))
          }

          overwrite <- TRUE

          cat(
            paste(
              " -d1",
              "cols1.txt",
              basename(filename),
              "-out_file",
              outputFilename
            ),
            file = fullPathCombinedArgsFilename,
            sep = "\n",
            append = overwrite
          )
          outfileSeq <- outfileSeq + 1
        }
        nxtSeq <- nxtSeq + 1
      }
    }
    return(numSimulationRuns)
  }
