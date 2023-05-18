# Reads the requested number of lines out of progress.txt file while a
# job is running.
#
# Returns : a vector of strings, the 1st line is the header information
#

readProgressUpdate <- function(jobDirectory, numIterationsToRead) {
  results <- c()
  tryCatch(
    {
      filename <- file.path(jobDirectory, "progress.txt")
      if (file.exists(filename)) {
        lines <- readLines(filename)
        if (length(lines) > 2) {
          nSubj <- gsub(")", "", unlist(strsplit(lines[1], " = "))[2], fixed = TRUE)
          nObs <- gsub(")", "", unlist(strsplit(lines[2], " = "))[2], fixed = TRUE)
          lines <- lines[3:length(lines)]
          iterLineNos <- grep("Iteration", lines)
          numIterations <- length(iterLineNos)
          numTokens <- length(lines) / numIterations - 1

          firstTime <- TRUE
          startIteration <- (numIterations - numIterationsToRead + 1)
          if (startIteration < 1) {
            startIteration <- 1
          }
          for (it in numIterations:startIteration) {
            firstLine <- (it - 1) * (numTokens + 1) + 1
            names <- list(numTokens)
            values <- list(numTokens)
            headerLine <- ""
            detailLine <- ""
            for (indx in 1:numTokens) {
              l <- lines[firstLine - 1 + indx]
              values[indx] <- gsub(")", "", unlist(strsplit(l, " = "))[2], fixed = TRUE)
              names[indx] <- unlist(strsplit(unlist(strsplit(l, " = "))[1], "(", fixed = TRUE))[2]
              if (names[indx] == "LL") {
                names[indx] <- "-2LL"
                values[indx] <- sprintf("%7.2f", as.numeric(values[indx]) * -2)
              }
              detailLine <- sprintf("%s \t%-10s", detailLine, values[[indx]])

              if (firstTime == TRUE) {
                headerLine <- sprintf("%s \t%-10s", headerLine, names[[indx]])
              }
            }
            if (firstTime == TRUE) {
              headerLine <- sprintf("%s \t%-10s \t%-10s", headerLine, "nSubj", "nObs")
              results <- c(results, headerLine)
              firstTime <- FALSE
            }
            detailLine <- sprintf("%s \t%-10s \t%-10s", detailLine, nSubj, nObs)
            results <- c(results, detailLine)
          }
        }
      } else {
        filename <- file.path(jobDirectory, "err1.txt")
        if (file.exists(filename)) {
          lines <- readLines(filename)
          iterLines <- grep("iter=", lines)
          start <- length(iterLines) - numIterationsToRead
          if (start < 1) {
            start <- 1
          }
          for (i in length(iterLines):start) {
            results <- c(results, lines[iterLines[i]])
          }
        }
      }
    },
    error = function(ex) {
    }
  )
  return(results)
}
