
# Read progress.txt file and return values in a list
#
readProgressDotTxt <- function(fileToRead) {
  progress <- data.frame()
  if (file.exists(fileToRead) && file.info(fileToRead)$size != 0) {
    progressLines <- read.delim(fileToRead,
      row.names = NULL,
      sep = "",
      header = FALSE,
      colClasses = "character"
    )
    if (nrow(progressLines) > 2) {
      # removing nSubj nObs
      progressLines <- progressLines[-c(1:2), ]
      progressLines$Parameter <- gsub("\\(", "", progressLines$V1)
      progressLines$Value <- as.double(gsub("\\)", "", progressLines$V3))
      # removing renamed columns
      progressLines <- progressLines[, !(names(progressLines) %in% c("V1", "V2", "V3"))]
      # get rid of parts not parsed completely
      progressLines <- progressLines[progressLines$Parameter != ")", ]
      # changing parameters
      Iterations <- progressLines$Value[progressLines$Parameter == "Iteration"]
      progressLines <- progressLines[progressLines$Parameter != "Iteration", ]
      IterationsDF <- data.frame(Iter = rep(Iterations, each = nrow(progressLines) / length(Iterations)))
      progressLines$Parameter <- gsub("^LL$", "-2LL", progressLines$Parameter)
      progressLines$Value[progressLines$Parameter == "-2LL"] <- -2 * progressLines$Value[progressLines$Parameter == "-2LL"]
      progress <- cbind.data.frame(IterationsDF, progressLines)
    }
  } else {
    warning("Unable to find ", fileToRead)
  }
  return(progress)
}
