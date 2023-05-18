.get_bluptable <- function(bluptableFile, etaNames, currentEta.csv) {
  bluptable <- data.table::data.table()
  bluptableInfo <- file.info(bluptableFile)$size[1]
  if (is.na(bluptableInfo) ||
      file.info(bluptableFile)$size[1] < 600) {
    return(bluptable)
  }

  try({
    bluptable <-
      read.table(
        bluptableFile,
        skip = 3,
        fill = TRUE,
        comment.char = ""
      )
    bluptableNames <- bluptable[1,]
    bluptableNames[1] <- "ID"
    bluptable <- bluptable[-c(1),]
    colnames(bluptable) <- bluptableNames
    startOfDeletedSection <- which(bluptable[, 2] == "!eta_mean")
    EndOfDeletedSection <-
      c(which(bluptable[, 2] == "ETA_SEQ"), nrow(bluptable))
    rowsToDelete <-
      unlist(sapply(seq_along(startOfDeletedSection), function(i,
                                                               startOfDeletedSection,
                                                               EndOfDeletedSection) {
        startOfDeletedSection[i]:EndOfDeletedSection[i]
      }, startOfDeletedSection, EndOfDeletedSection))

    # deleting informational rows
    bluptable <- bluptable[-rowsToDelete,]
    # removing ID
    bluptable$ID <- NULL
    Eta <- etaNames[as.integer(bluptable$ETA_SEQ)]
    bluptable$ETA_SEQ <- NULL
    IDsDF <- currentEta.csv[, 1:5]
    bluptable <- cbind.data.frame(IDsDF, Eta, bluptable, row.names = NULL)
  }, silent = TRUE)

  bluptable
}
