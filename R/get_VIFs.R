# getting VIFs produced for NaivePooled engine results
# from the out file (they are not presented anywhere else)
.get_VIFs <- function(outfile) {
  if (!file.exists(outfile)) {
    return(NULL)
  }

  outText <- readLines(outfile, warn = FALSE)
  varFixefInfStart <- grep("^varFixefInf$", outText)
  if (length(varFixefInfStart) == 0) {
    return(NULL)
  }

  if (length(varFixefInfStart) > 1) {
    warning(
      "More than one varFixefInf record was found in \n",
      outfile,
      "\nOnly the first occurence will be used."
    )
    varFixefInfStart <- varFixefInfStart[1]
  }

  # fixefs are starting at the next row
  varFixefInfStart <- varFixefInfStart + 1
  # from VarFixefInfStart to the end of the file
  outTextStart <- outText[varFixefInfStart:length(outText)]
  # entities are separated with empty rows
  varFixefInfLength <- match("", outTextStart) - 1
  if (is.na(varFixefInfLength) || varFixefInfLength < 1) {
    warning(
      "Current ouptut file was not parsed correctly \n",
      outfile,
      "\nUnable to read VIFs."
    )
    return(NULL)
  }

  varFixefList <- strsplit(outTextStart[1:varFixefInfLength], "#")
  VIFs <- sapply(
    varFixefList,
    function(Row) {
      if (length(Row) < 2) {
        fixef <- NA
      } else {
        fixef <- as.numeric(Row[1])
        names(fixef) <- trimws(Row[2])
      }
      fixef
    }
  )

  VIFs <- na.omit(VIFs)

  VIFs
}

# get VIF from VIFObs for individual mode
.get_VIFobs <- function(outFileName, resids) {
  VIFObsFile <- file.path(dirname(outFileName), "VIFObs.csv")
  if (file.exists(VIFObsFile)) {
    VIFObs <- read.csv(VIFObsFile)
    VIFObs <- Filter(function(x) !any(is.na(x)), VIFObs)
    # there could be no VIFs
    if ("VIF" %in% colnames(VIFObs)) {
      resids <- cbind.data.frame(resids, VIF = VIFObs$VIF)
    }
  }

  resids
}
