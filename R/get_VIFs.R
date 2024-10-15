# parse any structure of consequent rows in the out file
.parse_OutFileStructure <-
  function(outfile, ToCapture = "varFixefInf") {
    if (!file.exists(outfile)) {
      return("")
    }

  outText <- .get_outtxt(outfile)

  StructureStart <- grep(paste0("^", ToCapture, "$"), outText)

  if (length(StructureStart) == 0) {
    return("")
  }

  if (length(StructureStart) > 1) {
    warning(
      paste0("More than one ", ToCapture,
             " record was found in \n",
      outfile,
      "\nOnly the first occurence will be used."
      )
    )
    StructureStart <- StructureStart[1]
  }

  # Structure is starting at the next row
  StructureStart <- StructureStart + 1
  # from StructureStart to the end of the file
  outTextStart <- outText[StructureStart:length(outText)]
  # entities are separated with empty rows
  StructureLength <- match("", outTextStart) - 1
  if (is.na(StructureLength) || StructureLength < 1) {
    warning(
      "Current ouptut file was not parsed correctly \n",
      outfile,
      "\nUnable to read Structures."
    )
    return("")
  }

  StructureList <- strsplit(outTextStart[1:StructureLength], "#")
  suppressWarnings(Structures <- sapply(StructureList,
                                        function(Row)
                                          as.numeric(Row[[1]])))

  StructuresNamesUnits <- sapply(
    StructureList,
    function(Row) trimws(Row[[2]])
  )

  # could be units inside
  StructuresNamesSplit <- strsplit(StructuresNamesUnits, " ")
  StructuresNames <- sapply(StructuresNamesSplit, function(x) x[[1]])
  StructuresUnits <- sapply(StructuresNamesSplit, function(x) {ifelse(length(x) > 1, paste(x[2:length(x)]), "")})
  names(Structures) <- StructuresNames
  attr(Structures, "units") <- StructuresUnits
  Structures
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
