write_function_to_file <-
  function(FunctionName, HeadLine, TargetFile) {
    cat(HeadLine,
        file = TargetFile,
        sep = "\n",
        append = TRUE)
    lines <- deparse(eval(FunctionName))
    lineFunctionBegins <-
      grep("{", lines[1:length(lines)], fixed = TRUE)[1]
    if (length(lineFunctionBegins) == 0) {
      stop(paste0(
        "Function ",
        FunctionName,
        "couldn't be saved in file myglobaldefs.R"
      ))
    }

    lines[lineFunctionBegins] <-
      substr(lines[lineFunctionBegins], regexpr("{", lines[lineFunctionBegins], fixed = TRUE), nchar(lines[lineFunctionBegins]))

    lineFunctionEnds <-
      grep(", original", lines[1:length(lines)], fixed = TRUE)[1]
    if (is.na(lineFunctionEnds)) {
      lineFunctionEnds <- length(lines)
    } else {
      # debug mode saves additional data we don't need to source
      lines[lineFunctionEnds] <- "}"
    }

    cat(lines[lineFunctionBegins:lineFunctionEnds],
        file = TargetFile,
        sep = "\n",
        append = TRUE)
  }
