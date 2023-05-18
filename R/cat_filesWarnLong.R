cat_filesWarnLong <- function(...,
                              file = "",
                              sep = " ",
                              fill = FALSE,
                              labels = NULL,
                              append = FALSE) {
  catRes <- tryCatch(
    cat(
      ...,
      file = file,
      sep = sep,
      fill = fill,
      labels = labels,
      append = append
    ),
    error = function(e) {
      if (.Platform$OS.type == "windows") {
        if (nchar(file) > 254) {
          longErrortxt <- paste0(
            "Cannot write to the file\n",
            file,
            "\ndue to over-long path. Original error reported:\n",
            e$message
          )
          stop(longErrortxt)
        } else {
          stop(e)
        }
      } else {
        stop(e)
      }
    },
    warning = function(w) {
      if (any(grepl("cannot open file", w$message)) &&
          nchar(file) > 254 &&
          .Platform$OS.type == "windows") {
        longErrortxt <- paste0("Cannot write file\n", file,
                               "\ndue to over-long path.")
        stop(longErrortxt)
      } else {
        warning(w)
      }
    }
  )
}
