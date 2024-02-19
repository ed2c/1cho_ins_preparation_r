

#' Close All View Tabs in RStudio
#'
#' This function closes all view tabs in RStudio. It starts from the current document and moves to the next tab until it reaches the starting document again.
#'
#' Note that this function will prompt you to save any unsaved files. If you choose not to save, these files will be closed without saving.
#'
#' @note This function requires the `rstudioapi` package, which is not automatically included in R. You need to install it manually if you haven't done so.
#'
#' @return None
#'
#' @export
close_view <- function() {
  Tabs <- c()

  doc <- rstudioapi::documentPath()

  while (is.null(doc)||!doc%in% Tabs) {
    if (is.null(doc)) {
      rstudioapi::executeCommand('closeSourceDoc')
    }
    rstudioapi::executeCommand('nextTab')

    Tabs <- c(Tabs, doc);

    doc <- rstudioapi::documentPath()
  }
}
