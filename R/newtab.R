#' Creates a new Rmarkdown tab.
#'
#' Make sure that Rmd chunk option results='asis' is present.
#'
#' @param title Tab title
#' @param header An int indicating Rmd html header level (e.g. 3 means "###")
#' @param nonumber A boolean for whether tab should be numbered or not
#' @param subtabs A boolean for whether tab has subtabs below it or not
#' @param pills A boolean for whether tab should be pill-shaped or not
#'
#' @export
newtab = function(title="Tab", header=2, nonumber=T, subtabs=F, pills=F) {
  cat("  \n\n")

  # create header hashtags
  HEAD = ""
  for (i in 1:header) {
    HEAD = paste0(HEAD, "#")
  }

  # check if there are any options
  ANYOPTIONS = subtabs | nonumber

  # create subtabs and numbering indicators
  OPTIONS = ""
  if (ANYOPTIONS) {
    OPTIONS = paste0(OPTIONS, "{")
    if (nonumber) { OPTIONS = paste0(OPTIONS, "- ") }
    if (subtabs){
      OPTIONS = paste0(OPTIONS, ".tabset .tabset-fade")
      if (pills) { OPTIONS = paste0(OPTIONS, " .tabset-pills") }
    }
    OPTIONS = paste0(OPTIONS, "}")
  }

  cat(HEAD, " ", title, " ", OPTIONS, "  \n\n")

  cat("  \n\n")
}

#' @export
newtab1 = function(...) newtab(header=1, ...)
#' @export
newtab2 = function(...) newtab(header=2, ...)
#' @export
newtab3 = function(...) newtab(header=3, ...)
#' @export
newtab4 = function(...) newtab(header=4, ...)
#' @export
newtab5 = function(...) newtab(header=5, ...)
