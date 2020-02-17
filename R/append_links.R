
append_links <- function(cv, new_links){
  UseMethod("append_links")
}

#' @export
append_links.cv_printer <- function(cv, new_links){
  attr(cv, 'links') <- c(attr(cv, 'links'), new_links)

  cv
}
