
#' Append links to cv printer attributes
#'
#' Helper function to append links to a `cv_printer` object's `links` attribute.
#'
#' @inheritParams print_skill_bars
#' @param new_links Character array of new links to append to links attribute.
#'
#' @return
#' @export
#'
#' @examples
append_links <- function(cv, new_links){

  UseMethod("append_links")
}

#' @export
append_links.cv_printer <- function(cv, new_links){
  attr(cv, 'links') <- c(attr(cv, 'links'), new_links)

  cv
}
