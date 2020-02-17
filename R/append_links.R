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
#'
#' text_w_links <- "Here is my text with a cool [markdown link](https://my_cool_website.com/a_page) in it. For good measure [here's another link.](http://another_cool_site.com/)"
#'
#' # Setup empty CV printer
#' printer <- new_cv_printer(list(), pdf_mode = TRUE)
#'
#' extract_results <- extract_links(text_w_links, printer)
#'
#' # Text w/ no links is here
#' extract_results$text
#'
#' # Extracted links are here
#' extract_results$links
#'
#' # No links stored yet
#' attr(printer, 'links')
#'
#' # We can then append these links to our CV so it remembers them for later footnote printing
#' printer <- printer %>% append_links(extract_results$links)
#'
#' # Links have been updated
#' attr(printer, 'links')
#'
append_links <- function(cv, new_links){

  UseMethod("append_links")
}

#' @export
append_links.cv_printer <- function(cv, new_links){
  attr(cv, 'links') <- c(attr(cv, 'links'), new_links)

  cv
}
