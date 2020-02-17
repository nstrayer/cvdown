#' Print stripped links to section
#'
#' Prints all the links that were stripped from various text as cv print object
#' was used to a single section with the link footnote identifier next to each
#' link.
#'
#' @inheritParams print_skill_bars
#'
#' @return
#' @export
#'
#' @examples
print_links <- function(cv){

  UseMethod("print_links")
}

print_links.default <- function(cv){
  cat("print_links generic")
}

#' @export
print_links.cv_printer <- function(cv){
  # Only prints stuff if cv printer was in pdf mode.
  if(attr(cv, 'pdf')){
    cat("

Links {data-icon=link}
--------------------------------------------------------------------------------

<br>


  ")
    links <- attr(cv, 'links')

    purrr::walk2(links, 1:length(links), function(link, index){
      cat(glue::glue('{index}. {link}'), "\n")
    })
  }
}
