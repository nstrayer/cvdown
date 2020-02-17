#' Print skill bars
#'
#' Writes out the skill bars as divs with varying widths based on skill level
#'
#' @param cv object of class `cv_printer` created by \code{\link{new_cv_printer}}
#' @param out_of What is the skill value out of?
#' @param bar_color Color of filled in skill bar
#' @param bar_background Color of background (unfilled in part) of skill bar.
#'
#' @return
#' @export
#'
#' @examples
print_skill_bars <- function(cv, out_of = 5, bar_color = "#969696", bar_background = "#d9d9d9"){

  UseMethod("print_skill_bars")
}

#' @export
print_skill_bars.cv_printer <- function(cv, out_of = 5, bar_color = "#969696", bar_background = "#d9d9d9"){
  if(is.null(cv$skills)){
    stop("Missing skills data. Make sure when you setup your cv printer your passed data has skills data stored with list key of 'skills'.")
  }

  cv$skills %>%
    mutate(width_percent = round(100*level/out_of)) %>%
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    ) %>%
    cat(sep = "")

  cv
}


