#' Setup CV printer object
#'
#' @param cv_data List of dataframes containging info for CV
#' @param pdf_mode Boolean of if document is being exported as pdf and thus
#'   links should be stripped for placement at end.
#'
#' @return
#' @export
#'
#' @examples
new_cv_printer <- function(cv_data, pdf_mode = TRUE){

  structure(
    cv_data,
    links = c(),
    pdf = pdf_mode,
    class = 'cv_printer'
  )
}


