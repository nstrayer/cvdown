new_cv_printer <- function(cv_data, pdf_mode = TRUE){


  structure(
    cv_data,
    links = c(),
    pdf = pdf_mode,
    class = 'cv_printer'
  )
}


