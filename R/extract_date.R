#' Extract last modified date of file
#'
#' Short-hand around `file.info()`
#'
#' The function examines a file located at `path_file` and returns the last modified date (as a character string.
#'
#' @param path_file A character string, the path to the file.
#'
#' @return A character string, the last modified date of the file.
#
#' @keywords file
#' @export
#' @examples
#' tf <-
#'   tempfile()
#'
#'  write.csv2(mtcars, file = tf)
#'
#'  extract_date(tf)
#'
#'  file.remove(tf)

extract_date <- function(path_file){
  file.info(path_file)$mtime  |>
    stringr::str_extract("^.{10}")
}
