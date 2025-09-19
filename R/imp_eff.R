#' Average effectives after multiple imputation
#'
#' This function provides averaged effectives for categorical variables after missing data imputation.
#' 
#' You need to provide imputed datasets in a long format `data.table`, not containing the initial dataset. For example, with the `mice` package, `imp_data <- mice::complete(imp, "long", include = FALSE)`.
#' 
#' @param var A character vector, the variable to count the effectives of.
#' @param imp_data The long format `data.table` of imputed datasets.
#' @keywords mice missing_data
#' @import data.table
#' @export
#' @examples
#' ## not yet ready
#' 

imp_eff <- function(var,
                    imp_data) {
  x <- c(var, ".imp")
  
  res <- imp_data[, .N, by = x][, lapply(.SD, mean), by = var]
  
  data.table::setnames(res, c("level", "imp", "N"))
  
  res[, .(var = var,
          level,
          N)]
  
}