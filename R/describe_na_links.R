#' Explore MAR hypothesis
#'
#' Missing at Random (MAR) mechanism of missing data exploration, by bivariate analyses.
#'
#' Exploration of potential mechanism of missingness is mandatory before performing missing data imputation. The two acceptable settings are the Missing At Random (MAR) mechanism and Missing Completely At Random (MCAR). MAR may be assumed under certain conditions. One of them is that missingness of variables is associated with other variables (the aim is to reduce the plausibility of the last mechanism, Missing Not At Random). This function performs bivariate analyses via glm, between `is.na(var)`, where `var` is the variable of interest containing missing data, and one or more explaining variables, from `var_list`.
#' The function accepts more than one `var` to be evaluated, in the `var` argument.
#'
#' @param var A character vector, one or more variables with missing data.
#' @param var_list A character vector, the potential predictors of is.na(var),
#' @param df_md A data.frame, the underlying (unimputed) data.frame
#'
#' @return A data.frame, one row per `var`, with the number of associated variables and the Odds details.
#
#' @keywords missing data imputation
#' @importFrom purrr map list_rbind
#' @export
#' @examples
#' df_md <-
#'   data.frame(
#'     shuffle = rnorm(1000)
#'   )  |>
#'   dplyr::mutate(
#'     v1 = c(rep(1, 300), rep(0, 700)),
#'     v1 = c(rep(1, 250), rep(NA, 50), rep(0, 700)),
#'     v2 = ifelse(shuffle < - 0.8, 1, 0),
#'     v3 = c(rep(1, 200), rep(0, 70), rep(1, 30), rep(0, 700)),
#'     across(c(v1, v2, v3), as.factor)
#'   )
#'
#' md_diag <-
#'   describe_na_links(
#'     var = "v1",
#'     var_list = c("v3"),
#'     df_md = df_md
#'   )

describe_na_links <-
  function( # Compute univariate analysis between is.na(var) and other variables of interest var_list in a df_md dataset
    var, # character vector, one or more variables with missing data
    var_list, # character vector, the potential predictors of is.na(var),
    df_md # data.frame, the underlying data.frame (non imputed)
  ){
    core <- # this will perform a one to one (is.na(var) to one of var_list) analysis.
      function(var_,
               # a character string (typically one element of var, we will map around this core function later)
               var_list_,
               # also a character string
               df_md){

        fml <- expr(is.na(!!sym(var_)) ~ !!sym(var_list_))

        mod <- glm(
          eval(fml),
          data = df_md,
          family = binomial(link = "logit"),
          na.action = na.omit # why do i have to specify this ???
        )

        data.table::data.table(
          md_var = var_,
          summary(mod)$coefficients[-1, , drop = FALSE],
          keep.rownames = TRUE
        )  |>
          data.frame() |>
          dplyr::mutate(signif = ifelse(Pr...z.. < 0.05, 1, 0),
                 pval = vigicaen::nice_p(Pr...z..)) |>
          vigicaen::compute_or_mod(
            estimate = Estimate,
            std_er = Std..Error
          )
      }

    core_wrapper_around_var_list <-
      function(var_,
               var_list,
               core){
        mod_table <-
          purrr::map(var_list,
                  function(var_list_)
                    core(var_, var_list_, df_md)) |>
          purrr::list_rbind()

        associated_vars_detail <-
          mod_table |>
          dplyr::filter(signif == 1) |>
          dplyr::transmute(
            detail = paste0(rn, " (", orl, " ", ci, ")")
          ) |>
          paste0(collapse = ", ")

        data.frame(
          var = var_,
          total_vars_assessed = length(var_list),
          total_vars_associated =
            mod_table |>
            dplyr::filter(signif == 1) |>
            nrow(),
          associated_vars_detail =
            associated_vars_detail,
          ci.level = unique(mod_table$ci_level)
        )
      }

    purrr::map(var, function(var_)
      core_wrapper_around_var_list(var_ = var_, var_list, core)
    ) |>
      purrr::list_rbind()
  }
