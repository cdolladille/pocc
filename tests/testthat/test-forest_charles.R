if (requireNamespace("vdiffr", quietly = TRUE) && utils::packageVersion('testthat') >= '3.0.3' && utils::packageVersion('base') >= '4.4.1') {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  # If vdiffr is not available and visual tests are explicitly required, raise error.
  if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {
    abort("vdiffr is not installed")
  }

  # Otherwise, assign a dummy function
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}

test_that("detect errors with cols", {
  df <-
    dplyr::tibble(
      var = c("drug1", "drug2", "header", "drug3"),
      e1 = c(1.2, 0.8, NA, 5.0),
      e1_lci = c(0.9, 0.7, NA, 2.3),
      e1_uci = c(1.5, 0.9, NA, 7.8),
      col1 = vigicaen::cff(e1,
                          low_ci = e1_lci,
                          up_ci = e1_uci,
                          method = "num_ci",
                          dig = 1),
      color = ifelse(e1_lci > 1, "green", "red"),
      is_header = ifelse(
        is.na(e1),
        2,
        1),
      e2 = c(0.9, 0.5, NA, 0.8),
      e2_lci = c(0.8, 0.2, NA, 0.2),
      e2_uci = c(1.0, 0.8, NA, 1.4),
      e2_color = ifelse(e2_uci < 1, "blue", "black")
    )
  # dir <- tempdir()
  # write.csv2(df, paste0(dir, "/", "test_df_for_forest_charles.csv"),
  #            row.names = FALSE)


  expect_error(
    forest_charles(
    path_data = df,
    cols = "vari",
    cols_pos = c(1),
    cols_hjust = 0.5,

    est_1_color = "color",

    est_1 = "e1",
    est_1_low_ci = "e1_lci",
    est_1_up_ci = "e1_uci",

    graphic1distance = 10,

  ),
  "`cols` must be present in the initial dataset: can't find vari."
  )

  expect_error(
    forest_charles(
      path_data = df,
      cols = "var",
      cols_pos = c(1),
      cols_hjust = 0.5,

      cols_custom = "col1",

      est_1_color = "color",

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci",

      graphic1distance = 10,

    ),
    "`cols_custom` should all be present in arg `cols`: can't find col1."
  )

  expect_error(
    forest_charles(
      path_data = df,
      cols = "var",
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1_color = "is_header",

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci",

      graphic1distance = 10,

    ),
    "`est_1_color` should be of character type. That's not the case of is_header."
  )

  expect_error(
    forest_charles(
      path_data = df,
      cols = "var",
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1_color = "color",

      est_2_color = "is_header",

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci",

      graphic1distance = 10,

    ),
    "`est_2_color` should be of character type. That's not the case of is_header."
  )

  expect_error(
    forest_charles(
      path_data = df,
      cols = "var",
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1_color = "color",

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci",

      graphic1distance = 10,

      tiles = "wrong_tiles_column"

    ),
    "`tiles` must be present in the initial dataset: can't find wrong_tiles_column."
  )


  expect_error(
    df |>
      dplyr::mutate(
        wrong_tiles = c(0, 1, 2, 3) # should be 0 or 1
      ) |>
    forest_charles(

      cols = "var",
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1_color = "color",

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci",

      graphic1distance = 10,

      tiles = "wrong_tiles"

    ),
    "`tiles` column should contain 0 and 1 only"
  )

})

test_that("cols, cols_hjust, cols_names and cols_pos are same length", {
  df <-
   dplyr::tibble(
      var = c("drug1", "drug2", "header", "drug3"),
      e1 = c(1.2, 0.8, NA, 5.0),
      e1_lci = c(0.9, 0.7, NA, 2.3),
      e1_uci = c(1.5, 0.9, NA, 7.8)
    )

  expect_error(
    forest_charles(
      path_data = df,
      cols = c("var", "e1"),
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci"
    )
    ,
    "all of cols, cols_name, cols_pos and cols_hjust should be the same length."
  )
})

test_that("works with no estimate", {
  df <-
    dplyr::tibble(
      var = c("drug1"),
      e1 = NA,
      e1_lci = NA,
      e1_uci = NA
    )

  expect_doppelganger(
    title = "no estimates",
    forest_charles(
      path_data = df,
      cols = c("var"),
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci"
    )
  )
})

test_that("works with a unique estimate", {
  df <-
    dplyr::tibble(
      var = c("drug1"),
      e1 = c(1.2),
      e1_lci = c(0.9),
      e1_uci = c(1.5)
    )

  expect_doppelganger(
    title = "one estimate",
    forest_charles(
      path_data = df,
      cols = c("var"),
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci"
    )
  )
})

test_that("works with a repeated unique estimate", {
  df <-
    dplyr::tibble(
      var = c("drug1", "drug1"),
      e1 = c(1.2, 1.2),
      e1_lci = c(0.9, 0.9),
      e1_uci = c(1.5, 1.5)
    )

  expect_doppelganger(
    title = "repeated estimate",
    forest_charles(
      path_data = df,
      cols = c("var"),
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci"
    )
  )

  df <-
    dplyr::tibble(
      var = c("drug1", "drug1", NA),
      e1 = c(1.2, 1.2, NA),
      e1_lci = c(0.9, 0.9, NA),
      e1_uci = c(1.5, 1.5, NA)
    )

  expect_doppelganger(
    title = "repeated estimate and NA",
    forest_charles(
      path_data = df,
      cols = c("var"),
      cols_pos = c(1),
      cols_hjust = 0.5,

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci"
    )
  )
})

test_that("error if all available values of est_1 are out of ticks", {
  df <-
    dplyr::tibble(
      var = c("drug1"),
      e1 = c(10),
      e1_lci = c(9),
      e1_uci = c(11)
    )

  expect_error(
    forest_charles(
      path_data = df,
      cols = c("var"),
      cols_pos = c(1),

      est_1 = "e1",
      est_1_low_ci = "e1_lci",
      est_1_up_ci = "e1_uci",
      ticks = c(0.5, 1, 2)
    ),
    "No value from e1 falls in `ticks` range: 0.5-2. Set ticks appropriately."
  )
})

test_that("tiles work", {
 df <-
   dplyr::tibble(
     var = c("drug1", "drug2", "header", "drug3"),
     e1 = c(1.2, 0.8, NA, 5.0),
     e1_lci = c(0.9, 0.7, NA, 2.3),
     e1_uci = c(1.5, 0.9, NA, 7.8),
     col1 = charles::cff(e1,
                         low_ci = e1_lci,
                         up_ci = e1_uci,
                         method = "num_ci",
                         dig = 1),
    e1_size = c(1, 10, NA, 3),
    e2_size = c(10, 1, NA, 3),
     color = ifelse(e1_lci > 1, "green", "red"),
     is_header = ifelse(
       is.na(e1),
       2,
       1),
     e2 = c(0.9, 0.5, NA, 0.8),
     e2_lci = c(0.8, 0.2, NA, 0.2),
     e2_uci = c(1.0, 0.8, NA, 1.4),
     e2_color = ifelse(e2_uci < 1, "blue", "black"),
     tiles_guide = c(1, 0, 0, 1)
   )


 expect_doppelganger(
   title = "tiles_basic",
   suppressWarnings(forest_charles(
   path_data = df,
   cols = "var",
   cols_pos = c(2),

   est_1 = "e1",
   est_1_low_ci = "e1_lci",
   est_1_up_ci = "e1_uci",

   tiles = "tiles_guide"
 )))
})
