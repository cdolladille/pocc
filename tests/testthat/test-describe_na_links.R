test_that("produces good odds", {
  set.seed(123)

  df_md <-
    data.frame(
      shuffle = rnorm(1000)
      )  |>
    dplyr::mutate(
      v1 = c(rep(1, 300), rep(0, 700)),
      v1 = c(rep(1, 250), rep(NA, 50), rep(0, 700)),
      v2 = ifelse(shuffle < - 0.8, 1, 0),
      v3 = c(rep(1, 200), rep(0, 70), rep(1, 30), rep(0, 700)),
      dplyr::across(c(v1, v2, v3), as.factor)
    )

  md_diag <-
    describe_na_links(
      var = "v1",
      var_list = c("v3"),
      df_md = df_md
      )

  good_odd <-
    summary(glm(is.na(v1) ~ v3, family = "binomial",
                data = df_md))$coefficients[- 1, 1] %>%
    exp() %>%
    vigicaen::cff(dig = 2)

  pgo_res <-
    md_diag %>%
    dplyr::pull(associated_vars_detail) %>%
    stringr::str_detect(good_odd)

  expect_true(pgo_res)
})
