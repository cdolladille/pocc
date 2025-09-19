#' My costum forest plot function
#'
#' Check the forest_charles vignette for many showcase.
#'
#' The present one is based on ggplot2. Starts back from scratch. You should provide a single file for the data, even in the case you want to draw 2 plots. A second file is needed for column name correspondence with some labels. Use `plot_lims` to set the overall plot limits, i.e. including space for printing `cols`. Use `ticks` to set the estimate plot limits (conditions the use of arrows for outsider values). `cols_custom` can be used if you want the content of some columns to be customized (some rows in plain text, others in
#' bold, etc.). Many more examples can be found in the vignette.
#'
#' @param path_data character string, path to a .csv data for main plot
#' @param cols character vector, columns to be printed
#' @param cols_name character vector of length `length(cols)`, names to be printed ahead of columns
#' @param cols_pos numeric vector, (probably will be removed in the shiny app) graphic positions to plot the columns. Must be the same length as cols.
#' @param cols_hjust numeric vector, hjust for cols (same length as cols)
#'
#' @param cols_custom character vector, columns requiring a special layout for the content.
#' @param cols_custom_fontface character string, a column with numeric values from 1 to 4,
#' specifying the fontface of cols_custom. 1 plain, 2 bold, 3 italic, 4 bold and italic.

#' @param est_1 character string, column with numeric values for the estimated effect to be plotted on the main plot.
#' @param est_1_low_ci character string, columns with numeric values for the confidence interval margins on the main plot.
#' @param est_1_up_ci character string
#' @param est_1_color character string, column used to define box colors
#' @param est_1_size character string, column used for est_1 box size.
#'
#' @param est_2 character string, all the same as for est_1, but can be NULL if there is no other plot.
#' @param est_2_low_ci character string, same as for est_1
#' @param est_2_up_ci character string, same as for est_1
#' @param est_2_color character string, same as for est_1
#' @param est_2_size character string, same as for est_1
#'
#' @param blank_rows numeric, blank rows under boxplot
#' @param title_size numeric
#' @param data_size numeric
#' @param axis_size numeric
#' @param log_scale logical, should scale be log-transformed?
#' @param size_range numeric length 2, min and max size of box plots.
#' @param plot_lims numeric vector length 2, overall plot limits (including space for columns printing, see details)
#' @param graph_width numeric, output graphics dimensions
#' @param graph_height numeric, output graphics dimensions
#' @param graphic1distance numeric, main plot position
#' @param g1_panelcolor character string, color of graphic 1 panel
#'
#' @param graphic2distance numeric, second plot position
#' @param g2_panelcolor character string, color of graphic 2 panel
#'
#' @param ticks numeric vector, plot ticks on the axis.
#' @param keep_grid logical, do you want to print the underlying grid? Helps for positionning columns
#' @param tiles character string, column indicating lines that need a rectangle
#' @param tiles_height a numeric, half-height of tiles. Defaults to 0.5
#' @param tiles_color character string, color to fill tiles with
#' @param box_shape a numeric, shape of boxes, see ggplot2 cheatsheet
#'
#' @keywords meta-analysis
#' @importFrom purrr map
#' @importFrom rlang !! !!! list2
#' @import ggplot2
#' @import grid
#' @export
#' @examples
#'
#' df <-
#'   dplyr::tibble(
#'     var = c("drug1", "drug2", "header", "drug3"),
#'     e1 = c(1.2, 0.8, NA, 5.0),
#'     e1_lci = c(0.9, 0.7, NA, 2.3),
#'     e1_uci = c(1.5, 0.9, NA, 7.8),
#'     col1 = vigicaen::cff(e1,
#'                         low_ci = e1_lci,
#'                         up_ci = e1_uci,
#'                         method = "num_ci",
#'                         dig = 1),
#'    e1_size = c(1, 10, NA, 3),
#'    e2_size = c(10, 1, NA, 3),
#'     color = ifelse(e1_lci > 1, "green", "red"),
#'     is_header = ifelse(
#'       is.na(e1),
#'       2,
#'       1),
#'     e2 = c(0.9, 0.5, NA, 0.8),
#'     e2_lci = c(0.8, 0.2, NA, 0.2),
#'     e2_uci = c(1.0, 0.8, NA, 1.4),
#'     e2_color = ifelse(e2_uci < 1, "blue", "black"),
#'     tiles_guide = c(1, 0, 0, 1)
#'   )
#'
#' # you can use a data.frame directly
#'
#' forest_charles(
#'   path_data = df,
#'   cols = "var",
#'   cols_pos = c(1),
#'
#'   est_1 = "e1",
#'   est_1_low_ci = "e1_lci",
#'   est_1_up_ci = "e1_uci"
#'
#' )
#'
#' # or use a csv file
#'
#' dir <- tempdir()
#' write.csv2(df, paste0(dir, "/", "test_df_for_forest_charles.csv"),
#'            row.names = FALSE)
#'
#'
#' forest_charles(
#'   path_data = paste0(dir, "/", "test_df_for_forest_charles.csv"),
#'   cols = "var",
#'   cols_pos = c(1),
#'   cols_hjust = 0.5,
#'
#'   est_1 = "e1",
#'   est_1_low_ci = "e1_lci",
#'   est_1_up_ci = "e1_uci"
#'
#' )
#'
#' # Plot multiple columns
#'
#' forest_charles(
#' path_data = df,
#' cols = c("var", "col1"),
#' cols_pos = c(1, 5),
#' cols_hjust = c(0.5, 0.5),
#'
#' est_1 = "e1",
#' est_1_low_ci = "e1_lci",
#' est_1_up_ci = "e1_uci"
#' )
#'
#'
#' # Plot 2 graphics
#'
#' forest_charles(
#'   path_data = df,
#'   cols = c("var"),
#'   cols_pos = c(1),
#'
#'   est_1 = "e1",
#'   est_1_low_ci = "e1_lci",
#'   est_1_up_ci = "e1_uci",
#'   graphic1distance = 10,
#'
#'   est_2 = "e2",
#'   est_2_low_ci = "e2_lci",
#'   est_2_up_ci = "e2_uci",
#'   graphic2distance = 15
#' )
#'
#' # Those 2 graphics have separate parameters
#'
#' forest_charles(
#'   path_data = df,
#'   cols = c("var", "col1"),
#'   cols_pos = c(1, 5),
#'   cols_hjust = c(0.5, 0.5),
#'
#'   est_1_color = "color",
#'   est_1_size = "e1_size",
#'   est_1 = "e1",
#'   est_1_low_ci = "e1_lci",
#'   est_1_up_ci = "e1_uci",
#'   graphic1distance = 10,
#'
#'   est_2_color = "e2_color",
#'   est_2 = "e2",
#'   est_2_low_ci = "e2_lci",
#'   est_2_up_ci = "e2_uci",
#'   est_2_size = "e2_size",
#'   graphic2distance = 15
#' )

forest_charles <-
  function(
    path_data,

    cols,
    cols_name = stringr::str_to_title(cols),
    cols_pos,
    cols_hjust = rep(.5, each = length(cols)),

    cols_custom = NULL,

    cols_custom_fontface = NULL,


    est_1,
    est_1_low_ci,
    est_1_up_ci,

    est_1_color = NULL, # see what can be done for the colors of other plot parts
    est_1_size = NULL,

    est_2 = NULL,
    est_2_low_ci = NULL,
    est_2_up_ci = NULL,

    est_2_color = NULL,
    est_2_size  = NULL,

    blank_rows = 2,
    title_size = 4,
    data_size = 9,
    axis_size = 6,

    plot_lims = c(0, 30),
    ticks = c(.1, .5, 1, 2, 10),
    log_scale = TRUE,

    size_range = c(1, 3),

    graph_width = 5,
    graph_height = 3.5,

    graphic1distance = 20,
    g1_panelcolor = "white",

    graphic2distance = NULL,

    g2_panelcolor = "white",

    keep_grid = FALSE,

    tiles = NULL,
    tiles_height = 0.5,
    tiles_color = "grey",

    box_shape = 16

  ){

    # parameters setting --- ####



#     rlang::set_names(cols, function(nom) paste0("col_", # seq_along(nom)))
    # lidee est de recuperer une list2 quon peut !!! un peu plus loin. vraiment utile ou non ?

    # col1 <- "value" |> set_names("col1")
    #
    # expr(f(!!!rlang::list2(col1)))
    #
    # rlang::sym(col1)

    size_converter <- 0.352777778

    data_size <- data_size * size_converter
    axis_size <- axis_size * size_converter

    yplot_lims <- plot_lims # the plot is a 90 degree rotation
    y_ticks <- ticks

    lowci_seuil <- min(y_ticks)
    upci_seuil <- max(y_ticks)

    ## BASIC THEMES (SO TO PLOT BLANK GRID)  ---- ####
    theme_bare <- theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.length = unit(0.0001, "mm"),
      axis.text = element_text(margin = unit(c(0,0,0,0), "lines")),
      legend.position = "none",
      panel.background = element_rect(fill = "transparent"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-0.1,-0.1,-0.1,-0.1), "mm"),
      plot.margin = unit(c(5,0,5,0.01), "mm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

    ## build a function to add the layers
    hl_rect <- function(col = "white",
                        alpha = 0.5) {
      rectGrob(
        x = 0,
        y = 0,
        width = 1,
        height = 1,
        just = c("left", "bottom"),
        gp = gpar(alpha = alpha, fill = col)
      )
    }

    ## DATA MANAGEMENT ---- ####

    data_load <-
      if("data.frame" %in% class(path_data)){
        path_data
      } else if("character" %in% class(path_data)){
        read.csv2(path_data,
                  stringsAsFactors = FALSE)
      }

    data1 <-
      data_load

    # check arg length consistency ---- ####

    if(!all(
      length(cols) ==
        purrr::map(
          list(cols_name,
            cols_pos,
            cols_hjust),
          length
        )
    )){
      stop("all of cols, cols_name, cols_pos and cols_hjust should be the same length.")
    }

    # col checkers ---- #####

    if(!all(cols %in% names(data_load))){
      absent_cols <-
        paste0(cols[!cols %in% names(data_load)], collapse = ", ")

      stop(paste0(
        "`cols` must be present in the initial dataset: can't find ",
        absent_cols, "."))
    }

    if(!all(cols_custom %in% cols)){
      not_in_cols <-
        paste0(cols_custom[!cols_custom %in% cols], collapse = ", ")
      stop(paste0(
        "`cols_custom` should all be present in arg `cols`: can't find ",
        not_in_cols, ".")
      )
    }

    if(!is.null(est_1_color) && class(data_load[[est_1_color]]) != "character"){
      stop(paste0(
        "`est_1_color` should be of character type. That's not the case of ",
        est_1_color, "."
      ))
    }

    if(!is.null(est_2_color) && class(data_load[[est_2_color]]) != "character"){
      stop(paste0(
        "`est_2_color` should be of character type. That's not the case of ",
        est_2_color, "."
      ))
    }

    # tiles should be 1/0 cols ----

    if(!is.null(tiles) &&
       !all(
         data_load[[tiles]] %in% c(0, 1)
         )
    )
       {
      stop(paste0(
        "`tiles` column should contain 0 and 1 only"
      ))
    }

    if(!is.null(tiles) && !tiles %in% names(data_load)){

      stop(paste0(
        "`tiles` must be present in the initial dataset: can't find ",
        tiles, "."))
    }

    # at least one est_1 value should fall into ticks ----

    # unless there are no values at all in est_1

    if(
      any(!is.na(data_load[[est_1]])) &&
        (all(data_load[[est_1]] > max(ticks)) |
         all(data_load[[est_1]] < min(ticks))
         )
      ){
      stop(paste0(
        "No value from ", est_1,
        " falls in `ticks` range: ", min(ticks), "-", max(ticks), ".",
        " Set ticks appropriately."
      ))
    }

    renamer_g1 <-
      rlang::list2(est_1, # old names
            est_1_low_ci,
            est_1_up_ci) |>
      rlang::syms() |>
      rlang::set_names(
        c("est_1", # new names
          "est_1_low_ci",
          "est_1_up_ci")
      )

    if(!is.null(est_1_color)){
      renamer_est_1_color <-
        rlang::list2(est_1_color) |>
        rlang::syms() |>
        rlang::set_names(
          c("est_1_color")
        )
    }

    if(!is.null(est_2_color)){
      renamer_est_2_color <-
        rlang::list2(est_2_color) |>
        rlang::syms() |>
        rlang::set_names(
          c("est_2_color")
        )
    }

    if(!is.null(est_1_size)){
      renamer_est_1_size <-
        rlang::list2(est_1_size) |>
        rlang::syms() |>
        rlang::set_names(
          c("est_1_size")
        )
    }

    if(!is.null(est_2_size)){
      renamer_est_2_size <-
        rlang::list2(est_2_size) |>
        rlang::syms() |>
        rlang::set_names(
          c("est_2_size")
        )
    }

    if(!is.null(est_2)){
      renamer_g2 <-
      rlang::list2(est_2, # old names
            est_2_low_ci,
            est_2_up_ci) |>
      rlang::syms() |>
      rlang::set_names(
        c("est_2", # new names
          "est_2_low_ci",
          "est_2_up_ci")
      )
    }

    if(!is.null(cols_custom)){
      renamer_ccff <-
        rlang::list2(cols_custom_fontface # old names
             ) |>
        rlang::syms() |>
        rlang::set_names(
          c("cols_custom_fontface" # new names
           )
        )
    }


    if(!is.null(tiles)){
      renamer_tiles <-
        rlang::list2(tiles # old names
        ) |>
        rlang::syms() |>
        rlang::set_names(
          c("tiles" # new names
          )
        )
    }


    if(is.null(est_1_color)){ # set a default color
      data1 <-
        data1 |>
        dplyr::mutate(est_1_color = "black")
    } else {
      data1 <-
        data1 |>
        dplyr::mutate(
          !!!renamer_est_1_color
        )
    }

    if(is.null(est_2_color)){ # set a default color
      data1 <-
        data1 |>
        dplyr::mutate(est_2_color = "black")
    } else {
      data1 <-
        data1 |>
        dplyr::mutate(
          !!!renamer_est_2_color
        )
    }

    if(is.null(est_1_size)){ # set a default size
      data1 <-
        data1 |>
        dplyr::mutate(est_1_size = 1)
    } else {
      data1 <-
        data1 |>
        dplyr::mutate(
          !!!renamer_est_1_size
        )
    }

    if(is.null(est_2_size)){ # set a default size
      data1 <-
        data1 |>
        dplyr::mutate(est_2_size = 1)
    } else {
      data1 <-
        data1 |>
        dplyr::mutate(
          !!!renamer_est_2_size
        )
    }



  data1 <-
    data1  |>
      dplyr::mutate(
        !!!renamer_g1,
          est_1_up_ci = ifelse(
          est_1_up_ci >= upci_seuil,
          upci_seuil,
          est_1_up_ci
        ),
        est_1_low_ci = ifelse(
          est_1_low_ci <= lowci_seuil,
          lowci_seuil,
          est_1_low_ci
        ),
        dplyr::across(dplyr::contains("est_1"),
               # dont plot estimates outside of plot_lims
               ~ ifelse(
                 est_1 <= lowci_seuil |
                   est_1 >= upci_seuil,
                 NA,
                 .)
               ),
        est_1_color = ifelse(
          is.na(est_1_color),
          "#ffffff",
          est_1_color
        )
        ) |>
      dplyr::mutate(
        order = nrow(data1) : 1) |>


      data.table::as.data.table()

  if(!is.null(est_2)){
    data1 <-
      data1 |>
      dplyr::mutate(
        !!!renamer_g2,
        est_2_up_ci = ifelse(
          est_2_up_ci >= upci_seuil,
          upci_seuil,
          est_2_up_ci
        ),
        est_2_low_ci = ifelse(
          est_2_low_ci <= lowci_seuil,
          lowci_seuil,
          est_2_low_ci
        ),
        dplyr::across(dplyr::contains("est_2"),
               # dont plot estimates outside of plot_lims
               ~ ifelse(
                 est_2 <= lowci_seuil |
                   est_2 >= upci_seuil,
                 NA,
                 .)
        ),
        est_2_color = ifelse(
          is.na(est_2_color),
          "#ffffff",
          est_2_color
        )
      )
  }



  # last data management steps

  if(!is.null(cols_custom)){
    data1 <-
      data1 |>
      dplyr::mutate(
        !!!renamer_ccff
      )
  }

  if(!is.null(tiles)){
    data1 <-
      data1 |>
      dplyr::mutate(
        !!!renamer_tiles
      )
  }

    ## GRAPHICAL SETTINGS ---- ####
    hl_rows <- data.frame(ID = (1:((nrow(
      data1
    )) / 2) * 2), col = "lightgrey")
    hl_rows$ID <- hl_rows$ID - blank_rows + 1

    ## Points to plot on the y scale
    scaledata <- data.frame(ID = 0, HR = y_ticks)


    ## DATA FOR TEXT LABELS ---- ####

    # distinguishing between custom cols and the others

    cols_custom_pos <-
      which(cols %in% cols_custom)

    cols_not_custom_pos <-
      which(!(cols %in% cols_custom))
    # the indices will help pick their hjust, x, y, and label
    # to build rt_lab and rt_lab_custom


    rt_lab <-
      dplyr::tibble(
        cols = cols,
        x = max(data1$order) + 1.5, # spacer between column titles and underlying data
        y = cols_pos,
        aes_hjust = cols_hjust,
        aes_label = cols_name
      ) |>
      data.table()

    rt_lab_call <- # les noms de rt_lab[["label"]] doivent correspondre a des noms de colonne dans data
      function(
    FUN = geom_text,
    data1,
    rt_lab,
    aes_x = "order",
    aes_y = "y",
    aes_hjust = "aes_hjust",
    aes_label = "aes_label",
    col = "n_i2",
    size = data_size
      ) {

        aes_x_s <- rlang::ensym(aes_x)
        aes_label_s <- rlang::ensym(aes_label)

        rlang::call2(rlang::expr(geom_text),
              data = rlang::quo(data1),
              rlang::call2(rlang::expr(aes),
                    x = rlang::expr(!!aes_x_s),
                    y = rt_lab[cols == col, ][[aes_y]],
                    hjust = rt_lab[cols == col, ][[aes_hjust]]),
              label = data1[[col]],
              size = rlang::quo(data_size))
      }

    rt_lab_custom_call <- # very similar to rt_lab, but with additional parameters
      function(
    FUN = geom_text,
    data1,
    rt_lab_,
    aes_x = "order",
    aes_y = "y",
    aes_hjust = "aes_hjust",
    aes_label = "aes_label",
    col = "n_i2",
    size = data_size
      ) {

        aes_x_s <- rlang::ensym(aes_x)
        aes_label_s <- rlang::ensym(aes_label)

        rlang::call2(rlang::expr(geom_text),
                     data = rlang::quo(data1),
                     rlang::call2(rlang::expr(aes),
                                  x = rlang::expr(!!aes_x_s),
                                  y = rt_lab_[cols == col, ][[aes_y]],
                                  hjust = rt_lab_[cols == col, ][[aes_hjust]]),
                     fontface = data1[[cols_custom_fontface]],
                     label = data1[[col]],
                     size = rlang::quo(data_size))
      }

    ## DATA for TILES ---- ####


    if(!is.null(tiles)) {
      tiles_data <-
        data1 |>
        dplyr::filter(tiles == 1)
    }


    # Log scaling ---- ####

    if(log_scale){
      rt_lab[["y"]] <- exp(rt_lab[["y"]])
      graphic1distance <- exp(graphic1distance)
      graphic2distance <-
        if(!is.null(graphic2distance))
           exp(graphic2distance)
      yplot_lims <- exp(yplot_lims)
    }

    `%+*%` <- # custom to set either + or * depending on log_scaling or not
      if(log_scale) {
        `*`
      } else {
        `+`
      }

    scale_y_charles <-
      if(log_scale) {
        ggplot2::scale_y_log10
      } else {
        ggplot2::scale_y_continuous
      }

    mark_O_1 <-
      if(log_scale) {
        1
      } else {
        0
      }


    # Plotting  ---- ####

    # color setting ---- ####
    mycolors_est_1 <- data1[["est_1_color"]]
    names(mycolors_est_1) <- mycolors_est_1
    # seems to be no longer used? its a simple graph without legend...

    # plot parts ----

    # core ---- ####

    core_plot <- function(data1, order, est_1){
      ggplot(data1, aes(order, est_1)) +
        labs(x = NULL, y = NULL)
    }

    # g functions ---- ####

    g_fond <- # g (graphique) fond
      function(data1, order,
               scaledata, HR,
               graphic1distance,
               fill = "white"){
      geom_rect(aes(xmin = 0,
                    xmax = max(data1$order) + .5,
                    ymin = min(scaledata$HR) %+*% graphic1distance,
                    ymax = max(scaledata$HR) %+*% graphic1distance), fill = fill)
      }

    g_ticks_line <- # g lignes des flipped y-ticks
      function(scaledata, data1,
               HR,
               graphic1distance,
               g_tl_linetype = 2,
               g_tl_size = 0.1){
        geom_segment(
          data = scaledata,
          aes(x = 0,
              xend = nrow(data1) + .5,
              y = HR %+*% graphic1distance,
              yend = HR %+*% graphic1distance),
              color = "black",
          linetype = g_tl_linetype,
          linewidth = g_tl_size)
      }

    g_ticks_themselves <-
      function(scaledata,
               HR,
               graphic1distance,
               g_tt_size = 0.1){
        geom_segment(
          data = data.frame(y = scaledata$HR),
          aes(
            x = -0.05,
            xend = .2,
            y = y %+*% graphic1distance,
            yend = y %+*% graphic1distance
          ),
          color = "black",
          linetype = 1,
          linewidth = g_tt_size
        )
      }

    g_ticks_labels <-
      function(scaledata, ID, HR,
               graphic1distance,
               axis_size){
        geom_text(data = scaledata,
                  aes(x = ID - .4,
                      y = HR %+*% graphic1distance,
                      label = HR),
                  size = axis_size)
      }

    g_median_line <- # g1 ligne de la valeur mediane (O ou 1 selon echelle)
      function(data1, order,
               mark_O_1,
               graphic1distance,
               g_ml_linetype = 1,
               g_ml_size = 1){
        geom_segment(
          aes(x = 0,
              xend = max(data1$order) + .5,
              y = mark_O_1 %+*% graphic1distance,
              yend = mark_O_1 %+*% graphic1distance),
          linetype = g_ml_linetype,
          linewidth = g_ml_size)
      }

    g_x_axis <- # actually, the y-flipped axis
      function(scaledata, HR,
               graphic1distance,
               g_xa_size = .1){
        geom_segment(
          aes(x = 0,
              xend = 0,
              y = min(scaledata$HR) %+*% graphic1distance,
              yend = max(scaledata$HR) %+*% graphic1distance),
          linewidth = g_xa_size)
      }

    g_est <- # g1 box plotting
      function(data1, order, est_1,
               graphic1distance,
               g_e1_size  = est_1_size,
               g_e1_color = est_1_color){
        g_e1_color <- rlang::ensym(g_e1_color)
        g_e1_size  <- rlang::ensym(g_e1_size)
        est_1      <- rlang::ensym(est_1)
        data_est <-
          data1 |> dplyr::filter(!is.na(!!est_1))
        rlang::quo(
          geom_point(
            data = data_est,
            aes(order,
                y = !!est_1 %+*% graphic1distance,
                size = !!g_e1_size),
            color = data_est |>
              dplyr::pull(!!g_e1_color),
            # size = data1 |>
            #   dplyr::pull(!!g_e1_size),
            shape = box_shape)
          ) |>
          rlang::eval_tidy()
      }

    g_est_1_ci <- # g1 error bar of est_1
      function(data1, est_1_low_ci, est_1_up_ci,
               graphic1distance,
               g_e1ci_color = est_1_color,
               g_e1ci_width = 0,
               g_e1ci_size = .2
      ){
        g_e1ci_color <- rlang::ensym(g_e1ci_color)
        est_1_low_ci <- rlang::ensym(est_1_low_ci)
        est_1_up_ci <-  rlang::ensym(est_1_up_ci)
        data_est <-
          data1 |> dplyr::filter(!is.na(!!est_1))
        rlang::quo(
        geom_errorbar(
          data = data_est,
          aes(ymin = !!est_1_low_ci %+*% graphic1distance,
              ymax = !!est_1_up_ci %+*% graphic1distance,
              color = !!g_e1ci_color),
          width = g_e1ci_width,
          linewidth = g_e1ci_size)
        ) |>
          rlang::eval_tidy()
      }

    g_scale_color <- # g1 colors
      function(est_color = "est_1_color",
               mycolors # see upfront
      ){
        # VERY tricky function, there should not be NAs in mycolors
        scale_color_manual(
          name = est_color,
          values = mycolors)
      }

    g_low_ci_arrows <- # outsiders values (compared to ticks) are printed with an arrow
      function(data1, order, est_1_low_ci,
               lowci_seuil,
               graphic1distance,
               est_1_color,
               g_lcia_size = .05
      ){
        geom_segment(
          data = data1 |>
            dplyr::filter(est_1_low_ci == lowci_seuil),
          aes(x = order,
              xend = order,
              y = est_1_low_ci %+*% graphic1distance,
              yend = (est_1_low_ci + .1) %+*% graphic1distance,
              color = est_1_color),
          linewidth = g_lcia_size,
          arrow =
            arrow(ends = "first",
                  length= unit(.05, "cm"),
                  type = "closed")
          )
      }

    g_up_ci_arrows <- # same but for upper outsiders
      function(data1, order, est_1_up_ci,
               upci_seuil,
               graphic1distance,
               est_1_color,
               g_ucia_size = .05
      ){
        geom_segment(
          data = data1 |>
            dplyr::filter(est_1_up_ci == upci_seuil),
          aes(x = order,
              xend = order,
              y = (est_1_up_ci - .1) %+*%  graphic1distance,
              yend = (est_1_up_ci) %+*%  graphic1distance,
              color = est_1_color),
          linewidth = g_ucia_size,
          arrow =
            arrow(ends = "last",
                  length= unit(.05, "cm"),
                  type = "closed")
          )
      }

    # columns ---- ####

    col_headers <- # columns headers
      function(rt_lab, x, y,
               aes_label, aes_hjust,
               ch_fontface = "bold",
               data_size
      ){
        geom_text(
          data = rt_lab,
          aes(x = x, y = y,
              label = aes_label,
              hjust = aes_hjust,
              fontface = ch_fontface),
          size = data_size
        )
      }

    col_text <- # columns content
      purrr::map(rt_lab[["cols"]][cols_not_custom_pos], function(col_)
        rt_lab_call(
          data1 = data1,
          rt_lab = rt_lab,
          col = col_,
          size = data_size
        ))

    col_text_custom <- # columns content for customized cols
      purrr::map(rt_lab[["cols"]][cols_custom_pos], function(col_)
        rt_lab_custom_call(
          data1 = data1,
          rt_lab_ = rt_lab,
          col = col_,
          size = data_size
        ))

    # tiles ---- ####

    g_tiles <- # g (graphique) tiles # 2024 03 29 to be continued
      function(tiles_data, order,
               scaledata, HR,
               yplot_lims,
               tiles_height,
               tiles_color,
               tiles_alpha){
        geom_rect(data = tiles_data,
                  aes(xmin = tiles_data$order - tiles_height,
                      xmax = tiles_data$order + tiles_height,
                      ymin = min(yplot_lims),
                      ymax = max(yplot_lims)
                      ),
                  fill = tiles_color,
                  alpha = tiles_alpha,
                  colour = NA
                  )
      }

    # final plotting ---- ####
    g_final <-
      core_plot(data1, order, est_1) +

      # plotting graphic backgrounds ---- ####

    g_fond(data1, order,
           scaledata, HR,
           graphic1distance,
           fill = g1_panelcolor) +

      {if(!is.null(est_2)){ # did not find a better way to conditionnally add this second plot

        g_fond(data1, order,
               scaledata, HR,
               graphic2distance,
               fill = g2_panelcolor)
      }} +

    # plotting tiles ---- #####

    {if(!is.null(tiles)){

      g_tiles(tiles_data, order,
              scaledata, HR,
              yplot_lims = yplot_lims,
              tiles_height = tiles_height,
              tiles_alpha  = 1,
              tiles_color  = tiles_color)
    }}  +

      # plotting g1 ---- ####

      g_ticks_line(scaledata, data1,
                    HR, graphic1distance,
                    g_tl_linetype = 2,
                    g_tl_size = 0.1) +
      g_ticks_themselves(
        scaledata,
        HR,
        graphic1distance,
        g_tt_size = 0.1
      ) +
      g_ticks_labels(scaledata, ID, HR,
               graphic1distance,
               axis_size) +

      g_x_axis(scaledata, HR,
                graphic1distance,
                g_xa_size = .1) +

      g_median_line(data1, order,
                     mark_O_1,
                     graphic1distance,
                     g_ml_linetype = 1,
                     g_ml_size = 1) +

      g_est_1_ci(data1, est_1_low_ci, est_1_up_ci,
                  graphic1distance,
                  g_e1ci_color = est_1_color,
                  g_e1ci_width = 0,
                  g_e1ci_size = .2) +

      g_low_ci_arrows(data1, order, est_1_low_ci,
                       lowci_seuil,
                       graphic1distance,
                       est_1_color,
                       g_lcia_size = .05) +

      g_up_ci_arrows(data1, order, est_1_up_ci,
                      upci_seuil,
                      graphic1distance,
                      est_1_color,
                      g_ucia_size = .05) +

      g_est(data1, order, est_1,
              graphic1distance,
              g_e1_size = est_1_size,
              g_e1_color = est_1_color) +

      g_scale_color(est_color = "est_1_color",
                    mycolors_est_1) +
      # plotting g2 ---- ####

      {if(!is.null(est_2)){
            g_ticks_line(scaledata, data1,
                         HR, graphic2distance,
                         g_tl_linetype = 2,
                         g_tl_size = 0.1)
        }} +
      {if(!is.null(est_2)){
            g_ticks_themselves(
              scaledata,
              HR,
              graphic2distance,
              g_tt_size = 0.1
            )
        }} +
      {if(!is.null(est_2)){
        g_ticks_labels(scaledata, ID, HR,
                           graphic2distance,
                           axis_size)
        }} +
      {if(!is.null(est_2)){
            g_x_axis(scaledata, HR,
                     graphic2distance,
                     g_xa_size = .1)
        }} +
      {if(!is.null(est_2)){
            g_median_line(data1, order,
                          mark_O_1,
                          graphic2distance,
                          g_ml_linetype = 1,
                          g_ml_size = 1)
        }} +
      {if(!is.null(est_2)){
           g_est_1_ci(data1, est_2_low_ci, est_2_up_ci,
                      graphic2distance,
                      g_e1ci_color = est_2_color,
                      g_e1ci_width = 0,
                      g_e1ci_size = .2)
        }} +
      {if(!is.null(est_2)){
           g_low_ci_arrows(data1, order, est_2_low_ci,
                           lowci_seuil,
                           graphic2distance,
                           est_2_color,
                           g_lcia_size = .05)
        }} +
      {if(!is.null(est_2)){
           g_up_ci_arrows(data1, order, est_2_up_ci,
                          upci_seuil,
                          graphic2distance,
                          est_2_color,
                          g_ucia_size = .05)
        }} +
      {if(!is.null(est_2)){
           g_est(data1, order, est_2,
                  graphic2distance,
                  g_e1_size = est_2_size,
                  g_e1_color = est_2_color)
      }} +


       # plotting scalers and coord_flip  ---- ####

      scale_size(range = size_range) +

      scale_y_charles() +

      coord_flip(ylim = yplot_lims) +

      # plotting column header and content ---- ####

      col_headers(rt_lab, x, y,
                  aes_label, aes_hjust,
                  ch_fontface = "bold",
                  data_size) +

      purrr::map(col_text, rlang::eval_tidy) +

      {if(!is.null(cols_custom)){
        purrr::map(col_text_custom, rlang::eval_tidy)
      }} +

      # plotting (or not) the grid ---- ####

      if(keep_grid) { theme_grid } else { theme_bare }

    g_final
  }
