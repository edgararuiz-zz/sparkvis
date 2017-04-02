#' @export
apply_props.tbl_spark <- function(data, props) {
    cols <- lapply(props, prop_value, data = data)
    colnames(cols) <- vapply(props, prop_label, character(1))
    quickdf(cols)
  }

#' @export
prop_type.tbl_spark <- function(data, prop) {

  if("tbl_spark" %in% class(data))
  {
    x_var <- as.character(prop$value)
    suppressMessages({
      top_rows <- data %>%
        dplyr::select_(x_var) %>%
        dplyr::top_n(10) %>%
        dplyr::collect()
    })
    s <- ggvis::vector_type(top_rows[[x_var]])
    return(s)

  }else
  {ggvis::vector_type(ggvis:::prop_value(prop, data))}
}


#' @export
eval_vector.tbl_spark <- function(x, f) {
  suppressMessages({
    field_name <- as.character(f)[2]

    top_rows <- x %>%
      dplyr::select_(field_name) %>%
      dplyr::top_n(10) %>%
      dplyr::collect()
  })
  return(top_rows[[field_name]])
}

#' @export
compute_bin.tbl_spark  <- function(x, x_var, w_var = NULL,
                                   width = NULL, center = NULL,
                                   boundary = NULL, closed = c("right", "left"),
                                   pad = FALSE, binwidth = NULL){


  x_name <- as.character(x_var)[2]

  data_prep <- dplyr::select_(x, x_field = x_name)
  data_prep <- dplyr::filter(data_prep, !is.na(x_field))
  data_prep <- dplyr::mutate(data_prep, x_field = as.double(x_field))

  s <- dplyr::summarise(data_prep,
                        max_x = max(x_field),
                        min_x = min(x_field),
                        mean_x = mean(x_field))
  s <- dplyr::collect(s)


  xmin <- s$min_x
  xmax <- s$max_x
  xmean <- s$mean_x

  width <- ifelse(is.null(width), 1, width)

  bins <- ceiling((xmax - xmin) / width)

  center <- ifelse(is.null(center), xmean, center)

  center_point <- center - (width  / 2)

  left_bins <- ceiling((center - xmin) / width)

  left_value <- center_point - (left_bins * width)


  new_bins <- left_value + (c(0:(bins + 1)) * width)


  all_bins <- data.frame(key_bin = 0:(length(new_bins) - 2),
                         bin = 1:(length(new_bins) - 1),
                         bin_ceiling = head(new_bins, - 1),
                         bin_floor = tail(new_bins, -1))



  plot_table <- sparklyr::ft_bucketizer(data_prep, input.col = "x_field", output.col = "key_bin", splits = new_bins)
  plot_table <- dplyr::group_by(plot_table, key_bin)
  plot_table <- dplyr::tally(plot_table)
  plot_table <- dplyr::collect(plot_table)

  all_bins <- data.frame(key_bin = 0:(length(new_bins) - 2),
                         bin = 1:(length(new_bins) - 1),
                         bin_ceiling = head(new_bins, - 1),
                         bin_floor = tail(new_bins, -1))

  all_bins$x <- all_bins$bin_ceiling + ((all_bins$bin_floor - all_bins$bin_ceiling) / 2)

  plot_table <- dplyr::full_join(plot_table, all_bins, by = "key_bin")
  plot_table <- dplyr::arrange(plot_table, key_bin)
  plot_table <- dplyr::mutate(plot_table, n = ifelse(!is.na(n), n, 0),
                              width = width)
  plot_table <- dplyr::select(plot_table, count_ = n, x_ = x, xmin_ = bin_ceiling , xmax_ = bin_floor, width_ = width)

  plot_table <- as.data.frame(plot_table,  stringsAsFactors = FALSE)

  return(plot_table)

}


#' @export
preserve_constants.tbl_spark  <- function(input, output) {
  if("tbl_spark" %in% class(input))
  {output}else
      {ggvis:::preserve_constants.data.frame(input, output)}
}





#' @export
compute_count.tbl_spark <- function(x, x_var, w_var = NULL) {

  x_field <- as.character(x_var)[2]


  data_prep <- dplyr::mutate_(x, x = x_field)
  data_prep <- dplyr::filter(data_prep, !is.na(x))

  s <- dplyr::group_by(data_prep, x)

  if(is.null(w_var)){
    s <- dplyr::tally(s)
  }else{
    w <- as.character(w_var)[2]
    s <- dplyr::mutate_(s, weight = w)
    s <- dplyr::summarise(s, n = sum(weight))}



  s <- dplyr::mutate(s, count_ = n, x_ = x)
  s <- dplyr::select(s, count_, x_)
  s <- dplyr::collect(s)

  s <- as.data.frame(s,  stringsAsFactors = FALSE)


  return(s)

}

#' @export
compute_boxplot.tbl_spark <- function(x, var = NULL, coef = 1.5){

  x_var <- as.character(var)[2]

  s <- dplyr::mutate_(x , x_var = var)
  s <- dplyr::summarise(s, min_ = percentile(x_var, 0),
              lower_ = percentile(x_var, 0.25),
              median_ = percentile(x_var, 0.50),
              upper_ = percentile(x_var, 0.75),
              max_ = percentile(x_var, 0.99))
  s<- dplyr::collect(s)
  s<- dplyr::mutate(s, outliers_ = list(numeric()))

  s <- as.data.frame(s,  stringsAsFactors = FALSE)

  groups <- dplyr::select(s, -min_, -lower_, -median_, -upper_, -max_, -outliers_)

  groups <- colnames(groups)

  s <- dplyr::group_by_(s, groups)
  s <- dplyr::arrange_(s, groups)

  return(s)

}

compute_boxplot_outliers.tbl_spark <- function(x) {


  groups <- dplyr::select(x, -min_, -lower_, -median_, -upper_, -max_, -outliers_)

  groups <- colnames(groups)

  outliers <- data.frame(x_var = character(), value_ = numeric())

  outliers <- data_frame(x_var = "", value_ = 0)

  colnames(outliers) <- c(groups, "value_")


  return(outliers)
  #outliers <- as.data.frame(outliers,  stringsAsFactors = FALSE)

}


