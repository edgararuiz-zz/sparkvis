#' @export
compute_count.tbl_spark <- function(x, x_var, w_var = NULL) {

  plot_table <- dplyr::mutate_(x, "x" = x_var)
  plot_table <- dplyr::filter(plot_table, !is.na(x))
  plot_table <- dplyr::group_by(plot_table, x)

  if(is.null(w_var)){
    plot_table <- dplyr::tally(plot_table)
    }
  else
    {
    w <- as.character(w_var)[2]
    plot_table <- dplyr::mutate_(plot_table, weight = w)
    plot_table <- dplyr::summarise(plot_table, n = sum(weight))
    }

  plot_table <- dplyr::mutate(plot_table, count_ = n, x_ = x)
  plot_table <- dplyr::select(plot_table, count_, x_)
  plot_table <- dplyr::collect(plot_table)
  plot_table <- as.data.frame(plot_table, stringsAsFactors = FALSE)

}

#' @export
compute_bin.tbl_spark  <- function(x, x_var, w_var = NULL,
                                   width = NULL, center = NULL,
                                   boundary = NULL, closed = c("right", "left"),
                                   pad = FALSE, binwidth = NULL){

  data_prep <- dplyr::mutate_(x, "x_field" = x_var)
  data_prep <- dplyr::select(data_prep, x_field)
  data_prep <- dplyr::filter(data_prep, !is.na(x_field))
  data_prep <- dplyr::mutate(data_prep, x_field = as.double(x_field))

  data_prep <- dplyr::summarise(data_prep,
                        max_x = max(x_field),
                        min_x = min(x_field),
                        mean_x = mean(x_field))
  data_prep <- dplyr::collect(data_prep)


  width         <- ifelse(is.null(width), 1, width)
  bins          <- ceiling(( data_prep$max_x - data_prep$min_x) / width)
  center        <- ifelse(is.null(center), data_prep$mean_x , center)
  center_point  <- center - (width  / 2)
  left_bins     <- ceiling((center - data_prep$min_x) / width)
  left_value    <- center_point - (left_bins * width)
  new_bins      <- left_value + (c(0:(bins + 1)) * width)


  all_bins <- data.frame(
                           key_bin = 0:(length(new_bins) - 2),
                           bin = 1:(length(new_bins) - 1),
                           bin_ceiling = head(new_bins, - 1),
                           bin_floor = tail(new_bins, -1)
                         )



  plot_table <- sparklyr::ft_bucketizer(
                                          data_prep,
                                          input.col = "x_field",
                                          output.col = "key_bin",
                                          splits = new_bins
                                        )
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
  plot_table <- dplyr::mutate(
                                plot_table,
                                n = ifelse(!is.na(n), n, 0),
                                width = width
                              )
  plot_table <- dplyr::select(
                                plot_table,
                                count_ = n,
                                x_ = x,
                                xmin_ = bin_ceiling ,
                                xmax_ = bin_floor,
                                width_ = width
                              )

  plot_table <- as.data.frame(plot_table,  stringsAsFactors = FALSE)

}

#' @export
compute_boxplot.tbl_spark <- function(x, var = NULL, coef = 1.5){


  s <- dplyr::mutate_(x , "x_var" = var)
  s <- dplyr::summarise(
                          s,
                          min_ = percentile(x_var, 0),
                          lower_ = percentile(x_var, 0.25),
                          median_ = percentile(x_var, 0.50),
                          upper_ = percentile(x_var, 0.75),
                          max_ = percentile(x_var, 1),
                          max_raw = max(x_var),
                          min_raw = min(x_var)
                        )


  s <- dplyr::mutate(s, iqr = (upper_ - lower_) * 1.5)

  s <- dplyr::mutate(s,
                     min_iqr = lower_ - iqr,
                     max_iqr = upper_ + iqr
  )

  s <- dplyr::mutate(s,
                     max_ = ifelse(max_raw > max_iqr, max_iqr, max_),
                     min_ = ifelse(min_raw < min_iqr, min_iqr, min_)
  )

  groups <- dplyr::select(s, -min_, -lower_, -median_, -upper_, -max_, -iqr, -min_iqr, -max_iqr, -min_raw, -max_raw)
  groups <- colnames(groups)


  o <- dplyr::mutate_(s, "x_var" = groups)

  p <- dplyr::mutate_(x, "y_var" = groups)
  p <- dplyr::mutate_(p, "value" = var)
  p <- dplyr::ungroup(p)
  p <- dplyr::select(p, y_var, value)


  q <- dplyr::inner_join(o, p , by = c("x_var" = "y_var"))

  q_max <- dplyr::filter(q, value < max_iqr)

  q_min <- dplyr::filter(q, value > min_iqr)

  q_upper <- dplyr::group_by_(q_max, groups)
  q_upper <- dplyr::summarise(q_upper, new_upper = max(value))
  q_upper <- dplyr::ungroup(q_upper)
  q_upper <- dplyr::mutate_(q_upper, "x_var" = groups)
  q_upper <- dplyr::select(q_upper, x_var, new_upper)


  q_lower <- dplyr::group_by_(q_min, groups)
  q_lower <- dplyr::summarise(q_lower, new_lower = min(value))
  q_lower <- dplyr::ungroup(q_lower)
  q_lower <- dplyr::mutate_(q_lower, "x_var" = groups)
  q_lower <- dplyr::select(q_lower, x_var, new_lower)

  s <- dplyr::mutate_(s, "x_var" = groups)

  s <- dplyr::left_join(s, q_upper, by = "x_var")
  s <- dplyr::left_join(s, q_lower, by = "x_var")


  s <- dplyr::collect(s)

  s <- dplyr::mutate(s, max_ = ifelse(is.nan(new_upper), max_, new_upper))
  s <- dplyr::mutate(s, min_ = ifelse(is.nan(new_lower), min_, new_lower))

  s <- dplyr::select(s, -iqr, -min_iqr, -max_iqr, -min_raw, -max_raw, -new_lower, -new_upper,  -x_var)

  s <- as.data.frame(s,  stringsAsFactors = FALSE)


  s <- dplyr::mutate(s, outliers_ = list(numeric()))
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


}


