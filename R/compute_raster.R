#' @export
compute_raster <- function(x, x_var, y_var, res = 50, intersects = 0){
  UseMethod("compute_raster")
}

#' @export
compute_raster.tbl_spark <- function(x, x_var, y_var, res = 50, intersects = 0){

  x_variable <- as.character(x_var)[2]

  y_variable <- as.character(y_var)[2]



  spark_table <- dplyr::mutate_(x, x_var = x_variable, y_var = y_variable)
  spark_table <- dplyr::filter(spark_table,
                               !is.na(x_var),
                               !is.na(y_var))
  spark_table <- dplyr::select(spark_table, x_var, y_var)
  spark_table <- dplyr::mutate(spark_table,
                               x_var = as.double(x_var),
                               y_var = as.double(y_var))


  limits <- dplyr::summarise(spark_table,
                             xmax = max(x_var),
                             xmin = min(x_var),
                             ymax = max(y_var),
                             ymin = min(y_var))
  limits <- dplyr::collect(limits)

  limits <- dplyr::mutate(limits,
                          x_max = xmax - xmin,
                          x_min = 0,
                          y_max = ymax - ymin,
                          y_min = 0)

  limits <- dplyr::mutate(limits,
                          x_size = (x_max - x_min) /res,
                          y_size = (y_max - y_min) /res)


  x_buckets <- (0:(res+1)) * limits$x_size[1]
  y_buckets <- (0:(res+1)) * limits$x_size[1]


  spark_table <- dplyr::mutate(spark_table,
                               x_var = x_var - limits$xmin[1],
                               y_var = y_var - limits$ymin[1])

  spark_table <-  sparklyr::ft_bucketizer(spark_table, "x_var", "x_assignment", x_buckets)
  spark_table <-  sparklyr::ft_bucketizer(spark_table, "y_var", "y_assignment", y_buckets)

  spark_table <- dplyr::select(spark_table,
                               x_assignment,
                               y_assignment)


  plot_table <- dplyr::group_by(spark_table, x_assignment, y_assignment)
  plot_table <- dplyr::tally(plot_table)
  plot_table <- dplyr::collect(plot_table)


  if(intersects ==  0){
    all_squares <- data.frame(x_assignment = as.numeric(paste0(sapply(1:res, function(x)rep(x, res)))),
                              y_assignment = rep(1:res, res))
    plot_table <- dplyr::right_join(plot_table, all_squares, by = c("x_assignment", "y_assignment"))
    plot_table <- dplyr::mutate(plot_table, n = ifelse(is.na(n), 0, n))
  } else {
    plot_table <- dplyr::filter(plot_table,
                                n >= intersects)
  }


  plot_table <- dplyr::mutate(plot_table,
                              x1_ = limits$xmin[1] + (limits$x_size * (x_assignment -1)),
                              x2_ = limits$xmin[1] + (limits$x_size * (x_assignment)),
                              y1_ = limits$ymin[1] + (limits$y_size * (y_assignment -1)),
                              y2_ = limits$ymin[1] + (limits$y_size * (y_assignment)),
                              count_ = n)

  plot_table <- dplyr::ungroup(plot_table)
  plot_table <- dplyr::select(plot_table,
                              x1_, x2_, y1_, y2_, count_)

  plot_table <- as.data.frame(plot_table,  stringsAsFactors = FALSE)

  return(plot_table)


}


#' @export
compute_raster.ggvis <- function(x, x_var, y_var, res = 50, intersects = 0) {
  args <- list(x_var = x_var,
               y_var = y_var,
               res = res,
               intersects = intersects)

  ggvis:::register_computation(x, args, "rect", function(data, args) {
    output <- ggvis:::do_call(compute_raster, quote(data), .args = args)
    ggvis:::preserve_constants(data, output)
  })
}
