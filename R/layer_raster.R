#' @export

layer_raster <- function(vis, ..., res = 50, intersects = 0) {

  new_props <- ggvis:::merge_props(ggvis:::cur_props(vis), ggvis:::props(...))

  x_var <- ggvis:::find_prop_var(new_props, "x.update")
  vis <- ggvis:::set_scale_label(vis, "x", ggvis:::prop_label(ggvis:::cur_props(vis)$x.update))


  y_var <- ggvis:::find_prop_var(new_props, "y.update")
  vis <- ggvis:::set_scale_label(vis, "y", ggvis:::prop_label(ggvis:::cur_props(vis)$y.update))

  vis <- ggvis:::set_scale_label(vis, "fill", "Frequency")

  vis <- ggvis:::layer_f(vis, function(v) {
    v <- ggvis:::add_props(v, .props = new_props)
    v <- compute_raster(v, x_var, y_var, res, intersects)

    v <- layer_rects(v, x = ~x1_, x2 = ~x2_ , y = ~y1_, y2 = ~y2_, fill = ~count_)

    v
    })
    vis
  }


