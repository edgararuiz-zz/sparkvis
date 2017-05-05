#' @export
prop_type.tbl_spark <- function(data, prop) {

  # 'tbl_spark' persists as S3 class even after the aggregate
  # is collected into R. The if-then statement allows the
  # regular prop_value function to be passed if the results are
  # no longer 'tbl_spark'.

  if("tbl_spark" %in% class(data))
  {
    # Passing top 6 rows to have vector_type() make the
    # variable type determination
    suppressMessages({
      top_rows <- dplyr::mutate_(data, "new_field" = prop$value)
      top_rows <- dplyr::select(top_rows, new_field)
      top_rows <- dplyr::collect(head(top_rows))
    })
    return(ggvis::vector_type(top_rows[[1]]))
  }else{
    ggvis::vector_type(ggvis:::prop_value(prop, data))
    }
}

#' @export
preserve_constants.tbl_spark  <- function(input, output) {

  # Figuring that only on the first pass will the
  # input will be 'tbl_spark', we just send it back
  # out untouched.  Since the 'tbl_spark' class persists
  # even if the input is now a data.frame, we then send
  # it through the regular preserve_constants() command

  if("tbl_spark" %in% class(input))
    {
      output
  }else{
      ggvis:::preserve_constants.data.frame(input, output)
    }
}

#' @export
eval_vector.tbl_spark <- function(x, f) {

  # Passing top 6 rows to have vector_type() make the
  # variable type determination

  suppressMessages({
    top_rows <- dplyr::mutate_(x, "new_field" = f)
    top_rows <- dplyr::select(top_rows,new_field)
    top_rows <- dplyr::collect(head(top_rows))
  })
  return(top_rows[[1]])
}

#' @export
apply_props.tbl_spark <- function(data, props) {

  # names() does not work with 'tbl_spark' type, the
  # colnames() function is used instead

  cols <- lapply(props, prop_value, data = data)
  colnames(cols) <- vapply(props, prop_label, character(1))
  quickdf(cols)
}

