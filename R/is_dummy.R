is_dummy <- function(x) {
  # removing NA values is not necessary since any NA's are removed in
  # sdc_model() before is_dummy() is called

  # this if clause returns FALSE for list columns
  if (!is.atomic(x)) {
    return(FALSE)
  }
  # handling complex vectors specifically because they cause a problem in the
  # last if clause of this function
  # if (is.complex(x)) {
  #   return(FALSE)
  # }

  if (is.logical(x)) {
    return(TRUE)
  }

  if (is.factor(x)) {
    return(TRUE)
  }

  if (is.character(x)) {
    return(TRUE)
  }

  # Detection for 0/1 variables removed since these can be handled easier as
  # usual continuous variables

  return(FALSE)
}
