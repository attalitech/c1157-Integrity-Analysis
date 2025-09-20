is_category <- function(x) {
  # Remove NAs first for efficiency, then check if all values are integers

  # If there are no na values, then it can't be a category
  if (sum(is.na(x)) == 0)
    return(FALSE)

  # If the vector is empty after removing NAs then it is not a category
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0)
    return(FALSE)

  # Check if all values are equal to their integer representation
  all(x_clean == as.integer(x_clean))
}
