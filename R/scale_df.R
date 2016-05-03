#' Scale a data.frame returning a data.frame
#'
#' \code{scale_df} returns a data.frame where each of the columns is scaled by a
#' constant.
#'
#' @param x A data.frame as input
#' @param by A character vector, numeric vector, or data.frame with scaling
#'   information
#'
#' @return a data.frame of the same dimensions as the input with numeric columns
#'   scaled and with additional attributes describing the scaling carried out.
#'
#' @seealso \code{\link[base]{scale}}, \code{\link[arm]{rescale}}, and
#'   \code{\link[arm]{standardize}}.
#'   \href{https://stevencarlislewalker.wordpress.com/2012/09/24/a-simpler-version-of-the-r-scale-command/}{Here's a different reason} to replace \code{scale()}
#'
#' @examples
#' df <- data.frame(x=1:5,y=6:10,z=factor(letters[1:5]))
#' \dontrun{
#' scale(df, center = FALSE, scale = apply(x, 2, sd, na.rm = TRUE)) # an error because of factor
#' }
#' scaled_df <- scale_df(df)
#' attributes(scaled_df)
#' @export
scale_df <- function(x, by = sd) {

  if (!is.data.frame(x))
    stop("x must be a data.frame")

  which_cols <- vapply(x, FUN = is.numeric, FUN.VALUE = TRUE)

  scale_by <- switch(typeof(by), character = "functions",
                     numeric = "constant", list = "attribute",
                     closure = "function")

  if (scale_by == "function") {
    scales <- numeric(ncol(x))
    scales[which_cols] <- vapply(x[, which_cols],
                                by, numeric(1), na.rm = TRUE)
    scales[!which_cols] <- NA
  } else {
    stop("scaling by functions, constants and attributes not yet implemented")
  }

  # iterate over data.frame, replacing columns
  for (i in seq_along(x)) {
    if (!is.na(scales[i])) {
      x[[i]] <- x[[i]]/scales[i]
    }
  }
  attr(x, which = "scales") <- scales
  x
}

#' Center numeric variables in a data.frame returning a data.frame
#'
#' \code{center_df} returns a data.frame where each of the columns
#' is centered by a constant.
#'
#' @param x A data.frame as input
#' @param by A character vector, numeric vector, or data.frame with scaling information
#'
#' @return a data.frame of the same dimensions as the input with numeric
#'   columns centered and with additional attributes describing the
#'   scaling carried out.
#'
#' @seealso \code{\link[base]{scale}}, \code{\link[arm]{rescale}}, and
#'   \code{\link[arm]{standardize}}.
#'   \href{https://stevencarlislewalker.wordpress.com/2012/09/24/a-simpler-version-of-the-r-scale-command/}{Here's a different reason} to replace \code{scale()}
#'
#' @examples
#' df <- data.frame(x=1:5,y=6:10,z=factor(letters[1:5]))
#' \dontrun{
#' scale(df, center = TRUE, scale = FALSE) # an error because of factor
#' }
#' centered_df <- center_df(df)
#' attributes(centered_df)
#' @export
center_df <- function(x, by = mean) {

  if (!is.data.frame(x))
    stop("x must be a data.frame")

  which_cols <- vapply(x, FUN = is.numeric, FUN.VALUE = TRUE)

  center_by <- switch(typeof(by), character = "functions",
                      numeric = "constant", list = "attribute",
                      closure = "function")

  if (center_by == "function") {
    centers <- numeric(ncol(x))
    centers[which_cols] <- vapply(x[, which_cols],
                                  by, numeric(1), na.rm = TRUE)
    centers[!which_cols] <- NA
  } else {
    stop("centering by functions, constants and attributes not yet implemented")
  }

  # iterate over data.frame, replacing columns
  for (i in seq_along(x)) {
    if (!is.na(centers[i])) {
      x[[i]] <- x[[i]] - centers[i]
    }
  }
  attr(x, which = "centers") <- centers
  x
}

#' Unscale a data.frame returning a data.frame
#'
#' \code{unscale_df} returns a data.frame where each of the columns is unscaled by a
#' constant or from information in the data.frame.
#'
#' @param x A data.frame as input, possibly including scaling information
#' @param by A numeric vector
#'
#' @return a data.frame of the same dimensions as the input with numeric columns
#'   scaled and with scaling attributes removed.
#'
#' @seealso \code{\link[base]{scale}}, \code{\link[arm]{rescale}}, and
#'   \code{\link[arm]{standardize}}.
#'   \href{https://stevencarlislewalker.wordpress.com/2012/09/24/a-simpler-version-of-the-r-scale-command/}{Here's a different reason} to replace \code{scale()}
#'
#' @examples
#' df <- data.frame(x=1:5,y=6:10,z=factor(letters[1:5]))
#' \dontrun{
#' scale(df, center = FALSE, scale = apply(x, 2, sd, na.rm = TRUE)) # an error because of factor
#' }
#' scaled_df <- scale_df(df)
#' attributes(scaled_df)
#' unscaled_df <- unscale_df(scaled_df)
#'
#' @export
unscale_df <- function(x, by = NULL) {

  if (!is.data.frame(x))
    stop("x must be a data.frame")
  scales <- attr(x, "scales")
  if (is.null(scales) & is.null(by)) {
    stop("x must be scaled or by must not be NULL")
  }

  if (!is.null(by)){
    if (length(by) == ncol(x)){
      scales = by
    } else {
      scales <- numeric(ncol(x))
      scales[] <- by ## this will trigger a warning if it doesn't match
    }
  }

  # figure out which columns to skip
  which_cols <- vapply(x, FUN = is.numeric, FUN.VALUE = TRUE)

  # iterate over data.frame, replacing columns
  for (i in seq_along(x)) {
    if (!is.na(scales[i])) {
      x[[i]] <- x[[i]]*scales[i]
    }
  }
  attr(x, which = "scales") <- NULL
  x
}

#' Uncenter numeric variables in a data.frame returning a data.frame
#'
#' \code{uncenter_df} returns a data.frame where each of the columns
#' has been unentered by a constant.
#'
#' @param x A data.frame as input, possibly including centering information
#' @param by A numeric vector
#'
#' @return a data.frame of the same dimensions as the input with numeric
#'   columns uncentered. Centering attributes are removed.
#'
#' @seealso \code{\link[base]{scale}}, \code{\link[arm]{rescale}}, and
#'   \code{\link[arm]{standardize}}.
#'   \href{https://stevencarlislewalker.wordpress.com/2012/09/24/a-simpler-version-of-the-r-scale-command/}{Here's a different reason} to replace \code{scale()}
#'
#' @examples
#' df <- data.frame(x=1:5,y=6:10,z=factor(letters[1:5]))
#' \dontrun{
#' scale(df, center = TRUE, scale = FALSE) # an error because of factor
#' }
#' centered_df <- center_df(df)
#' attributes(centered_df)
#' uncentered_df <- uncenter_df(centered_df)
#' all.equal(uncentered_df, df)
#' @export
uncenter_df <- function(x, by = NULL) {

  if (!is.data.frame(x))
    stop("x must be a data.frame")
  centers <- attr(x, "centers")
  if (is.null(centers) & is.null(by)) {
    stop("x must be centered or by must not be NULL")
  }

  if (!is.null(by)){
    if (length(by) == ncol(x)){
      centers = by
    } else {
      centers <- numeric(ncol(x))
      centers[] <- by ## this will trigger a warning if it doesn't match
    }
  }

  # figure out which columns to skip
  which_cols <- vapply(x, FUN = is.numeric, FUN.VALUE = TRUE)
  # this should probably check if all columns are non-numeric
  centers[!which_cols] <- NA

  # iterate over data.frame, replacing columns
  for (i in seq_along(x)) {
    if (!is.na(centers[i])) {
      x[[i]] <- x[[i]] + centers[i]
    }
  }
  attr(x, which = "centers") <- NULL
  x
}
