% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_df.R
\name{unscale_df}
\alias{unscale_df}
\title{Unscale a data.frame returning a data.frame}
\usage{
unscale_df(x, by = NULL)
}
\arguments{
\item{x}{A data.frame as input, possibly including scaling information}

\item{by}{A numeric vector}
}
\value{
a data.frame of the same dimensions as the input with numeric columns
  scaled and with scaling attributes removed.
}
\description{
\code{unscale_df} returns a data.frame where each of the columns is unscaled by a
constant or from information in the data.frame.
}
\examples{
df <- data.frame(x=1:5,y=6:10,z=factor(letters[1:5]))
\dontrun{
scale(df, center = FALSE, scale = apply(x, 2, sd, na.rm = TRUE)) # an error because of factor
}
scaled_df <- scale_df(df)
attributes(scaled_df)
unscaled_df <- unscale_df(scaled_df)

}
\seealso{
\code{\link[base]{scale}}, \code{\link[arm]{rescale}}, and
  \code{\link[arm]{standardize}}.
  \href{https://stevencarlislewalker.wordpress.com/2012/09/24/a-simpler-version-of-the-r-scale-command/}{Here's a different reason} to replace \code{scale()}
}

