% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggwrap.R
\name{ggwrap}
\alias{ggwrap}
\title{Wrap a 'gglot2' plot over multiple rows.}
\usage{
ggwrap(plot, n)
}
\arguments{
\item{plot}{A 'ggplot2' plot to be wrapped.}

\item{n}{The number of rows in the wrapped plot.}
}
\value{
Returns a plot object which can be viewed or saved to file with
`ggplot2::ggsave`.
}
\description{
Wrap a 'gglot2' plot over multiple rows.
}
\examples{
plot <- ggplot2::ggplot(
  ggplot2::economics,
  ggplot2::aes(x = date, y = unemploy, colour = uempmed)
) +
  ggplot2::geom_line()
plot <- ggwrap(plot, 4)
}
