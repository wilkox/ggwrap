#' Wrap a 'gglot2' plot over multiple rows.
#'
#' @return Returns a plot object which can be viewed or saved to file with
#' `ggplot2::ggsave`.
#'
#' @param plot A 'ggplot2' plot to be wrapped.
#' @param n The number of rows in the wrapped plot.
#'
#' @examples
#' plot <- ggplot2::ggplot(
#'   ggplot2::economics,
#'   ggplot2::aes(x = date, y = unemploy, colour = uempmed)
#' ) +
#'   ggplot2::geom_line()
#' plot <- ggwrap(plot, 4)
#' @export
ggwrap <- function(plot, n) {

  # Generate strips
  strips <- purrr::map(1:n, ~ partial_strip(plot, .x, n))
  strips <- purrr::map(1:n, ~ grid::gTree(
    children = grid::gList(strips[[.x]]), vp = paste0("strips::strip", .x)
  ))

  # Generate a viewport for each strip
  strip_viewports <- purrr::map(1:n, ~ grid::viewport(
    layout.pos.col = 1,
    layout.pos.row = .x,
    name = paste0("strip", .x)
  ))

  strips_gtree <- grid::gTree(
    childrenvp = grid::vpTree(
      grid::viewport(
        layout = grid::grid.layout(ncol = 1, nrow = n),
        name = "strips"
      ),
      do.call(grid::vpList, strip_viewports)
    ),
    children = do.call(grid::gList, strips)
  )

  # Turn plot into a grob
  pgrob <- ggplot2::ggplotGrob(plot)

  # Replace panel area with strips
  pgrob$grobs[[6]] <- strips_gtree

  # Remove axes
  pgrob$grobs[[3]] <- ggplot2::zeroGrob()
  pgrob$grobs[[7]] <- ggplot2::zeroGrob()

  # Add 'ggwrap' class
  class(pgrob) <- c("ggwrap", class(pgrob))

  # Return
  pgrob
}

#' @export
print.ggwrap <- function(x, ...) {

  grid::grid.newpage()

  # Record dependency on 'ggplot2' on the display list
  # (AFTER grid.newpage())
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(),
    getNamespace("ggplot2")
  )

  grid::grid.draw(x)

  invisible(x)
}

# Select a horizontal slice of a grob. Not exported.
partial_grob <- function(grob, i, n) {

  # Calculate the number of columns in the grid
  gridcols <- (2 * n) - 1

  # Re-encapsulate the grob with the window as the parent vp
  grob <- grid::gTree(
    children = grid::gList(grob),
    vp = "partial::window"
  )

  # Create the gtree
  partial_gtree <- grid::gTree(
    childrenvp = grid::vpTree(
      grid::viewport(
        layout = grid::grid.layout(nrow = 1, ncol = gridcols),
        width = gridcols,
        name = "partial"
      ),
      grid::vpList(grid::viewport(
        layout.pos.col = (n - i + 1):((2 * n) - i),
        layout.pos.row = 1,
        name = "window"
      ))
    ),
    children = grid::gList(grob)
  )

  return(partial_gtree)
}

# Draw a 'strip' of a 'ggplot2' plot containing only a horizontal slice of the
# plot. Not exported.
partial_strip <- function(plot, i, n) {

  # Turn plot into grob
  pgrob <- ggplot2::ggplotGrob(plot)

  # Extract panel and axes
  panel <- pgrob$grobs[[6]]
  xaxis <- pgrob$grobs[[7]]
  yaxis <- pgrob$grobs[[3]]

  # The x axis TableGrob sneakily tries to tell the x axis not to clip to its
  # parent viewport
  xaxis$children$axis$layout$clip <- c("inherit", "inherit")

  # Take partials of panel and x axis
  panel <- partial_grob(panel, i, n)
  xaxis <- partial_grob(xaxis, i, n)

  # Prepare panel and x axis grobs
  panel_grob <- grid::gTree(children = grid::gList(panel), vp = "strip::panel")
  xaxis_grob <- grid::gTree(children = grid::gList(xaxis), vp = "strip::xaxis")
  yaxis_grob <- grid::gTree(children = grid::gList(yaxis), vp = "strip::yaxis")

  # Prepare gtree for strip
  strip_gtree <- grid::gTree(
    childrenvp = grid::vpTree(
      grid::viewport(layout = grid::grid.layout(
        ncol = 2,
        nrow = 2,
        heights = grid::unit(c(1, 15), c("null", "points")),
        widths = grid::unit(c(5.5, 1), c("point", "null"))
      ) , name = "strip"),
      grid::vpList(
        grid::viewport(
          layout.pos.row = 1,
          layout.pos.col = 2,
          name = "panel",
          clip = "on"
        ),
        grid::viewport(
          layout.pos.row = 2,
          layout.pos.col = 2,
          name = "xaxis",
          clip = "on"
        ),
        grid::viewport(
          layout.pos.row = 1,
          layout.pos.col = 1,
          name = "yaxis"
        )
      )
    ),
    children = grid::gList(
      panel_grob,
      xaxis_grob,
      yaxis_grob
    )
  )

  return(strip_gtree)
}

