# Given a grob/gtree, return a gtree showing the selected portion of the grob
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

# Given a ggplot2 plot, return a 'partial'ed strip
partial_strip <- function(plot, i, n) {

  # Turn plot into grob
  pgrob <- ggplotGrob(plot)

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
        heights = unit(c(1, 15), c("null", "points")),
        widths = unit(c(5.5, 1), c("point", "null"))
      ) , name = "strip"),
      grid::vpList(
        grid::viewport(layout.pos.row = 1, layout.pos.col = 2, name = "panel", clip = "on"),
        grid::viewport(layout.pos.row = 2, layout.pos.col = 2, name = "xaxis", clip = "on"),
        grid::viewport(layout.pos.row = 1, layout.pos.col = 1, name = "yaxis")
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

ggwrap <- function(plot, nstrips) {

  # Generate strips
  strips <- purrr::map(1:nstrips, ~ partial_strip(plot, .x, nstrips))
  strips <- purrr::map(1:nstrips, ~ grid::gTree(
    children = grid::gList(strips[[.x]]), vp = paste0("strips::strip", .x)
  ))

  # Generate a viewport for each strip
  strip_viewports <- purrr::map(1:nstrips, ~ grid::viewport(
    layout.pos.col = 1,
    layout.pos.row = .x,
    name = paste0("strip", .x)
  ))

  strips_gtree <- grid::gTree(
    childrenvp = grid::vpTree(
      grid::viewport(
        layout = grid::grid.layout(ncol = 1, nrow = nstrips),
        name = "strips"
      ),
      do.call(grid::vpList, strip_viewports)
    ),
    children = do.call(grid::gList, strips)
  )

  # Turn plot into a grob
  pgrob <- ggplotGrob(plot)

  # Replace panel area with strips
  pgrob$grobs[[6]] <- strips_gtree

  # Remove axes
  pgrob$grobs[[3]] <- ggplot2::zeroGrob()
  pgrob$grobs[[7]] <- ggplot2::zeroGrob()

  grid::grid.newpage()
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(),
    getNamespace("ggplot2")
  )
  grid::grid.draw(pgrob)
}
