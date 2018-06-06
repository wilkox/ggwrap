# Make a plot
library(ggplot2)
library(grid)
library(tidyverse)

# Prepare sample data
plot <- economics %>%
  ggplot(aes(x = date, y = unemploy, colour = pop)) +
  geom_point()

ggstrip(plot, 4)

# Given a grob/gtree, return a gtree showing the selected portion of the grob
partial_grob <- function(grob, i, n) {

  # Calculate the number of columns in the grid
  gridcols <- (2 * n) - 1

  # Re-encapsulate the grob with the window as the parent vp
  grob <- gTree(children = gList(grob), vp = "partial::window")

  # Create the gtree
  partial_gtree <- gTree(
    childrenvp = vpTree(
      viewport(
        layout = grid.layout(nrow = 1, ncol = gridcols),
        width = gridcols,
        name = "partial"
      ),
      vpList(viewport(
        layout.pos.col = (n - i + 1):((2 * n) - i),
        layout.pos.row = 1,
        name = "window"
      ))
    ),
    children = gList(grob)
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

  # Take partials of panel and x axis
  panel <- partial_grob(panel, i, n)
  xaxis <- partial_grob(xaxis, i, n)

  # Prepare panel and x axis grobs
  panel_grob <- gTree(children = gList(panel), vp = "strip::panel")
  xaxis_grob <- gTree(children = gList(xaxis), vp = "strip::xaxis")
  yaxis_grob <- gTree(children = gList(yaxis), vp = "strip::yaxis")

  # Prepare gtree for strip
  strip_gtree <- gTree(
    childrenvp = vpTree(
      viewport(layout = grid.layout(
        ncol = 2,
        nrow = 2,
        heights = unit(c(1, 15), c("null", "points")),
        widths = unit(c(5.5, 1), c("point", "null"))
      ) , name = "strip"),
      vpList(
        viewport(layout.pos.row = 1, layout.pos.col = 2, name = "panel", clip = "on"),
        viewport(layout.pos.row = 2, layout.pos.col = 2, name = "xaxis"),
        viewport(layout.pos.row = 1, layout.pos.col = 1, name = "yaxis")
      )
    ),
    children = gList(
      panel_grob,
      xaxis_grob,
      yaxis_grob,
      clipGrob(height = 1, width = 1, vp = "strip::panel")
    )
  )

  return(strip_gtree)
}

ggstrip <- function(plot, nstrips) {

  # Generate strips
  strips <- purrr::map(1:nstrips, ~ partial_strip(plot, .x, nstrips))
  strips <- purrr::map(1:nstrips, ~ gTree(
    children = gList(strips[[.x]]), vp = paste0("strips::strip", .x)
  ))

  # Generate a viewport for each strip
  strip_viewports <- purrr::map(1:nstrips, ~ viewport(
    layout.pos.col = 1,
    layout.pos.row = .x,
    name = paste0("strip", .x)
  ))

  strips_gtree <- gTree(
    childrenvp = vpTree(
      viewport(layout = grid.layout(ncol = 1, nrow = nstrips), name = "strips"),
      do.call(vpList, strip_viewports)
    ),
    children = do.call(gList, strips)
  )

  # Turn plot into a grob
  pgrob <- ggplotGrob(plot)

  # Replace panel area with strips
  pgrob$grobs[[6]] <- strips_gtree

  # Remove axes
  pgrob$grobs[[3]] <- zeroGrob()
  pgrob$grobs[[7]] <- zeroGrob()

  grid.draw(pgrob)
}
