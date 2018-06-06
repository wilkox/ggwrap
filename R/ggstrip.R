# Make a plot
dev.off()
library(ggplot2)
library(grid)
library(tidyverse)

mpg %>%
  group_by(year, class) %>%
  summarise(displ = mean(displ)) %>%
  ungroup

plot <- ggplot(mpg, aes(x = displ, colour = class)) +
  geom_bar()

pgrob <- ggplotGrob(plot)

# Extract panel and axes
panel <- pgrob$grobs[[6]]
xaxis <- pgrob$grobs[[7]]
yaxis <- pgrob$grobs[[3]]

grid.newpage()

# Prepare panel and x axis grobs
panel_grob <- gTree(children = gList(panel), vp = "stripright::panel")
xaxis_grob <- gTree(children = gList(xaxis), vp = "stripright::xaxis")
yaxis_grob <- gTree(children = gList(yaxis), vp = "stripleft::yaxis")

# Prepare gtree for right hand part of strip
stripright_gtree <- gTree(
  name = "stripright_gtree",
  childrenvp = vpTree(
    viewport(layout = grid.layout(
      ncol = 1,
      nrow = 2,
      heights = c(0.8, 0.2),
    ), name = "stripright"),
    vpList(
      viewport(layout.pos.row = 1, layout.pos.col = 1, name = "panel"),
      viewport(layout.pos.row = 2, layout.pos.col = 1, name = "xaxis")
    )
  ),
  children = gList(panel_grob, xaxis_grob),
  layout.pos.row = 1,
  layout.pos.col = 2,
  vp = "strip::stripright"
)

# Prepare gtree for left hand part of the strip
stripleft_gtree <- gTree(
  name = "stripleft_gtree",
  childrenvp = vpTree(
    viewport(layout = grid.layout(
      ncol = 1,
      nrow = 2,
      heights = c(0.8, 0.2),
    ), name = "stripleft"),
    vpList(
      viewport(layout.pos.row = 1, layout.pos.col = 1, name = "yaxis")
    )
  ),
  children = gList(yaxis_grob),
  layout.pos.row = 1,
  layout.pos.col = 1,
  vp = "strip::stripleft"
)

# Prepare strip gtree
strip_gtree <- gTree(
  name = "strip_gtree",
  childrenvp = vpTree(
    viewport(layout = grid.layout(ncol = 2, nrow = 1, widths = c(0.2, 0.8)), name = "strip"),
    vpList(
      viewport(layout.pos.row = 1, layout.pos.col = 1, name = "stripleft"),
      viewport(layout.pos.row = 1, layout.pos.col = 2, name = "stripright")
    )
  ),
  children = gList(stripleft_gtree, stripright_gtree)
)

grid.draw(strip_gtree)

# Given a grob/gtree, return a gtree showing the selected portion of the grob
partialgrob <- panel
numerator <- 1
denominator <- 4

# Re-encapsulate the grob with the window as the parent vp
partialgrob <- gTree(children = gList(partialgrob), vp = "partialgrid::window")

# Create the grid layout
gridcols <- (2 * denominator) - 1
partialgrid <- viewport(
  layout = grid.layout(nrow = 1, ncol = gridcols),
  name = "partialgrid",
  width = gridcols
)

# Create the 'window' viewport
window <- viewport(
  name = "window",
  layout.pos.col = (denominator - numerator + 1):((2 * denominator) - numerator),
  layout.pos.row = 1
)

# Create the vptree, encapsulated in a gtree
partial_gtree <- gTree(
  name = "partial_gtree",
  chilrenvp = vpTree(
    partialgrid,
    vpList(window)
  ),
  children = gList(partialgrob)
)

grid.newpage()
grid.draw(partial_gtree)
