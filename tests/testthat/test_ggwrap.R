context("ggwrap")

test_that("ggwrap runs without error", {
  plot <- ggplot2::ggplot(
    ggplot2::economics,
    ggplot2::aes(x = date, y = unemploy)
  ) + 
    ggplot2::geom_line()
  expect_silent(ggwrap(plot, 4))
  expect_silent(ggwrap(plot, 40))
})
