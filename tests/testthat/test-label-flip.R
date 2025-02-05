test_that("Labels be flipped", {

  n_sides <- 71
  angles <- seq(0, 2*pi*(1-1/n_sides), length = n_sides)
  d <- data.frame(
    x = 0,
    y = 0,
    xend = cos(angles),
    yend = sin(angles),
    angle = angles
  )

  ggplot(d) +
    geom_marqued_bezier(
      aes(
        x,
        y,
        xend = xend,
        yend = yend,
        label = round(angle, digits = 2),
        start_cap = circle(5, "mm")
      ),
      arrow = arrow()
    )

  d_bomb <- data.frame(
    x = c(0, 0),
    y = c(0, 1),
    xend = c(0, 0),
    yend = c(1, 0)
  )

  ggplot(d_bomb) +
    geom_marqued_bezier(
      aes(
        x,
        y,
        xend = xend,
        yend = yend,
        angle = 5,
        label = "meow5",
        start_cap = circle(5, "mm"),
        end_cap = circle(5, "mm")
      ),
      arrow = arrow()
    ) +
    geom_marqued_bezier(
      aes(
        x,
        y,
        xend = xend,
        yend = yend,
        angle = 15,
        label = "meow15",
        start_cap = circle(5, "mm"),
        end_cap = circle(5, "mm")
      ),
      arrow = arrow()
    )

})
