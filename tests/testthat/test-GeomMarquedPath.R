library(ggplot2)

# NOTE: use `testthat::snapshot_review()` to review failed tests.

set.seed(2425)

test_that("geom_marqued_bezier works with straight lines", {

  d_straight <- data.frame(
    x =    c(0,  0),
    y =    c(0, -1),
    xend = c(1, -1),
    yend = c(0,  0),
    angle = 0
  )

  p_straight <- ggplot(d_straight) +
    geom_marqued_bezier(
      aes(
        x = x, y = y, xend = xend, yend = yend, angle = angle
      )
    )
  vdiffr::expect_doppelganger("p_straight", p_straight)

  p_straight_arrow <- ggplot(d_straight) +
    geom_marqued_bezier(
      aes(
        x = x, y = y,
        xend = xend, yend = yend,
        angle = angle,
        start_cap = circle(),
        end_cap = square()
      ),
      arrow = arrow(ends = "both")
    )
  vdiffr::expect_doppelganger("p_straight_arrow", p_straight_arrow)

})

test_that("geom_marqued_bezier works with curved lines", {

  d_curved <- data.frame(
    x =    c(0,  0, 1, -2),
    y =    c(0, -1, 2, -2),
    xend = c(1, -1, 2, -3),
    yend = c(0,  0, 3, -4),
    angle = c(30, 30, -45, -90),
    end_cap = circle(c(0, 0, 1, 0))
  )

  p_curved <- ggplot(d_curved) +
    geom_marqued_bezier(
      aes(
        x = x, y = y, xend = xend, yend = yend, angle = angle,
        end_cap = end_cap,
        label = angle,
        label_nudge = 0.1,
      ),
      arrow = arrow()
    )
  vdiffr::expect_doppelganger("p_curved", p_curved)

  p_curved_trick <- ggplot(d_curved) +
    geom_marqued_bezier(
      aes(
        x = x, y = y, xend = xend, yend = yend, angle = angle,
        end_cap = end_cap,
        label = angle,
        label_nudge = sign(angle) * 0.1,
      ),
      arrow = arrow()
    )
  vdiffr::expect_doppelganger("p_curved_trick", p_curved_trick)

})

test_that("geom_marqued_bezier understands strength", {

  d_strength <- data.frame(
    x =    c(0,  0),
    y =    c(0,  0),
    xend = c(-1, -10),
    yend = c(-1, -10),
    angle = c(30, 30),
    start_cap = circle(c(0.4, 0.4))
  )

  p_strength <- ggplot(d_strength) +
    geom_marqued_bezier(
      aes(
        x = x, y = y, xend = xend, yend = yend, angle = angle,
        start_cap = start_cap,
      ),
      arrow = arrow()
    )
  vdiffr::expect_doppelganger("p_strength", p_strength)

  p_strength_one <- ggplot(d_strength) +
    geom_marqued_bezier(
      aes(
        x = x, y = y, xend = xend, yend = yend, angle = angle,
        start_cap = start_cap,
        strength = 1,
      ),
      arrow = arrow()
    )
  vdiffr::expect_doppelganger("p_strength_one", p_strength_one)

  p_strength_ten <- ggplot(d_strength) +
    geom_marqued_bezier(
      aes(
        x = x, y = y, xend = xend, yend = yend, angle = angle,
        start_cap = start_cap,
        strength = 10,
      ),
      arrow = arrow()
    )
  vdiffr::expect_doppelganger("p_strength_ten", p_strength_ten)

})
