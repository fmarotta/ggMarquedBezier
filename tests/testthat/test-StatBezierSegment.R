test_that("StatBezierSegment computes bezier paths correctly", {
  data <- data.frame(
    x = 0,
    y = 0,
    xend = 1,
    yend = 0,
    angle = c(0, 30, 45, 90),
    strength = 1
  )
  output <- StatBezierSegment$compute_panel(data, scales = NULL, n = 3)
  expect_equal(unique(output$group), 1:4)
  g1 <- output[output$group == 1, ]
  expect_equal(g1$x, c(0, 0.5, 1))
  expect_equal(g1$y, c(0, 0, 0))
  g2 <- output[output$group == 2, ]
  expect_equal(g2$x, c(0, 0.5, 1))
  expect_true(all(g2$y >= g1$y))
  expect_true(g2[2, ]$y < sin(30 * pi / 180))
  g3 <- output[output$group == 3, ]
  expect_equal(g3$x, c(0, 0.5, 1))
  expect_true(all(g3$y >= g2$y))
  expect_true(g3[2, ]$y < sin(45 * pi / 180))
  g4 <- output[output$group == 4, ]
  expect_equal(g4$x, c(0, 0.5, 1))
  expect_true(all(g4$y >= g3$y))
})
