test_that("test-relationships-to-index", {

  rel <- data.frame(fromedge = c(2, 3, 4, 9, 14, 15), toedge = c(1, 2, 2, 10, 10, 14))
  n_index <- relationships_to_index(rel)
  us <- sort(n_index$usrid[n_index$rid == 1])
  expect_true(all(us == c(1, 2, 3, 4)))

  us <- sort(n_index$usrid[n_index$rid == 10])
  expect_true(all(us == c(9, 10, 14, 15)))


  rel <- data.frame(fromedge = c(3, 3, 3, 3), toedge = c(1, 1, 1, 1))
  n_index <- relationships_to_index(rel)
  us <- sort(n_index$usrid[n_index$rid == 1])
  expect_true(all(us == c(1, 3)))


  rel <- data.frame(fromedge = c(2, 3, 4, 5, 6), toedge = c(1, 2, 3, 4, 5))
  n_index <- relationships_to_index(rel)
  us <- sort(n_index$usrid[n_index$rid == 1])
  expect_true(all(us == c(1, 2, 3, 4, 5, 6)))

  us <- sort(n_index$usrid[n_index$rid == 5])
  expect_true(all(us == c(5, 6)))




})
