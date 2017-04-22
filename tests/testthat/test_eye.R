data(motData)

test_that("Test that as.eye function works", {
  eye1 <- df.eye %>% filter(id == 1, trial == 7) %>% as.eye()
  
  expect_true(is.valid.eye(eye1))
  expect_error(df.eye %>% filter(id == 1) %>% as.eye())
  
})

eye1 <- df.eye %>% filter(id == 1, trial == 7) %>% as.eye()

test_that("Test that print.eye function works", {
  expect_output(print(eye1))
})

test_that("Test that plot.eye function works", {
  expect_output(print(eye1))
})

test_that("Test that load.eye and save.eye functions work", {
  expect_silent(save.eye(eye1, eye.dir = tempdir()))
  eyex <- load.eye(id = eye1$id, tr = eye1$trial, eye.dir = tempdir())
  expect_equal(eye1,eyex)
})

test_that("Test that save.all.eye function work", {
  expect_silent(save.all.eye(df.eye, eye.dir = tempdir(), verbose = F))
  
})

test_that("Test that describe.eye function work", {
  dfx <- describe.eye(eye1)
  dist.between <- function(x, y) {
    return(sqrt(diff(x) ^ 2 + diff(y) ^ 2))
  }
  expect_equal(dist.between(c(1,2,5),c(1,1,5)),c(1,5))
  expect_equal(dist.between(c(1,2,3),c(1,1,1)),c(1,1))
  
  cha <- function(x, y){
    chull(x, y) -> i
    return(splancs::areapl(cbind(x[i],y[i])))
  }
  expect_equal(dfx$mean.x, mean(eye1$xyt$x))
  expect_equal(dfx$mean.y, mean(eye1$xyt$y))
  expect_equal(dfx$sd.x, sd(eye1$xyt$x))
  expect_equal(dfx$sd.y, sd(eye1$xyt$y))
  expect_equal(dfx$max.x, max(eye1$xyt$x))
  expect_equal(dfx$max.y, max(eye1$xyt$y))
  expect_equal(dfx$min.x, min(eye1$xyt$x))
  expect_equal(dfx$min.y, min(eye1$xyt$y))
  expect_equal(dfx$conv.hull, cha(eye1$xyt$x, eye1$xyt$y))
  expect_equal(dfx$mean.difference, mean(dist.between(eye1$xyt$x, eye1$xyt$y)))
  expect_equal(dfx$sd.difference, sd(dist.between(eye1$xyt$x, eye1$xyt$y)))
  expect_equal(dfx$total.length, sum(dist.between(eye1$xyt$x, eye1$xyt$y)))
  
  
})

test_that("Test that move.eye function work", {
  eyex <- move.eye(eye1, 1, 2)
  expect_equal(eye1$id, eyex$id)
  expect_equal(eye1$trial, eyex$trial)
  expect_equal(eyex$xyt$x, eye1$xyt$x + 1)
  expect_equal(eyex$xyt$y, eye1$xyt$y + 2)
  expect_equal(eye1, move.eye(eyex, -1, -2))
})

