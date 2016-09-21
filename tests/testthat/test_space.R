test_that("step.size works as expected",{
  expect_true(step.size(c(1,2,3))==1)
  expect_error(step.size(c(1,2,5)))
  expect_true(step.size(c(0.997,1,1.003))==0.003)
})

test_that("empty_space() works as expected",{
  expect_true(all(empty.space()$data==0))
  expect_true(all(empty.space()$x.steps==make.steps()$x))
  expect_true(all(empty.space()$y.steps==make.steps()$y))
  expect_true(all(empty.space()$z.steps==make.steps()$t))
  expect_true(step.size(empty.space()$z.steps)==make.steps()$tstep)
})

