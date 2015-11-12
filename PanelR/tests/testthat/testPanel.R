print(getwd())
context("Reading Panels")

grun_dt <- fread('../../Data/grunfeld.csv')
grun_panel <- Panel$new(fread('../../Data/grunfeld.csv'), i = 'FIRM', t = 'YEAR')
# print(names(grun_dt))
print(grun_dt[, I])

test_that("panel attrs are correct on load", {
  expect_equal(grun_panel$i, 'FIRM')
  expect_equal(grun_panel$t, 'YEAR')
  expect_equal(grun_panel$T, 20)
  expect_equal(grun_panel$n, 10)
  expect_equal(grun_panel$N, 200)
  expect_equal(grun_panel$balanced, TRUE)
}
)
#
# test_that("methods work", {
#   print(grun_panel$within('I'))
#   print(grun_dt[, mean(I)])
#   expect_equal(as.numeric(grun_panel$within('I'))[['I']], as.numeric(grun_dt[, 'I' - mean('I'), by=FIRM, with = FALSE]))
# })
