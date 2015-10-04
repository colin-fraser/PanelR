require(data.table)
require(R6)

Panel <- R6Class("Panel",
                 public = list(
                   data = NA,
                   i = NA,
                   t = NA,
                   T = NA,
                   N = NA,
                   balanced = NA,
                   initialize = function(data, i, t) {
                     self$data <- data
                     self$i <- i
                     self$t <- t
                     self$N <- self$data[, length(unique(get(self$i)))]
                     self$T <- self$data[, length(unique(get(self$t)))]
                     self$balanced <- self$determine_balance()
                     setkeyv(self$data, c(self$i, self$t))
                   },
                   within = function(col) {
                     result <- self$data[, .(mean(get(col))), by=get(self$i)]
                     setnames(result, 'V1', colname)
                     result
                   },
                   determine_balance = function() {
                     observations <- self$data[, length(get(self$t)) == self$T, by=get(self$i)][, V1]
                     min(observations) == max(observations) # check if all equal
                   },
                   print = function() {
                     print(self$data)
                   }
                   
                 )
)

within <- function(dt, i, col) {
  dt[, mean(col), by=i]
}

dt <- data.table(person = rep(1:5, each=5), time = rep(seq(1:5)), data = rnorm(25))
panel <- Panel$new(dt, 'person', 'time')
panel$within('data')
panel$balanced
4
