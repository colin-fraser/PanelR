library(data.table)
library(R6)

Panel <- R6Class("Panel",
                 public = list(
                   data = NA,
                   i = NA,
                   t = NA,
                   T = NA,
                   n = NA,
                   N = NA,
                   initialize = function(data, i, t) {
                     self$data <- data
                     self$i <- i
                     self$t <- t
                     private$set_panel_attrs()
                     setkeyv(self$data, c(self$i, self$t))
                   },
                   within = function(col) {
                     result <- self$data[, .(mean(get(col))), by=get(self$i)]
                     setnames(result, 'V1', colname)
                     result
                   },
                   print = function() {
                     print(self$data)
                   },
                   trim = function(start_date, end_date, verbose = T) {
                     # drops observations outside of [start_date, end_date]
                     indices <- self$data[, .(tf = get(self$t) >= start_date & get(self$t) <= end_date)]
                     self$data <- self$data[indices$tf]
                     if (verbose) {
                       dropped <- sum(!indices$tf)
                       total <- sum(indices$tf) + sum(!indices$tf)
                       warning(paste(sum(!indices$tf), 'of', sum(indices$tf) + sum(!indices$tf),
                                     paste('(',round(100 * dropped / total, 2),'%)', sep=''), 
                                     'observations dropped'))
                     }
                     private$set_panel_attrs()
                   },
                 ),
                 active = list(
                   balanced = function() {
                     observations <- self$data[, length(get(self$t)) == self$T, by=get(self$i)][, V1]
                     min(observations) == max(observations) # check if all equal
                   }
                 ),
                 private = list(
                   set_panel_attrs = function() {
                     self$n <- self$data[, length(unique(get(self$i)))]
                     self$T <- self$data[, length(unique(get(self$t)))]
                     self$N <- dim(self$data)[1]
                   }
                 )
)
