library(data.table)
library(lmtest)
library(sandwich)
library(R6)

Panel <- R6Class("Panel",
                 public = list(
                   data = NA,
                   i = NA,
                   t = NA,
                   T = NA,
                   n = NA,
                   N = NA,
                   first_obs = NA,
                   last_obs = NA,
                   initialize = function(data, i, t) {
                     self$data <- data
                     self$i <- i
                     self$t <- t
                     private$set_panel_attrs()
                     setkeyv(self$data, c(self$i, self$t))
                   },
                   within = function(cols) {
                     result <- self$data[, c(self$i, self$t, cols), with=FALSE]
                     for (col in cols) {
                       rep <- result[, get(col) - mean(get(col)), by=get(self$i)]$V1
                       set(result, j=col, value=rep)
                     }
                     result[, cols, with=FALSE]
                   },
                   print = function() {
                     print(self$data)
                   },
                   trim = function(start_date, end_date) {
                     # drops observations outside of [start_date, end_date]
                     indices <- self$data[, .(tf = get(self$t) >= start_date & get(self$t) <= end_date)]
                     self$data <- self$data[indices$tf]
                     private$set_panel_attrs()
                   },
                   force_balance = function() {
                     # drops subjects with fewer than self$T observations
                     self$data[, count := length(get(self$t)), by=get(self$i)]
                     self$data <- self$data[count == self$T]
                     self$data[, count := NULL]
                     private$set_panel_attrs()
                   },
                   xtreg = function(dependent, independent, type='fe') {
                     f <- as.formula(paste(dependent, '~ 0 +', paste(independent, collapse=' + ')))
                     print(f)
                     
                     l <- lm(f, data=self$within(c(dependent, independent)))
                     l$df.residual <- l$df.residual - self$n
                     l
                   },
                   update_panel = function() {
                     private$set_panel_attrs()
                   }
                 ),
                 active = list(
                   balanced = function() {
                     observations <- self$data[, length(get(self$t)) == self$T, by=get(self$i)][, V1]
                     min(observations) == max(observations) # check if all equal
                   },
                   columns = function() {
                     names(self$data)
                   },
                   panel_summary = function() {
                     details <- character()
                     details[1] <- ifelse(self$balanced, 'Balanced panel.', 'Unbalanced panel.')
                     details[2] <- paste('n =', self$n)
                     details[3] <- paste('T =', self$T)
                     details[4] <- paste('N =', self$N)
                     details[5] <- paste('First observation:', self$first_obs)
                     details[6] <- paste('Last observation:', self$last_obs)
                     paste(details, collapse = '\n')
                   }
                 ),
                 private = list(
                   set_panel_attrs = function() {
                     n <- self$data[, length(unique(get(self$i)))]
                     T <- self$data[, length(unique(get(self$t)))]
                     N <- dim(self$data)[1]
                     first_obs <- self$data[, min(get(self$t))]
                     last_obs <- self$data[, max(get(self$t))]
                     if (!is.na(self$N) & self$N != N) {
                       warning(
                         paste(self$N - N, 'of', self$N,
                                     paste('(',round(100 * (1 -  N / self$N), 2),'%)', sep=''), 
                                     'observations dropped'
                               )
                       )
                     }
                     self$n <- n
                     self$T <- T
                     self$N <- N
                     self$first_obs <- first_obs
                     self$last_obs <- last_obs
                   }
                 )
)



`[.Panel` <- function(x, ...) {
  x$data[...]
  x$set_panel_attrs()
}

get_CL_vcov<-function(model, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  
  #calculate degree of freedom adjustment
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate the uj's
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum))
  
  #use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}
