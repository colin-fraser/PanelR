library(data.table)
library(multiwayvcov)
library(lmtest)
library(R6)



Panel <- R6Class("Panel",
                 public = list(

                   data = NA,
                   i = NA,
                   t = NA,
                   T = NA,
                   n = NA,
                   N = NA,
                   individual_index = NA,
                   time_index = NA,
                   first_obs = NA,
                   last_obs = NA,

                   initialize = function(data, i, t) {
                     self$data <- data
                     self$i <- i
                     self$t <- t
                     self$individual_index <- self$data[, get(self$i)]
                     self$time_index <- self$data[, get(self$t)]
                     private$set_panel_attrs()
                     setkeyv(self$data, c(self$i, self$t))
                   },

                   within = function(cols) {
                     result <- self$data[, c(self$i, self$t, cols), with = FALSE]
                     d <- function(x) x - mean(x) # demean function

                     for (col in cols) {
                       rep <- result[, d(get(col)), by = get(self$i)]$V1
                       set(result, j = col, value = rep)
                     }
                     result[, cols, with = FALSE]
                   },

                   print = function() {
                     print(self$data)
                   },

                   trim = function(start_date, end_date) {
                     # drops observations outside of [start_date, end_date]
                     indices <- self$data[, .(tf = get(self$t) >= start_date &
                                                get(self$t) <= end_date)]
                     self$data <- self$data[indices$tf]
                     private$set_panel_attrs()
                   },

                   force_balance = function() {
                     # drops subjects with fewer than self$T observations

                     self$data[, count := length(get(self$t)), by = get(self$i)]
                     self$data <- self$data[count == self$T]
                     self$data[, count := NULL]
                     private$set_panel_attrs()
                   },

                   xtreg = function(f, type = c('pooled', 'fe'),
                                    se = c('pooled', 'cluster', 'robust'),
                                    cluster = FALSE,
                                    vcov_type = c('stata_robust', 'HC0', 'HC1',
                                                  'HC2', 'HC3', 'HC4')) {

                     type <- match.arg(type)
                     se <- match.arg(se)
                     vcov_type <- match.arg(vcov_type)

                     f <- formula(f)
                     return(PanelRegression$new(formula = f, panel = self,
                                                regression_type = type,
                                                std_err_type = se,
                                                cluster = cluster,
                                                vcov_type = vcov_type))
                   },

                   update_panel = function() {
                     private$set_panel_attrs()
                   },

                   to_dta = function(filename) {
                     haven::write_dta(self$data, filename)
                   },
                   panel_summary = function(file="") {
                     s <- paste(private$panel_summary_list(), collapse = '\n')
                     cat(s, file = file)
                   }
                 ),
                 active = list(
                   balanced = function() {
                     observations <- self$data[, length(get(self$t)) == self$T,
                                               by = get(self$i)][, V1]
                     min(observations) == max(observations) # check if all equal
                   },
                   columns = function() {
                     names(self$data)
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
                         paste(
                           self$N - N, 'of', self$N, paste('(',round(
                             100 * (1 -  N / self$N), 2),'%)', sep = ''),
                           'observations dropped')
                       )
                     }
                     self$n <- n
                     self$T <- T
                     self$N <- N
                     self$first_obs <- first_obs
                     self$last_obs <- last_obs
                     return(self)
                   },
                   panel_summary_list = function() {
                     details <- character()
                     details[1] <- ifelse(self$balanced, 'Balanced panel.', 'Unbalanced panel.')
                     details[2] <- paste('n =', self$n)
                     details[3] <- paste('T =', self$T)
                     details[4] <- paste('N =', self$N)
                     details[5] <- paste('First observation:', self$first_obs)
                     details[6] <- paste('Last observation:', self$last_obs)
                     details
                   }
                 )
)

PanelRegression <- R6Class(
  classname = 'PanelRegression',
  public = list(
    regression_type = NA,
    panel = NA,
    formula = NA,
    std_err_type = NA,
    vcov_type = NA,
    model = NA,
    vcov = NA,
    t_test = NA,
    initialize = function(formula, panel,
                          regression_type = c('pooled', 'fe', 're'),
                          std_err_type = c('pooled', 'robust'),
                          cluster = FALSE,
                          vcov_type = c('pooled', 'stata_robust', 'HC0', 'HC1',
                                        'HC2', 'HC3', 'HC4')) {

      regression_type <- match.arg(regression_type)
      std_err_type <- match.arg(std_err_type)
      vcov_type <- match.arg(vcov_type)

      self$regression_type <- regression_type
      self$panel <- panel
      self$formula <- formula
      self$vcov_type <- vcov_type
      self$std_err_type <- std_err_type
      self$xtreg(formula, panel, regression_type, std_err_type, cluster,
                 vcov_type)
    },

    xtreg = function(f, panel, regression_type, std_err_type, cluster,
                     vcov_type) {

      f <- formula(f)

      if (regression_type == 'pooled') {
        data <- panel$data[, all.vars(f), with = FALSE]
        l <- lm(f, data = data)
      }

      # Fixed effects
      if (regression_type == 'fe') {
        # the fixed effects model is estimated using the within
        # transformation with an intercept
        # as detailed here:
        # http://www.stata.com/support/faqs/statistics/intercept-in-fixed-effects-model/

        data <- panel$within(all.vars(f))
        grand_means <- private$grand_means(panel, all.vars(f))
        for (col in 1:length(all.vars(f))) {
          v = data[, col, with = FALSE] + grand_means[col]
          set(data, j = col, value = v)
        }
        l <- lm(f, data = as.data.frame(data))
      }

      # Random effects
      if (regression_type == 're') {
        print("Random effects not implemented yet")
        return(FALSE)
      }

      ## Standard errors

      # No clustering for pooled
      if (std_err_type == 'pooled' & cluster != FALSE) {
        stop('Cluster option not available for pooled standard error')
      }

      # Robust and not clustered

      if (std_err_type == 'robust' & cluster == FALSE) {
        vc <- cluster.vcov(l, 1:panel$N)
      }

      # Robust and clustered
      if (std_err_type == 'robust' & cluster != FALSE) {
        # If cluster == true, use individual index for clustering, otherwise
        # use what is passed by cluster
        cluster_var <- ifelse(cluster == TRUE, panel$individual_index,
                              panel[, cluster, with = FALSE])
        vc <- cluster.vcov(l, cluster_var)
      }

      # pooled
      if (std_err_type == 'pooled') {
        vc <- vcov(l)
      }

      self$model <- l
      self$t_test <- coeftest(l, vc)
      self$vcov <- vc
    }
  ),
  private = list(
    grand_means = function(panel, colnames) {
      means <- numeric()
      for (col in colnames) {
        means <- append(x = means, values = panel$data[, mean(get(col), na.rm = T)])
      }
      setNames(object = means, nm = colnames)
      return(means)
    }
  )
)




`[.Panel` <- function(x, ...) {
  x$data[...]
  x$update_panel()
}

summary.Panel <- function(x) {
  summary(x$data)
}


