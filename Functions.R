
# x is the result of a rmse_prev

get_indicateurs_all <- function(x, years, ...) {
  variables = seq_along(names(x$model$lm$coefficients))
  result = t(sapply(variables, get_indicateurs_variable, x = x, years = years))
  rownames(result) = names(x$model$lm$coefficients)
  return(result)
}

# x est un rmse_prev, et on donne une variable

get_indicateurs_variable <- function(x, years, variable, ...) {
  var_all_date = lapply(as.numeric(years), function(dates) {
    get_tvlm_coef(date = dates,
                  model = x,
                  variable = variable)
  })
  names(var_all_date) = years
  are_na = sapply(var_all_date, function(x) {
    is.na(x[length(x)])
  })
  var_all_date = var_all_date[!are_na]
  var_all_date = lapply(var_all_date, na.locf)
  colMeans(t(sapply(var_all_date, calculate_indicateurs)), na.rm = TRUE)
}

# x est un ts

calculate_indicateurs <- function(x) {
  first_value <- x[1]
  last_value <- x[length(x)]
  result <- c(abs((first_value - x[c(10, 15, 20, length(x))])/last_value),
              abs((x[20] - last_value)/last_value))
  names(result) <- c("Difference 1-10", "Difference 1-15", "Difference 1-20", "Difference 1-end", "Difference 20-end")
  return(result)
}

get_indicateurs_all_ssm <- function(x, years, ...) {
  variables = seq_len(ncol(x[[1]]) - 1)
  result = t(sapply(variables, get_indicateurs_variable_ssm, x = x, years = years))
  rownames(result) = colnames(x[[1]])[- ncol(x[[1]])]
  return(result)
}

get_indicateurs_variable_ssm <- function(x, years, variable, ...) {
  smoothed_states = lapply(x[[2]], `[[`, "smoothed_states")
  smoothed_states = smoothed_states[!is.na(lapply(smoothed_states, any))]
  smoothed_states = lapply(seq_along(smoothed_states), function(i) {
    ts(smoothed_states[[i]][complete.cases(smoothed_states[[i]]),], end = time(x[[1]])[i], frequency = 4)
  })
  var_all_date = lapply(as.numeric(years), function(date){
    sapply(seq_along(smoothed_states), function(i) {
      window(smoothed_states[[i]][, variable], start = date, end = date, extend = TRUE)
    })
  })
  names(var_all_date) = years
  are_na = sapply(var_all_date, function(x) {
    is.na(x[length(x)])
  })
  var_all_date = var_all_date[!are_na]
  var_all_date = lapply(var_all_date, na.locf)
  colMeans(t(sapply(var_all_date, calculate_indicateurs)), na.rm = TRUE)
}



rmse_false_oos_tvlm = function(tvlm) {
  datatest = tvlm$x
  coef = tvlm$coef
  coef = ts(coef, end = end(datatest), frequency = frequency(datatest))
  predict <- sapply(2:nrow(datatest), function(i) {
    datatest[i, ] %*% coef[i-1, ]
  })
  rmse_res(tvlm$y[-1] - predict)
}

