
# Dates par modèles/secteurs --------

breakdates = sapply(test, function(x) {
  x$model$piece_lm$breakdates
})

breakdates_ = unlist(Filter(function(x) any(x != 0), breakdates))
hist(trunc(breakdates_), main = "Breakdates", xlab = "Number of breakdates", border = "black", breaks = time(data))



breakdates_c1 = unlist(breakdates[names(breakdates) %in% ls(pattern = "model_c1_")])
breakdates_c3 = unlist(breakdates[names(breakdates) %in% ls(pattern = "model_c3_")])
breakdates_c4 = unlist(breakdates[names(breakdates) %in% ls(pattern = "model_c4_")])
breakdates_c5 = unlist(breakdates[names(breakdates) %in% ls(pattern = "model_c5_")])
breakdates_manuf = unlist(breakdates[names(breakdates) %in% ls(pattern = "model_manuf_")])

hist(breakdates_c1, main = "Breakdates in c1", xlab = "Number of breakdates", border = "black", breaks = time(data))
hist(breakdates_c3, main = "Breakdates in c3", xlab = "Number of breakdates", border = "black", breaks = time(data))
hist(breakdates_c4, main = "Breakdates in c4", xlab = "Number of breakdates", border = "black", breaks = time(data))
hist(breakdates_c5, main = "Breakdates in c5", xlab = "Number of breakdates", border = "black", breaks = time(data))
hist(breakdates_manuf, main = "Breakdates in manuf", xlab = "Number of breakdates", border = "black", breaks = time(data))

# Nombre de breakdates ----------

nb_breakdates = sapply(breakdates, length)
hist(nb_breakdates, main = "Breakdates", xlab = "Number of breakdates", border = "black")

# Révisions des breakdates ------------

x = lapply(test$model_c5_5$prevision$prev_lm$model, function(x) {
  x$model
})
lapply(x, function(df) {
  breakpoints(formula = prod_c5 ~., data = df)
})

formula = lapply(models, get_formula)

