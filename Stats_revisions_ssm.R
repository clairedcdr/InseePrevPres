# A priori tjrs un pb sur filtered et filtering pour les modèles avec des dummies. Dans ssm_lm, data0 ne prend des valeurs que si toutes les variables d'une colonne valent 0. Hors le principe d'une indicatrice c'est que toutes ses valeurs ne valent pas 0. Donc ensuite ne calcule pas les coefficients jusqu'à la présence de toutes les dummies.

ssm_lm_all
x = ssm_lm_all$model_c5_5
get_indicateurs_all(x, years)
ssm_lm_all_oos$model_c5_5

length(x[[2]])

# Get coef filtering ts

x = ssm_lm_oos(model_c5_5)
y = lapply(x[[2]], `[[`, "filtering_states")

y = lapply(seq_along(y), function(i) {
  ts(y[[i]][complete.cases(y[[i]]),], end = time(x[[1]])[i], frequency = 4)
})

y[[1]][, "(Intercept)"]
ts(y[[1]][, 1], start = 1994, end = 1994)
w = sapply(seq_along(y), function(i) {
  window(y[[i]][,variable], start = date, end = date, extend = TRUE)
})

years = window(time(data), start = 1999.5, end = 2015)
names(models)
ri_ssm = sapply(ssm_lm_all_oos, get_indicateurs_all_ssm, years = years)

ri_ssm_c1 = ri_ssm[names(ri_ssm) %in% ls(pattern = "model_c1_")]
ri_ssm_c3 = ri_ssm[names(ri_ssm) %in% ls(pattern = "model_c3_")]
ri_ssm_c4 = ri_ssm[names(ri_ssm) %in% ls(pattern = "model_c4_")]
ri_ssm_c5 = ri_ssm[names(ri_ssm) %in% ls(pattern = "model_c5_")]
ri_ssm_manuf = ri_ssm[names(ri_ssm) %in% ls(pattern = "model_manuf_")]

ri_ssm_1_10 = unlist(sapply(ri_ssm, function(x) {x[, "Difference 1-10"]} ))
ri_ssm_1_15 = unlist(sapply(ri_ssm, function(x) {x[, "Difference 1-15"]} ))
ri_ssm_1_20 = unlist(sapply(ri_ssm, function(x) {x[, "Difference 1-20"]} ))
ri_ssm_1_last = unlist(sapply(ri_ssm, function(x) {x[, "Difference 1-end"]} ))
ri_ssm_20_last = unlist(sapply(ri_ssm, function(x) {x[, "Difference 20-end"]} ))

ri_ssm_c1_1_10 = unlist(sapply(ri_ssm_c1, function(x) {x[, "Difference 1-10"]} ))
ri_ssm_c1_1_15 = unlist(sapply(ri_ssm_c1, function(x) {x[, "Difference 1-15"]} ))
ri_ssm_c1_1_20 = unlist(sapply(ri_ssm_c1, function(x) {x[, "Difference 1-20"]} ))
ri_ssm_c1_1_last = unlist(sapply(ri_ssm_c1, function(x) {x[, "Difference 1-end"]} ))
ri_ssm_c1_20_last = unlist(sapply(ri_ssm_c1, function(x) {x[, "Difference 20-end"]} ))

ri_ssm_c3_1_10 = unlist(sapply(ri_ssm_c3, function(x) {x[, "Difference 1-10"]} ))
ri_ssm_c3_1_15 = unlist(sapply(ri_ssm_c3, function(x) {x[, "Difference 1-15"]} ))
ri_ssm_c3_1_20 = unlist(sapply(ri_ssm_c3, function(x) {x[, "Difference 1-20"]} ))
ri_ssm_c3_1_last = unlist(sapply(ri_ssm_c3, function(x) {x[, "Difference 1-end"]} ))
ri_ssm_c3_20_last = unlist(sapply(ri_ssm_c3, function(x) {x[, "Difference 20-end"]} ))

ri_ssm_c4_1_10 = unlist(sapply(ri_ssm_c4, function(x) {x[, "Difference 1-10"]} ))
ri_ssm_c4_1_15 = unlist(sapply(ri_ssm_c4, function(x) {x[, "Difference 1-15"]} ))
ri_ssm_c4_1_20 = unlist(sapply(ri_ssm_c4, function(x) {x[, "Difference 1-20"]} ))
ri_ssm_c4_1_last = unlist(sapply(ri_ssm_c4, function(x) {x[, "Difference 1-end"]} ))
ri_ssm_c4_20_last = unlist(sapply(ri_ssm_c4, function(x) {x[, "Difference 20-end"]} ))

ri_ssm_c5_1_10 = unlist(sapply(ri_ssm_c5, function(x) {x[, "Difference 1-10"]} ))
ri_ssm_c5_1_15 = unlist(sapply(ri_ssm_c5, function(x) {x[, "Difference 1-15"]} ))
ri_ssm_c5_1_20 = unlist(sapply(ri_ssm_c5, function(x) {x[, "Difference 1-20"]} ))
ri_ssm_c5_1_last = unlist(sapply(ri_ssm_c5, function(x) {x[, "Difference 1-end"]} ))
ri_ssm_c5_20_last = unlist(sapply(ri_ssm_c5, function(x) {x[, "Difference 20-end"]} ))

ri_ssm_manuf_1_10 = unlist(sapply(ri_ssm_manuf, function(x) {x[, "Difference 1-10"]} ))
ri_ssm_manuf_1_15 = unlist(sapply(ri_ssm_manuf, function(x) {x[, "Difference 1-15"]} ))
ri_ssm_manuf_1_20 = unlist(sapply(ri_ssm_manuf, function(x) {x[, "Difference 1-20"]} ))
ri_ssm_manuf_1_last = unlist(sapply(ri_ssm_manuf, function(x) {x[, "Difference 1-end"]} ))
ri_ssm_manuf_20_last = unlist(sapply(ri_ssm_manuf, function(x) {x[, "Difference 20-end"]} ))

