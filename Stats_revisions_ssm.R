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
ri_ssm_c1 = sapply(ssm_lm_all_oos[1:7], get_indicateurs_all_ssm, years = years) #ok
ri_ssm_c3 = sapply(ssm_lm_all_oos[8:12], get_indicateurs_all_ssm, years = years)
ri_ssm_c4 = sapply(ssm_lm_all_oos[13:17], get_indicateurs_all_ssm, years = years)
ri_ssm_c5 = sapply(ssm_lm_all_oos[18:22], get_indicateurs_all_ssm, years = years)
ri_ssm_manuf = sapply(ssm_lm_all_oos[23:25], get_indicateurs_all_ssm, years = years)

get_indicateurs_all_ssm(ssm_lm_all_oos$model_c3_1, years) #ok
get_indicateurs_all_ssm(ssm_lm_all_oos$model_c3_2, years) #ok
ssm_lm_all_oos$model_c3_2

ssm_lm_oos(model_c3_2)
