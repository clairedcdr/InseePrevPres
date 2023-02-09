
years = window(time(data), start = 1999.5, end = 2015)

ri_tvlm = sapply(test_fixed, get_indicateurs_all, years = years)

ri_tvlm_c1 = ri_tvlm[names(ri_tvlm) %in% ls(pattern = "model_c1_")]
ri_tvlm_c3 = ri_tvlm[names(ri_tvlm) %in% ls(pattern = "model_c3_")]
ri_tvlm_c4 = ri_tvlm[names(ri_tvlm) %in% ls(pattern = "model_c4_")]
ri_tvlm_c5 = ri_tvlm[names(ri_tvlm) %in% ls(pattern = "model_c5_")]
ri_tvlm_manuf = ri_tvlm[names(ri_tvlm) %in% ls(pattern = "model_manuf_")]

ri_tvlm_1_10 = unlist(sapply(ri_tvlm, function(x) {x[, "Difference 1-10"]} ))
ri_tvlm_1_15 = unlist(sapply(ri_tvlm, function(x) {x[, "Difference 1-15"]} ))
ri_tvlm_1_20 = unlist(sapply(ri_tvlm, function(x) {x[, "Difference 1-20"]} ))
ri_tvlm_1_last = unlist(sapply(ri_tvlm, function(x) {x[, "Difference 1-end"]} ))
ri_tvlm_20_last = unlist(sapply(ri_tvlm, function(x) {x[, "Difference 20-end"]} ))

ri_tvlm_c1_1_10 = unlist(sapply(ri_tvlm_c1, function(x) {x[, "Difference 1-10"]} ))
ri_tvlm_c1_1_15 = unlist(sapply(ri_tvlm_c1, function(x) {x[, "Difference 1-15"]} ))
ri_tvlm_c1_1_20 = unlist(sapply(ri_tvlm_c1, function(x) {x[, "Difference 1-20"]} ))
ri_tvlm_c1_1_last = unlist(sapply(ri_tvlm_c1, function(x) {x[, "Difference 1-end"]} ))
ri_tvlm_c1_20_last = unlist(sapply(ri_tvlm_c1, function(x) {x[, "Difference 20-end"]} ))

ri_tvlm_c3_1_10 = unlist(sapply(ri_tvlm_c3, function(x) {x[, "Difference 1-10"]} ))
ri_tvlm_c3_1_15 = unlist(sapply(ri_tvlm_c3, function(x) {x[, "Difference 1-15"]} ))
ri_tvlm_c3_1_20 = unlist(sapply(ri_tvlm_c3, function(x) {x[, "Difference 1-20"]} ))
ri_tvlm_c3_1_last = unlist(sapply(ri_tvlm_c3, function(x) {x[, "Difference 1-end"]} ))
ri_tvlm_c3_20_last = unlist(sapply(ri_tvlm_c3, function(x) {x[, "Difference 20-end"]} ))

ri_tvlm_c4_1_10 = unlist(sapply(ri_tvlm_c4, function(x) {x[, "Difference 1-10"]} ))
ri_tvlm_c4_1_15 = unlist(sapply(ri_tvlm_c4, function(x) {x[, "Difference 1-15"]} ))
ri_tvlm_c4_1_20 = unlist(sapply(ri_tvlm_c4, function(x) {x[, "Difference 1-20"]} ))
ri_tvlm_c4_1_last = unlist(sapply(ri_tvlm_c4, function(x) {x[, "Difference 1-end"]} ))
ri_tvlm_c4_20_last = unlist(sapply(ri_tvlm_c4, function(x) {x[, "Difference 20-end"]} ))

ri_tvlm_c5_1_10 = unlist(sapply(ri_tvlm_c5, function(x) {x[, "Difference 1-10"]} ))
ri_tvlm_c5_1_15 = unlist(sapply(ri_tvlm_c5, function(x) {x[, "Difference 1-15"]} ))
ri_tvlm_c5_1_20 = unlist(sapply(ri_tvlm_c5, function(x) {x[, "Difference 1-20"]} ))
ri_tvlm_c5_1_last = unlist(sapply(ri_tvlm_c5, function(x) {x[, "Difference 1-end"]} ))
ri_tvlm_c5_20_last = unlist(sapply(ri_tvlm_c5, function(x) {x[, "Difference 20-end"]} ))

ri_tvlm_manuf_1_10 = unlist(sapply(ri_tvlm_manuf, function(x) {x[, "Difference 1-10"]} ))
ri_tvlm_manuf_1_15 = unlist(sapply(ri_tvlm_manuf, function(x) {x[, "Difference 1-15"]} ))
ri_tvlm_manuf_1_20 = unlist(sapply(ri_tvlm_manuf, function(x) {x[, "Difference 1-20"]} ))
ri_tvlm_manuf_1_last = unlist(sapply(ri_tvlm_manuf, function(x) {x[, "Difference 1-end"]} ))
ri_tvlm_manuf_20_last = unlist(sapply(ri_tvlm_manuf, function(x) {x[, "Difference 20-end"]} ))


boxplot(ri_tvlm_c4, horizontal = FALSE, outline = FALSE)
boxplot(ri_tvlm_1_20, horizontal = TRUE, outline = FALSE)
boxplot(ri_tvlm, horizontal = TRUE, outline = FALSE)
boxplot(ri_tvlm_c5_1_10, horizontal = TRUE, outline = FALSE)

boxplot(list(ri_tvlm_c1_1_10, ri_tvlm_c1_1_15, ri_tvlm_c1_1_20), horizontal = TRUE, outline = FALSE)
