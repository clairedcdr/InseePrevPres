models = mget(ls(pattern = "model_(c|m)"))

tvlm = mget(ls(pattern = "^tvlm_(c|m)\\w*(_\\d*)+$"))

model_variant = mget(c("model_c3_2", "model_c3_4", "model_c5_1", "model_c5_2", "model_c5_3", "model_c5_4", "model_c5_5", "model_manuf_3"))
model_invariant = models[names(models) %in% names(model_variant) == FALSE]

get_formula_variants = lapply(model_variant, get_formula)
fixed_coef_variants = lapply(model_variant, fixed_coefficients)

test_model_variant = lapply(seq_along(model_variant), function(i) {
  rmse_prev(formula = get_formula_variants[[i]],
            data = data,
            var_fixes = fixed_coef_variants[[i]],
            date = 28)
})
names(test_model_variant) = names(model_variant)
test_model_invariant = lapply(lapply(model_invariant, get_formula), rmse_prev, data = data, date = 28)

test = c(test_model_invariant[1:8],
         test_model_variant[1],
         test_model_invariant[9],
         test_model_variant[2],
         test_model_invariant[10:15],
         test_model_variant[3:7],
         test_model_invariant[16:17],
         test_model_variant[8])

test_variant_fixed = lapply(seq_along(model_variant), function(i) {
  rmse_prev(formula = get_formula_variants[[i]],
            data = data,
            var_fixes = fixed_coef_variants[[i]],
            date = 28,
            fixed_bw = TRUE)
})
names(test_variant_fixed) = names(model_variant)
test_invariant_fixed = lapply(lapply(model_invariant, get_formula), rmse_prev, data = data, date = 28, fixed_bw = TRUE)

test_fixed = c(test_invariant_fixed[1:8],
               test_variant_fixed[1],
               test_invariant_fixed[9],
               test_variant_fixed[2],
               test_invariant_fixed[10:15],
               test_variant_fixed[3:7],
               test_invariant_fixed[16:17],
               test_variant_fixed[8])

model_dummy = sapply(models, function(mod) {
  ind <- length(grep("ind", names(coef(mod)))) > 0
})
model_dummy = mget(names(model_dummy[model_dummy == TRUE]))
model_wo_dummy = models[names(models) %in% names(model_dummy) == FALSE]

