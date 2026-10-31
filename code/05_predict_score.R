# ==============================================================================
# 05_predict_score.R
#
# Predicts the sentencing score (runningvariable) of every case from
# characteristics fixed before the judge rules, which mirror the inputs of the
# guidelines worksheet:
#   charges filed    offense-type counts of the felony charges and the number of
#                    misdemeanor charges (03), felony-class counts, counts of the 100
#                    most common code sections, number of charges
#   criminal record  SC prior-record flags (Part 12, 04); number of / any prior
#                    SC convictions by offense group; number of / any prior CC
#                    cases charged, convicted and incarcerated by offense type (04)
#   court and year   of the case
# Race, sex and neighborhood are left out, as the guidelines do not use them;
# process outcomes (pleas, trials, dispositions, sentences) and the judge are
# left out, as the judge affects them.
#
# Random forest (ranger), trained on the cases with a matched score, without and
# with the prior charged CC cases (cc_prior_fel_charged_*, cc_prior_mis_charged_*); the version with the
# lower out-of-bag RMSE is used. Cases with a score get their out-of-bag
# prediction, the others the prediction of the forest fit on all scored cases.
#
# Inputs:  data_dir/cc_person_level_data.csv
#          data_dir/cc_felony_charges.csv   (the cases' charges, 02)
# Output:  data_dir/score_predictions.csv   case_group, rv_predicted
# ==============================================================================

nb_vars <- c(str_c("nb_", offense_type_names), "nb_m")
record_vars <- c("flag_prior_conviction", "flag_prior_conviction_5yr", "flag_prior_incar",
                 "flag_prior_sc_convic", "num_prior_convic", "time_since_prior")

cases <- read_csv(file.path(data_dir, "cc_person_level_data.csv"),
                  col_select = c(all_of(c("case_group", "fipscode_raw", "Y", "runningvariable",
                                          nb_vars, record_vars)),
                                 starts_with(c("sc_prior_", "cc_prior_"))),
                  show_col_types = FALSE)


# ---- features ------------------------------------------------------------------
# the charges filed: number, felony classes, most common code sections
charges <- read_csv(file.path(data_dir, "cc_felony_charges.csv"), col_types = cols(.default = col_character()),
                    col_select = c(case_group = case, codesection, class = felony_class)) |>
  mutate(case_group = as.numeric(case_group)) |>
  semi_join(cases, by = "case_group")
top_codes <- count(charges, codesection, sort = TRUE) |> slice_head(n = 100) |> pull(codesection)
charge_counts <- charges |>
  mutate(class = str_c("class_", if_else(class %in% c(1:6, "U"), class, "missing")),
         code  = str_c("code_", str_replace_all(if_else(codesection %in% top_codes, codesection, "other"),
                                                "[^A-Za-z0-9]", "_"))) |>
  pivot_longer(c(class, code), values_to = "feature") |>
  count(case_group, feature) |>
  pivot_wider(names_from = feature, values_from = n, values_fill = 0) |>
  mutate(n_charges = rowSums(pick(starts_with("class_"))))

features <- cases |>
  select(case_group, fipscode_raw, Y, all_of(c(nb_vars, record_vars)), starts_with(c("sc_prior_", "cc_prior_"))) |>
  left_join(charge_counts, by = "case_group", relationship = "one-to-one") |>
  mutate(across(-case_group, \(x) coalesce(x, 0)),
         court = factor(fipscode_raw)) |>
  select(-case_group, -fipscode_raw)
scored <- !is.na(cases$runningvariable)
y <- cases$runningvariable[scored]
cat("cases:", format(nrow(cases), big.mark = ","), "| with a score (training):", format(sum(scored), big.mark = ","),
    "\n")


# ---- random forest, without and with prior charged cases --------------------------
rmse <- \(pred) sqrt(mean((y - pred)^2))
r2   <- \(pred) 1 - mean((y - pred)^2) / mean((y - mean(y))^2)
fit_forest <- \(x) ranger::ranger(x = x[scored, ], y = y, num.trees = 300, seed = match_seed,
                                  respect.unordered.factors = "order", verbose = FALSE,
                                  num.threads = max(1, parallel::detectCores() - 1))

fits <- tibble(version = c("without prior charged", "with prior charged"),
               x       = list(select(features, -matches("^cc_prior_(fel|mis)_charged_")), features)) |>
  mutate(forest     = map(x, fit_forest),
         n_features = map_int(x, ncol),
         oob_rmse   = map_dbl(forest, \(f) rmse(f$predictions)),
         oob_r2     = map_dbl(forest, \(f) r2(f$predictions)))
cat("out-of-bag fit (score SD", round(sd(y), 3), "):\n")
print(select(fits, version, n_features, oob_rmse, oob_r2), digits = 4)
best <- slice_min(fits, oob_rmse, n = 1, with_ties = FALSE)
cat("using:", best$version, "\n")

rv_predicted <- numeric(nrow(cases))
rv_predicted[scored]  <- best$forest[[1]]$predictions
rv_predicted[!scored] <- predict(best$forest[[1]], best$x[[1]][!scored, ], verbose = FALSE)$predictions

tibble(case_group = cases$case_group, rv_predicted) |>
  write_csv(file.path(data_dir, "score_predictions.csv"))
cat("score_predictions:", format(nrow(cases), big.mark = ","), "cases\n")
