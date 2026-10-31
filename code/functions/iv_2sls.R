# ==============================================================================
# iv_2sls.R
#
# 2SLS of the judge IV by race (07, 99): outcomes on a treatment (default non-carceral
# conviction, d1) instrumented by a version's instrument z_conv (iv_versions,
# 00_master.R), with the version's controls and fixed effects (v$fe: the construction
# fixed effects, plus the (predicted) score in the secondary and tertiary versions) and
# iv_cluster standard errors.
#   version_samples(v, d)    full, all, White and Black defendants of a version (only the
#                            convicted if convicted_only; White and Black: only the
#                            cases of judges with enough cases of each race, v$race_sample;
#                            all: White and Black together, with race_interacted columns,
#                            if the version has race-specific instruments; full: every case
#                            of the version, before the race-sample rule)
#   race_interacted(x, v)    adds <control>_bk and <fixed effect>_bk = the control or fixed
#                            effect within race, for the all-defendants specification
#                            (for_sample, 00_master.R)
#   fe_of(v), x_of(...)      the absorbed part of a formula: the fixed effects and the
#                            controls, which enter as fixed effects (one indicator per
#                            value; user decision 2026-10-26); the regressors, "1" if none
#   iv_by_race(v, ys, white, black, with_dism, treat)   White, Black and White -
#                            Black estimates for outcomes ys, one tibble each
#                            (outcome, est, se, p, mean_y, n)
#   ols_by_race(v, ys, white, black)   the same for OLS of ys on d1 and d0 (d1:
#                            non-carceral conviction vs incarceration)
# ==============================================================================

rhs <- \(...) str_flatten(c(...), " + ")
fe_of <- \(v) rhs(v$fe, v$controls)
x_of <- \(...) if (length(c(...))) rhs(...) else "1"

version_samples <- \(v, d) {
  x <- if (v$convicted_only) filter(d, d0 == 0) else d
  wb <- if (is.null(v$race_sample)) x else filter(x, .data[[v$race_sample]] == 1)
  white <- filter(wb, race %in% "White")
  black <- filter(wb, race %in% "Black")
  list(full = x, all = if (is.null(v$z_conv_race)) x else race_interacted(bind_rows(white, black), v),
       white = white, black = black)
}

race_interacted <- \(x, v) {
  absorbed <- intersect(c(v$fe, setdiff(v$controls, race_controls)), names(x))
  mutate(x, across(all_of(absorbed), \(f) str_c(f, "_", black), .names = "{.col}_bk"))
}

# coefficient, se, p, outcome mean and N of `term`, one row per outcome of a multi-outcome fit
coef_tbl <- \(fits, term) {
  as.list(fits) |>
    map(\(m) tibble(est = coef(m)[[term]], se = se(m)[[term]], p = pvalue(m)[[term]],
                    mean_y = fitstat(m, "my")[[1]], n = nobs(m))) |>
    list_rbind()
}

# 2SLS within one group (with_dism: also controlling for the dismissal instrument)
iv_fit <- \(v, ys, data, with_dism, treat = "d1") {
  feols(as.formula(str_glue("c({str_flatten(ys, ', ')}) ~ {x_of(if (with_dism) v$z_dism)} | ",
                            "{fe_of(v)} | {treat} ~ {v$z_conv}")),
        data = data, cluster = iv_cluster, notes = FALSE) |>
    coef_tbl(str_c("fit_", treat))
}

# White - Black: one regression of White and Black defendants with every control,
# fixed effect, treatment and instrument interacted with a White indicator, so the
# interaction equals the difference of the two separate estimates and its standard
# error accounts for judges shared by both groups
iv_diff <- \(v, ys, white, black, with_dism, treat = "d1") {
  ctrl <- if (with_dism) v$z_dism
  x <- bind_rows(white, black) |>
    mutate(wh = as.integer(race == "White"),
           across(all_of(c(ctrl, treat, v$z_conv)), \(c) c * wh, .names = "{.col}_wh"))
  feols(as.formula(str_glue("c({str_flatten(ys, ', ')}) ~ {x_of(ctrl, if (length(ctrl)) str_c(ctrl, '_wh'))} | ",
                            "{str_flatten(str_c(c(v$fe, v$controls), '^wh'), ' + ')} | ",
                            "{treat} + {treat}_wh ~ {v$z_conv} + {v$z_conv}_wh")),
        data = x, cluster = iv_cluster, notes = FALSE) |>
    coef_tbl(str_c("fit_", treat, "_wh"))
}

# OLS by race, for comparison with the 2SLS: outcomes ys on non-carceral conviction (d1)
# and dismissal (d0), so that the coefficient on d1 compares non-carceral conviction with
# incarceration (the comparison of the 2SLS with the dismissal-leniency control), with
# the version's controls and fixed effects; White, Black and White - Black (fully
# interacted, as iv_diff)
ols_by_race <- \(v, ys, white, black) {
  v <- for_sample(v, "white")
  x_vars <- c("d1", "d0")
  ols <- \(data, terms, fe) feols(as.formula(str_glue("c({str_flatten(ys, ', ')}) ~ {rhs(terms)} | {fe}")),
                                  data = data, cluster = iv_cluster, notes = FALSE)
  pooled <- bind_rows(white, black) |>
    mutate(wh = as.integer(race == "White"), across(all_of(x_vars), \(c) c * wh, .names = "{.col}_wh"))
  r <- list(white = coef_tbl(ols(white, x_vars, fe_of(v)), "d1"),
            black = coef_tbl(ols(black, x_vars, fe_of(v)), "d1"),
            diff  = coef_tbl(ols(pooled, c(x_vars, str_c(x_vars, "_wh")),
                                 str_flatten(str_c(c(v$fe, v$controls), "^wh"), " + ")), "d1_wh"))
  stopifnot(max(abs(r$diff$est - (r$white$est - r$black$est))) < 1e-8)   # pooled = separate
  map(r, \(x) mutate(x, outcome = ys, .before = 1))
}

# White, Black and White - Black estimates for outcomes ys on the given samples
# (with the instruments by race, if the version has them: the same variables in
# both groups, so the pooled regression still equals the separate ones)
iv_by_race <- \(v, ys, white, black, with_dism, treat = "d1") {
  v <- for_sample(v, "white")
  r <- list(white = iv_fit(v, ys, white, with_dism, treat), black = iv_fit(v, ys, black, with_dism, treat),
            diff  = iv_diff(v, ys, white, black, with_dism, treat))
  stopifnot(max(abs(r$diff$est - (r$white$est - r$black$est))) < 1e-8)   # pooled = separate
  map(r, \(x) mutate(x, outcome = ys, .before = 1))
}

# table note on the score control of an analysis with two versions (analyses,
# 00_master.R: names = versions, values = column labels)
score_note <- \(vns) if (length(vns) < 2) "" else {
  v2 <- iv_versions[[names(vns)[2]]]
  str_glue(" Columns '{vns[2]}': the instruments ({v2$z_conv}, {v2$z_dism}; by race {v2$z_conv_race}, ",
           "{v2$z_dism_race}) built, and the regressions estimated, also with one indicator per rounded value of the ",
           "predicted sentencing score (05).")
}
