# ==============================================================================
# 99_checks.R
#
# Checks of the analysis dataset, and comparison with the original model-17
# dataset (orig_dir/iv_good_courts_cnum_newcntrls_j17.dta). The original case
# groups are linked to the cases here through their charges (court x person x case
# number: orig_dir/cases_IV_casenumber_part1only.dta and cc_case_xwalk, 02); the
# comparisons use the pairs linked one-to-one.
#   1. sample: size, overlap with the original, instrument coverage
#   2. treatment and prior-conviction flags vs the original
#   3. race and sex, and the black, white and female indicators
#   4. sentencing score: match coverage by case outcome, and the predicted score
#   5. correlations between instrument versions, and with the original instruments
#   6. first stages of the three versions and of the original instruments
#   7. recidivism (new felony cases) vs the original indicators (recidCC): agreement,
#      and 2SLS with each on the same cases -- non-carceral conviction controlling for
#      dismissal leniency (as 07), and
#      incarceration instrumented by incarceration leniency controlling for
#      dismissal leniency
# ==============================================================================

instr <- c("loo_d0_jyr", "loo_d1_jyr", "loo_d2_jyr")
flag_vars <- c("flag_prior_conviction", "flag_prior_conviction_any")

main <- read_csv(file.path(data_dir, "final_judge_iv_analysis.csv"), guess_max = Inf, show_col_types = FALSE)
orig <- read_dta(file.path(orig_dir, "iv_good_courts_cnum_newcntrls_j17.dta"),
                 col_select = all_of(c("case_group", "d0", "d1", "d2", instr, flag_vars))) |>
  rename_with(\(x) str_c(x, "_orig"), -case_group) |>
  rename(orig_group = case_group)

# original case groups -> cases here, through their charges
link <- read_dta(file.path(orig_dir, "cases_IV_casenumber_part1only.dta"),
                 col_select = c(case_group, fipscode_raw, person_id, casenumber)) |>
  semi_join(orig, by = c("case_group" = "orig_group")) |>
  distinct(orig_group = case_group, fips = fipscode_raw, person_id, casenumber) |>
  inner_join(read_csv(file.path(data_dir, "cc_case_xwalk.csv"), col_types = cols(casenumber = "c")),
             by = c("fips", "person_id", "casenumber"), relationship = "many-to-one") |>
  distinct(orig_group, case)
one_to_one <- link |>
  filter(n() == 1, .by = orig_group) |>
  filter(n() == 1, .by = case)
x <- main |>
  inner_join(one_to_one, by = c("case_group" = "case"), relationship = "one-to-one") |>
  inner_join(orig, by = "orig_group", relationship = "one-to-one")

cat("\nCHECK 1: sample and instruments\n")
cat("  cases =", nrow(main), "| original =", nrow(orig), "| original case groups whose case is in the sample:",
    n_distinct(semi_join(link, main, by = c("case" = "case_group"))$orig_group),
    "| linked one-to-one:", nrow(x), "\n")
cat("  max |loo_d0 + loo_d1 + loo_d2| =", signif(max(abs(main$loo_d0_jyr + main$loo_d1_jyr + main$loo_d2_jyr)), 2),
    "| unresidualized, max |sum - 1| =",
    signif(max(abs(main$loo_d0_jyr_raw + main$loo_d1_jyr_raw + main$loo_d2_jyr_raw - 1)), 2),
    "(the residualized leave-out means sum to zero and the raw shares to one, so any two carry all the information)\n")
cat("  missing: primary", sum(is.na(main$loo_d1_cond_jyr_rv)),
    "| secondary", sum(!complete.cases(select(main, all_of(instr)))), "\n")

cat("\nCHECK 2: treatment and prior-conviction flags vs the original (cases linked one-to-one)\n")
x |>
  mutate(treatment = case_when(d0 == 1 ~ "d0", d1 == 1 ~ "d1", d2 == 1 ~ "d2"),
         treatment_orig = case_when(d0_orig == 1 ~ "d0", d1_orig == 1 ~ "d1", d2_orig == 1 ~ "d2")) |>
  count(treatment_orig, treatment) |>
  pivot_wider(names_from = treatment, values_from = n, values_fill = 0) |>
  print()
walk(flag_vars, \(v) cat(str_glue("  {v}: differences {sum(x[[v]] != x[[str_c(v, '_orig')]])}"), "\n"))

cat("\nCHECK 3: race and sex; indicators should align with them\n")
stopifnot(all(main$black == (main$race %in% "Black")), all(main$white == (main$race %in% "White")),
          all(main$female == (main$sex %in% "Female")))
count(main, race, black, white) |> arrange(desc(n)) |> print()
count(main, sex, female) |> print()
cat("  persons with differing race across their cases:",
    sum(summarise(main, k = n_distinct(race), .by = person_id)$k > 1), "\n")

cat("\nCHECK 4: sentencing score, by outcome\n")
main |>
  mutate(outcome = case_when(d0 == 1 ~ "d0 dismissed",
                             d1 == 1 ~ "d1 convicted, no incar",
                             d2 == 1 ~ "d2 incarcerated")) |>
  summarise(cases = n(),
            matched_stage1  = round(mean(rv_match_stage %in% 1), 4),
            matched_stage2  = round(mean(rv_match_stage %in% 2), 4),
            with_score      = round(mean(!is.na(runningvariable)), 4),
            mean_score      = round(mean(runningvariable, na.rm = TRUE), 2),
            mean_predicted  = round(mean(rv_predicted), 2),
            .by = outcome) |>
  arrange(outcome) |>
  print(width = Inf)
cat(sprintf("  predicted vs matched score (out-of-sample, cases with a score): corr = %.3f\n",
            cor(main$runningvariable, main$rv_predicted, use = "complete.obs")))


cat("\nCHECK 5: correlations of the instruments\n")
# the original instruments are unresidualized leave-out shares: compare ours of the same kind
walk(instr, \(v) cat(sprintf("  %-12s secondary unresidualized vs original (linked) = %.4f\n", v,
                             cor(x[[str_c(v, "_raw")]], x[[str_c(v, "_orig")]], use = "complete.obs"))))
cat(sprintf("  primary vs secondary, convicted cases with a score: loo_d1_jyr %.4f | loo_d0_jyr %.4f\n",
            with(filter(main, d0 == 0, !is.na(runningvariable)), cor(loo_d1_cond_jyr_rv, loo_d1_jyr, use = "complete.obs")),
            with(filter(main, d0 == 0, !is.na(runningvariable)), cor(loo_d1_cond_jyr_rv, loo_d0_jyr, use = "complete.obs"))))
cat(sprintf("  residualized leave-out vs UJIVE, convicted cases with a score: conviction %.4f | dismissal %.4f\n",
            with(filter(main, d0 == 0, !is.na(runningvariable)), cor(loo_d1_cond_jyr_rv, loo_d1_cond_jyr_rv_ujive,
                                                                    use = "complete.obs")),
            with(filter(main, d0 == 0, !is.na(runningvariable)), cor(loo_d0_jyr, loo_d0_jyr_ujive, use = "complete.obs"))))
walk(c("White", "Black"), \(r) {
  x <- filter(main, race %in% r)
  cat(sprintf("  %s defendants: secondary by race vs pooled, loo_d1: %.4f, loo_d0: %.4f | missing by race: %d\n", r,
              cor(x$loo_d1_jyr_race, x$loo_d1_jyr, use = "complete.obs"),
              cor(x$loo_d0_jyr_race, x$loo_d0_jyr, use = "complete.obs"), sum(is.na(x$loo_d1_jyr_race))))
  # the primary instrument's cases (convicted with a score)
  iwalk(list(primary = filter(x, d0 == 0, !is.na(runningvariable))), \(conv, vn) {
    z <- iv_versions[[vn]]$z_conv
    cat(sprintf(str_c("  %s defendants, %s: by race vs pooled: %.4f | missing by race: %d | SD by race %.4f, ",
                      "pooled %.4f\n"), r, vn,
                cor(conv[[str_c(z, "_race")]], conv[[z]], use = "complete.obs"), sum(is.na(conv[[str_c(z, "_race")]])),
                sd(conv[[str_c(z, "_race")]], na.rm = TRUE), sd(conv[[z]], na.rm = TRUE)))
  })
})


# First stages with the FEs and clustering of the original first-stage table
# (areg ... i.dow i.gr_Y_crt, absorb(gr_M_crt) cluster(gr_jdg_Y_crt)), for d0, d1
# and d2 on each version's pair of instruments; F = own-instrument F statistic
# for the treatment the instrument is for
cat("\nCHECK 6: first stages (all years); se clustered by judge-year-court\n")
first_stages <- function(z, own, data) {
  walk(c("d0", "d1", "d2"), \(dv) {
    fml <- as.formula(str_glue("{dv} ~ {str_flatten(z, ' + ')} + {str_flatten(covariates, ' + ')} | gr_Y_crt + gr_M_crt + dow"))
    m <- feols(fml, data = data, cluster = ~gr_jdg_Y_crt, ssc = ssc(fixef.K = "full"), notes = FALSE)
    f <- if (!is.na(own[dv])) sprintf("   F(%s) = %6.1f", own[dv], wald(m, keep = str_c("^", own[dv], "$"), print = FALSE)$stat) else ""
    cat(sprintf("    %s: %s%s\n", dv, str_flatten(sprintf("%s %6.3f (%5.3f)", z, coef(m)[z], se(m)[z]), "   "), f))
  })
}
cat("\n  secondary (non-carceral conviction and incarceration instruments)\n")
first_stages(c("loo_d1_jyr", "loo_d2_jyr"), c(d1 = "loo_d1_jyr", d2 = "loo_d2_jyr"), main)
cat("\n  original instruments (cases linked one-to-one; treatment here)\n")
first_stages(c("loo_d1_jyr_orig", "loo_d2_jyr_orig"), c(d1 = "loo_d1_jyr_orig", d2 = "loo_d2_jyr_orig"), x)
cat("\n  primary (dismissal, and non-carceral conviction given conviction)\n")
first_stages(c("loo_d0_jyr", "loo_d1_cond_jyr_rv"), c(d0 = "loo_d0_jyr", d1 = "loo_d1_cond_jyr_rv"), main)


cat("\nCHECK 7: new felony cases after the first disposition, here vs the original recidivism indicators",
    "(cases linked one-to-one, first disposed in or before", full_follow_up_year(c("1yr", "7yr")),
    "-- full follow-up for every window)\n")
# original (recidivism_felonies_casenumber_part1only.do): any new felony charge (F),
# conviction (G_F) or incarceration (I_F) with an offense date in year 1, years 2-4
# or years 5-7 after the case's first disposition; years 1-7: any of the three, as
# in the original analysis
recid_orig <- read_dta(file.path(orig_dir, "iv_good_courts_cnum_newcntrls_j17.dta"),
                       col_select = c(case_group, matches("^recidCC(1|2_4|5_7)_(F|G_F|I_F)_eva$"))) |>
  pivot_longer(-case_group, names_to = c("window", "status"), names_pattern = "^recidCC(1|2_4|5_7)_(.+)_eva$") |>
  mutate(window = case_match(window, "1" ~ "1yr", "2_4" ~ "2to4yr", "5_7" ~ "5to7yr"),
         status = case_match(status, "F" ~ "charged", "G_F" ~ "conv", "I_F" ~ "incar"),
         value  = as.integer(value))
recid_orig <- bind_rows(recid_orig,
                        summarise(recid_orig, value = max(value), .by = c(case_group, status)) |> mutate(window = "7yr")) |>
  pivot_wider(names_from = c(window, status), values_from = value, names_glue = "orig_cc_post{window}_{status}_all_any")
x <- inner_join(x, recid_orig, by = c("orig_group" = "case_group"), relationship = "one-to-one")

# outcome pairs (here, original); all on the cases first disposed in or before 2012,
# with full follow-up for every window (as 07-09)
pairs <- expand_grid(window = c("1yr", "2to4yr", "5to7yr", "7yr"), status = c("charged", "conv", "incar")) |>
  mutate(here = str_glue("cc_post{window}_{status}_all_any"), orig = str_c("orig_", here))
x <- filter(x, Y <= full_follow_up_year(pairs$window))
cat("  NA in the original indicators:", sum(is.na(select(x, all_of(pairs$orig)))), "\n")
pmap(pairs, \(window, status, here, orig) {
  tibble(window, status, cases = nrow(x), mean_here = mean(x[[here]]), mean_orig = mean(x[[orig]]),
         here_only = mean(x[[here]] > x[[orig]]), orig_only = mean(x[[here]] < x[[orig]]), corr = cor(x[[here]], x[[orig]]))
}) |>
  list_rbind() |>
  print(n = Inf, digits = 3)

# 2SLS (White, Black, White - Black) on the same cases: coefficient, stars (* p < 0.1,
# ** p < 0.05, *** p < 0.01), (se). Outcomes here with the instruments of 07 (built
# within the defendant's race for White and Black defendants); the original outcomes
# with the pooled instruments in each race sample; and, to separate the two
# differences, the outcomes here with the pooled instruments (here_pooled). iv(v): the
# version with the instrument of the treatment
pooled <- \(v) modifyList(v, list(z_conv_race = NULL, z_dism_race = NULL))
compare_2sls <- \(title, treat, iv, with_dism) {
  cat("\n  ", title, ", same cases: coefficient (se). here: outcomes here, instruments of 07; here_pooled: outcomes ",
      "here, pooled instruments; orig: original outcomes, pooled instruments\n", sep = "")
  walk(names(iv_versions), \(vn) {
    v <- iv(iv_versions[[vn]])
    # the cases with every instrument of the runs (a few have none by race)
    z <- c(v$z_conv, v$z_dism, v$z_conv_race, v$z_dism_race)
    vs <- map(version_samples(v, x), \(s) drop_na(s, all_of(z)))
    runs <- list(here = list(v = v, ys = "here"), here_pooled = list(v = pooled(v), ys = "here"),
                 orig = list(v = pooled(v), ys = "orig"))
    if (is.null(v$z_conv_race)) runs$here_pooled <- NULL       # no instruments by race: the same as here
    est <- imap(runs, \(run, source) {
      iv_by_race(run$v, as.character(pairs[[run$ys]]), vs$white, vs$black, with_dism, treat) |>
        imap(\(r, g) mutate(r, group = g, source = source, outcome = str_remove(outcome, "^orig_"))) |>
        list_rbind()
    }) |>
      list_rbind()
    # the same cases in every column (no instrument or outcome is missing)
    stopifnot(max(summarise(est, k = n_distinct(n), .by = c(outcome, group))$k) == 1)
    cat(str_glue("\n  {v$label} instruments ({v$z_conv}{if (with_dism) str_c(', control ', v$z_dism) else ''})"), "\n")
    est |>
      mutate(cell = sprintf("%6.3f%-3s (%.3f)", est, str_remove_all(stars(p), "[\\^{}]"), se)) |>
      select(outcome, group, source, cell) |>
      pivot_wider(names_from = c(group, source), values_from = cell) |>
      select(outcome, starts_with("white_"), starts_with("black_"), starts_with("diff_")) |>
      left_join(est |> filter(source == "here", group != "diff") |>
                  select(outcome, group, n) |> pivot_wider(names_from = group, values_from = n, names_prefix = "n_"),
                by = "outcome") |>
      arrange(match(outcome, pairs$here)) |>
      print(n = Inf, width = Inf)
  })
}

# as 07: non-carceral conviction instrumented by the conviction instrument, controlling
# for dismissal leniency
compare_2sls("2SLS on non-carceral conviction (conviction leniency, controlling for dismissal leniency)", "d1",
             identity, TRUE)

# incarceration (d2) instrumented by incarceration leniency, controlling for
# dismissal leniency: the judge's leave-out incarceration share (secondary: loo_d2_jyr; by
# race with the suffix _race); in the primary version (convicted cases, d2 = 1 - d1),
# incarceration given conviction, the negative of its non-carceral conviction instrument
# (the leave-out shares of convicted cases sum to one, and the constant is absorbed by
# the fixed effects; by race likewise), so its estimates are those of non-carceral
# conviction with the dismissal control, sign flipped
x <- mutate(x, across(starts_with("loo_d1_cond_"), \(z) -z, .names = "{str_replace(.col, 'd1', 'd2')}"))
# predicted-score version: the residualized leave-out means of d0, d1 and d2 over the same
# cases sum to zero, so its incarceration instrument is minus the sum of the other two
x <- mutate(x, loo_d2_jyr_pred = -(loo_d0_jyr_pred + loo_d1_jyr_pred),
            loo_d2_jyr_pred_race = -(loo_d0_jyr_pred_race + loo_d1_jyr_pred_race))
incar_iv <- \(v) {
  z <- str_replace(v$z_conv, "d1", "d2")
  modifyList(v, list(z_conv = z, z_conv_race = if (!is.null(v$z_conv_race)) str_c(z, "_race")))
}
compare_2sls("2SLS on incarceration (incarceration leniency, controlling for dismissal leniency)", "d2",
             incar_iv, TRUE)
