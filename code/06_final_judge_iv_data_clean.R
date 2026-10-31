# ==============================================================================
# 06_final_judge_iv_data_clean.R
#
# The analysis sample and instruments of Cecile code/replication/01_cleaning/
# make_data_judge_iv_clean.do, judge model 17 (judge = modal_judge_5, 3-year
# window, 100-case cutoff, leave-out-person means), on the judge-IV cases rebuilt
# from the full CC record (03-04). The instruments are residualized leave-out means (as
# Arnold, Dobbie and Yang 2018): each decision is residualized on the controls and fixed
# effects of its analysis in a regression on all cases, and a case's instrument is the
# mean residual over the judge's cases of other defendants in the judge x court x
# 3-year window (leaving out all of the defendant's cases). Given to every case:
#   loo_d0_jyr, loo_d1_jyr, loo_d2_jyr: dismissal, non-carceral conviction and
#              incarceration rates over all cases (the secondary analysis; loo_d0_jyr is
#              also the primary analysis's dismissal instrument)
#   loo_d1_cond_jyr_rv: non-carceral conviction rate over the convicted cases with a
#              matched score (the primary analysis)
#   by race (suffix _race): the same over the judge's cases of defendants of the case's
#              race, for White and Black defendants (NA for other races)
#   loo_d0_jyr_pred, loo_d1_jyr_pred (and _race): residualized with the predicted-score
#              indicators as well (the secondary analysis with the score control)
#   loo_d0_jyr_raw, loo_d1_jyr_raw, loo_d2_jyr_raw: unresidualized leave-out means over all
#              cases, the Revolving Door paper's construction (09)
#   UJIVE versions of the primary analysis's instruments, for its robustness check (07):
#              loo_d0_jyr_ujive, loo_d1_cond_jyr_rv_ujive (and _race_ujive): the
#              decisions residualized on the controls and fixed effects (00_master.R:
#              resid_controls without race by race; resid_fe, plus one indicator per
#              guidelines offense group x matched score cell for the conviction instrument),
#              with the defendant
#              left out of the residualization as well as of the judge's mean
#              (functions/ujive.R), so that their own treatment never enters their
#              instrument
# offense_score_cell: the guidelines offense group of the case's own SC sentencing event
#   (rv_offense_group, 04: the worksheet its score is computed on) x its score value: the
#   fixed effects of the primary analysis
# judge_races_<main|secondary>: 1 if the case's judge has at least min_race_cases cases
#   of White and of Black defendants in the case's 3-year window (judge x court x years
#   Y-1 to Y+1, the instruments' window), among the cases the analysis's instruments are
#   built from (main: all cases, the secondary analysis; secondary: convicted with a
#   matched score, the primary analysis); the White and Black samples of the analysis
#   keep these cases only; judge_races_secondary_min<n>: the same with at least n cases of
#   each race (min_race_cases_robustness), for the robustness of the primary analysis
#
# Inputs:  data_dir/cc_person_level_data.csv
#          data_dir/score_predictions.csv
# Output:  data_dir/final_judge_iv_analysis.csv
# ==============================================================================

# ---- Part 1: basic dataset prep ------------------------------------------------
d <- read_csv(file.path(data_dir, "cc_person_level_data.csv"),
              guess_max = Inf, show_col_types = FALSE) |>
  model17_years() |>                                                     # 00_master
  rename(flag_prior_conviction_any = flag_prior_conviction,
         flag_prior_conviction     = flag_prior_conviction_5yr) |>
  mutate(judgeid = modal_judge_5) |>                                     # judge assignment of model 17
  # predicted score of every case (05), and its value rounded to an integer
  left_join(read_csv(file.path(data_dir, "score_predictions.csv"), show_col_types = FALSE),
            by = "case_group", relationship = "one-to-one") |>
  mutate(rv_pred_value = round(rv_predicted))


# ---- Part 2: model-17 sample (courts, judge caseloads: model17_sample, 00_master) ---
d <- exec(subset_judgeiv_data, d, !!!model17_sample)


# ---- Part 3: group and treatment variables --------------------------------------
# group ids are numbered in the sorted order of the grouping variables
d <- d |>
  mutate(any_mis = any_m, any_sexoff = any_sex_offense, any_sexas = any_sex_assault) |>
  group_by(judgeid, Y)           |> mutate(gr_jdg_Y = cur_group_id()) |>
  group_by(Y, fipscode)          |> mutate(gr_Y_crt = cur_group_id(), gr_yr_crt = gr_Y_crt) |>
  group_by(month, fipscode)      |> mutate(gr_M_crt = cur_group_id(), gr_m_crt = gr_M_crt) |>
  group_by(judgeid, Y, fipscode) |> mutate(gr_jdg_Y_crt = cur_group_id(),       # cells for
                                           gr_jdg_yr_crt = gr_jdg_Y_crt) |>     # judge instruments
  group_by(dow, fipscode)        |> mutate(gr_dow_crt = cur_group_id()) |>      # new: court x day of week
  ungroup() |>
  mutate(d0 = as.integer(convict %in% 0),                   # dismissed
         d1 = as.integer(convict %in% 1 & incar %in% 0),    # convicted, not incarcerated
         d2 = as.integer(convict %in% 1 & incar %in% 1))    # incarcerated

# shares of the controls (00_master.R) of the prior record (04: a conviction in the 5
# years before the case's first offense; felony: CC or SC) and race (black)
d |>
  summarise(across(all_of(c("any_mis", prior_record, "female", race_controls)), mean)) |>
  print()


# ---- Part 4: residualized leave-out instruments -----------------------------------
# the mean, over the judge's cases of other defendants in the window, of the decision
# residualized on the controls and fixed effects in a regression on all cases (rlo);
# cases with v = NA enter neither the regression nor the means (loo_mean, residualize:
# functions/gen_loo_instruments.R); raw: the unresidualized leave-out mean
rlo <- \(x, v, controls, fe) loo_mean(x, residualize(x, v, controls, fe), window = "3yr", loo_excl_person = TRUE)
raw <- \(x, v) loo_mean(x, x[[v]], window = "3yr", loo_excl_person = TRUE)

# the primary analysis's conviction instrument: non-carceral conviction among the
# convicted cases with a matched score (d1_cond is NA for all other cases), given to
# every case; and the cell of the matched SC event's guidelines offense group (its
# worksheet, 04; "unknown" if not recorded) x its score value
d <- d |>
  mutate(d1_cond = if_else(convict %in% 1 & !is.na(runningvariable), d1, NA_integer_),
         offense_score_cell = if_else(is.na(runningvariable), NA_character_,
                                      str_c(coalesce(na_if(rv_offense_group, ""), "unknown"), "_", runningvariable)))
d <- mutate(d, loo_d1_cond_jyr_rv = rlo(d, "d1_cond", resid_controls, resid_fe_cells))
cat("primary conviction instrument: built from", format(sum(!is.na(d$d1_cond)), big.mark = ","),
    "convicted cases with a matched score;", n_distinct(d$offense_score_cell[!is.na(d$d1_cond)]),
    "guidelines offense group x score cells among them, median cases per cell",
    median(table(d$offense_score_cell[!is.na(d$d1_cond)])), "\n")
print(count(filter(d, !is.na(d1_cond)), rv_offense_group, sort = TRUE), n = Inf)

# UJIVE versions of the primary analysis's instruments (robustness, 07)
d <- d |>
  mutate(loo_d0_jyr_ujive = ujive_loo(d, "d0", resid_controls, resid_fe, window = "3yr"),
         loo_d1_cond_jyr_rv_ujive = ujive_loo(d, "d1_cond", resid_controls, resid_fe_cells, window = "3yr"))

# the secondary analysis's instruments with the predicted-score control, and the
# unresidualized ones (the Revolving Door paper's, 09)
d <- d |>
  mutate(loo_d0_jyr_pred = rlo(d, "d0", resid_controls, resid_fe_pred),
         loo_d1_jyr_pred = rlo(d, "d1", resid_controls, resid_fe_pred),
         loo_d0_jyr_raw = raw(d, "d0"), loo_d1_jyr_raw = raw(d, "d1"), loo_d2_jyr_raw = raw(d, "d2"))

# dismissal, non-carceral conviction and incarceration over all cases (residualized on
# the controls and fixed effects); drops the cases whose unadjusted leave-out mean of a
# decision equals 1, as the original
cat("instruments over all cases: ")
main <- d |>
  gen_loo_instruments(window = "3yr", loo_excl_person = TRUE, resid_controls = resid_controls, resid_fe = resid_fe) |>
  rename(loo_d0_jyr = loo_d0_jdg_yr, loo_d1_jyr = loo_d1_jdg_yr, loo_d2_jyr = loo_d2_jdg_yr)

# by race: over the judge's cases of defendants of the case's race (the judge's leniency
# toward that race), residualized in a regression on the race's cases, without the race
# controls, as race is constant
by_race <- map(c("White", "Black"), \(r) {
  x <- filter(d, race %in% r)
  controls <- setdiff(resid_controls, race_controls)
  tibble(case_group = x$case_group, loo_d0_jyr_race = rlo(x, "d0", controls, resid_fe),
         loo_d1_jyr_race = rlo(x, "d1", controls, resid_fe), loo_d2_jyr_race = rlo(x, "d2", controls, resid_fe),
         loo_d1_cond_jyr_rv_race = rlo(x, "d1_cond", controls, resid_fe_cells),
         loo_d0_jyr_pred_race = rlo(x, "d0", controls, resid_fe_pred),
         loo_d1_jyr_pred_race = rlo(x, "d1", controls, resid_fe_pred),
         loo_d0_jyr_race_ujive = ujive_loo(x, "d0", controls, resid_fe, window = "3yr"),
         loo_d1_cond_jyr_rv_race_ujive = ujive_loo(x, "d1_cond", controls, resid_fe_cells, window = "3yr"))
}) |>
  list_rbind()

# judges with enough cases of each race for the instruments by race: for each case,
# the White and Black cases its judge has in the window (judge x court x years Y-1 to
# Y+1), among the cases a version's instruments are built from
judge_races <- \(pop, min_cases = min_race_cases) {
  cells <- d |> filter(pop, race %in% c("White", "Black")) |> count(judgeid, fipscode, Y, race, name = "cases")
  win <- map(-1:1, \(o) mutate(cells, Y = Y + o)) |>
    list_rbind() |>
    summarise(cases = sum(cases), .by = c(judgeid, fipscode, Y, race)) |>
    summarise(min_n = if (n() == 2) min(cases) else 0L, .by = c(judgeid, fipscode, Y))   # both races, else 0
  left_join(select(d, judgeid, fipscode, Y), win, by = c("judgeid", "fipscode", "Y"), relationship = "many-to-one") |>
    transmute(ok = as.integer(coalesce(min_n, 0L) >= min_cases)) |>
    pull(ok)
}
d <- d |>
  mutate(judge_races_main      = judge_races(rep(TRUE, n())),
         judge_races_secondary = judge_races(!is.na(d1_cond)))
# the primary analysis's race samples under other minimums (robustness, 07)
for (n in min_race_cases_robustness)
  d[[str_c("judge_races_secondary_min", n)]] <- judge_races(!is.na(d$d1_cond), n)

d <- main |>
  left_join(select(d, case_group, starts_with("judge_races_")), by = "case_group", relationship = "one-to-one") |>
  left_join(by_race, by = "case_group", relationship = "one-to-one")

# cases without instruments: the judge has no other defendant's case in the window
cat("cases without the instruments over all cases (dropped):", sum(is.na(d$loo_d1_jyr)), "\n")
d <- filter(d, !is.na(loo_d0_jyr), !is.na(loo_d1_jyr), !is.na(loo_d2_jyr))
cat("White and Black cases without the instruments by race (no other case of a defendant of the same race):",
    sum(d$race %in% c("White", "Black") & is.na(d$loo_d1_jyr_race)), "| convicted with a matched score, without",
    "the primary conviction instrument by race:",
    sum(d$race %in% c("White", "Black") & !is.na(d$d1_cond) & is.na(d$loo_d1_cond_jyr_rv_race)), "\n")
cat("correlation of the residualized leave-out and UJIVE instruments, convicted cases with a matched score: conviction",
    round(cor(d$loo_d1_cond_jyr_rv[!is.na(d$d1_cond)], d$loo_d1_cond_jyr_rv_ujive[!is.na(d$d1_cond)],
              use = "complete.obs"), 3), "| dismissal", round(cor(d$loo_d0_jyr, d$loo_d0_jyr_ujive, use = "complete.obs"), 3),
    "(they differ only by whether the defendant is in the residualizing regression)\n")
cat("White and Black cases of judges with at least", min_race_cases, "cases of each race in the window",
    "(kept in the White and Black samples), by the analysis's construction cases (main: all cases, secondary",
    "analysis; secondary: convicted with a matched score, primary analysis):\n")
list(main = TRUE, secondary = !is.na(d$d1_cond)) |>
  imap(\(pop, vn) d |>
         filter(pop, race %in% c("White", "Black")) |>
         summarise(cases = n(), kept = sum(.data[[str_c("judge_races_", vn)]]), .by = race) |>
         mutate(version = vn, share_kept = round(kept / cases, 3), .before = 1)) |>
  list_rbind() |>
  print()
write_csv(d, file.path(data_dir, "final_judge_iv_analysis.csv"))
cat("final_judge_iv_analysis:", format(nrow(d), big.mark = ","), "cases\n")
