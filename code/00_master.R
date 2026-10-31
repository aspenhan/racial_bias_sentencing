# ==============================================================================
# 00_master.R
#
# Judge (leave-one-out) instruments for dismissal, noncarceral
# conviction and incarceration, following the pipeline in
#   Impacts of incarceration/code/Cecile code/replication   (judge model 17)
# on judge-IV cases rebuilt from the full circuit-court record (cases grouped over
# all charges; treatment and judge from the felony charges). The instruments are JIVE
# ones: the judge's leave-out rate of a decision over their cases of other defendants in
# the case's 3-year window (leaving out all of the defendant's cases), with the controls
# and fixed effects in the 2SLS (Angrist, Imbens and Krueger 1999; Goldsmith-Pinkham,
# Hull and Kolesar 2026). Each case is matched to its own Sentencing Commission (SC)
# sentencing event to get its guidelines sentencing score (runningvariable). Instruments
# (06):
#   loo_d0_jyr, loo_d1_jyr, loo_d2_jyr: dismissal, non-carceral conviction and
#               incarceration rates over all cases
#   loo_d1_cond_jyr_rv: non-carceral conviction rate over the convicted cases with a
#               matched score
#   (all also by race, suffix _race: over the cases of defendants of the case's race, for
#   the White and Black samples, which keep only the cases of judges with at least
#   min_race_cases cases of each race in the case's 3-year window)
#   For the primary analysis's robustness check (07), the dismissal and conviction
#   instruments are also built as UJIVE ones, residualized on the controls and fixed
#   effects with each defendant left out of the residualization as well as of the
#   judge's mean (functions/ujive.R): suffix _ujive
#
# Run from anywhere inside the project (open racial_bias_sentencing.Rproj):
#   source("code/00_master.R")
# or from a terminal in the project root:
#   Rscript code/00_master.R
#
# Original data are read in place from orig_dir and never modified; the one
# input kept in data/ is vcc_2026_active_retired.xlsx (Virginia Crime Codes,
# for the supervision-violation code sections of step 02). Datasets are written
# to data/ as csv:
#   cc_sc_id_xwalk.csv           CC person_id <-> SC id crosswalk        (step 01)
#   cc_sc_for_merge.csv          CC person x SC sentencing event         (step 01)
#   cc_cases_full.csv            all CC cases 2000-2019 x charge class (felony, misdemeanor): offense types,
#                                outcome, conviction/incarceration types (step 02)
#   cc_cases_recid.csv           the same, felony charges without revocation proceedings and
#                                technical hearings: new cases of the recidivism outcomes (step 02)
#   cc_revocations_full.csv      all CC revocation proceedings           (step 02)
#   cc_felony_charges.csv        felony charges, not revocations: the
#                                charges of the judge-IV cases           (step 02)
#   cc_case_xwalk.csv            court x person x case number -> case    (step 02)
#   judge_iv_cases.csv           judge-IV cases: treatment, judge, dates,
#                                offense types                           (step 03)
#   cc_person_level_data.csv     + score, race, sex, age, prior record,
#                                recidivism (recid_windows below)        (step 04)
#   score_predictions.csv        predicted score of every case           (step 05)
#   final_judge_iv_analysis.csv  analysis data with all instrument sets  (step 06)
# Analysis (primary and secondary analyses), LaTeX tables to tables/, figures to
# figures/:
#   07_judge_iv_regressions.R    first stages and 2SLS, by race
#   08_judge_iv_validations.R    balance and monotonicity tests; recidivism and sentence
#                                length of the incarcerated
#   09_cross_check_revolving_door.R        step by step from our estimates to
#                                Table E.7 of the Revolving Door paper
#   10_revocation_judges.R       who decides the later revocations and new cases;
#                                revocation decisions and the judge's leniency
#   11_data_notes.R              tables of the data notes (data_notes.tex, section
#                                "Racial shares in the sample"), from the record to the samples
# 99_checks.R compares the data with the original model-17 data.
# ==============================================================================

# packages (pacman installs any that are missing)
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, haven, readxl, fixest, here, ranger, Matrix)
invisible(Sys.setlocale("LC_TIME", "C"))     # English month names: CC dates are "01jan2005" (02)
# the controls enter every regression as fixed effects (iv_2sls.R fe_of): demean to a tight
# tolerance, so that estimates match those with the controls as regressors
setFixest_estimation(fixef.tol = 1e-10, fixef.iter = 1e5)

# ---- paths --------------------------------------------------------------------
here::i_am("code/00_master.R")
orig_dir  <- "C:/Users/xyh2/Yale University Dropbox/Aspen Han/Impacts of incarceration/data_new"
# data of the Revolving Door paper's replication package (09, 11)
paper_data_dir <- file.path(dirname(orig_dir), "administrative", "submission_07_14_25_sent",
                            "replication_code_and_data", "data")
# Virginia county/city fips -> judicial circuit
circuit_xwalk_path <- "C:/Users/xyh2/Yale University Dropbox/Aspen Han/Alternative credit scores/OR_AH_codebase/int_dat/crosswalk_circuit_county.dta"
code_dir  <- here("code")
data_dir  <- here("data")
log_dir   <- here("logs")
table_dir <- here("tables")
fig_dir   <- here("figures")
walk(c(data_dir, log_dir, table_dir, fig_dir), \(path) dir.create(path, recursive = TRUE, showWarnings = FALSE))

# ---- specification ------------------------------------------------------------
# covariates of the original first stages (used in 99_checks.R)
covariates <- c("flag_prior_conviction", "female", "black", "any_mis", "any_assault",
                "any_burglary", "any_drugs", "any_fraud", "any_kidnapping", "any_larceny",
                "any_miscellaneous", "any_murder", "any_robbery", "any_sex_assault",
                "any_sex_offense", "any_traffic", "any_weapon")

# Controls of the IV analysis (and partialled out of the treatments in the UJIVE
# instruments):
#   controls (entered as fixed effects, one indicator per value, in every residualization
#     and regression; user decision 2026-10-26): offense-category dummies of the original covariate
#     list and any misdemeanor charge in the case (any_mis); prior record between
#     31 days and 5 years before the case's first offense, as the original (04): any
#     felony conviction (CC record from 2000, or the person's SC sentencing events,
#     from 1995), and any misdemeanor conviction (CC) but no felony conviction; sex
#     (female); race, Black vs non-Black (pooled instruments only: constant within
#     the instruments built by race)
#   fixed effects (absorbed): court x year, court x calendar month, court x day of
#     week; plus, in the primary version, one indicator per cell of the guidelines offense
#     group of the case's own SC sentencing event (its worksheet, on which the score is
#     computed) x its score value (offense_score_cell, 06: saturated in offense and score;
#     user decisions 2026-10-17, 2026-10-25), and in the secondary version with the score control the predicted
#     score rounded to an integer (rv_pred_value): one indicator per value, as the
#     outcomes are not linear in the score
# (no age: it comes from the SC records, which exist mostly for convicted defendants)
offense_type <- c("any_assault", "any_burglary", "any_drugs", "any_fraud",
                  "any_kidnapping", "any_larceny", "any_miscellaneous", "any_murder",
                  "any_robbery", "any_sex_assault", "any_sex_offense", "any_traffic",
                  "any_weapon")
prior_record  <- c("flag_prior_fel_conv", "flag_prior_mis_only_conv")
race_controls <- "black"                                              # vs non-Black
resid_controls      <- c(offense_type, "any_mis", prior_record, "female", race_controls)
resid_fe            <- c("gr_yr_crt", "gr_m_crt", "gr_dow_crt")       # secondary
resid_fe_cells      <- c(resid_fe, "offense_score_cell")              # primary: guidelines offense group x score value
resid_fe_pred       <- c(resid_fe, "rv_pred_value")                   # secondary with score: + predicted score value

# model-17 sample (06, 11), as the original: cases first disposed by 2018 (Virginia
# Beach, fips 810: by 2009) in the original's 19 courts (circuit 5, fips 999, is one
# court), of judges with at least 100 cases in the court in the case's 3-year window,
# in court-years with at least 2 such judges, in courts left with at least 3 years
# (functions/subset_judgeiv_data.R)
model17_years  <- \(d) filter(d, Y <= 2018, !(fipscode %in% 810 & Y >= 2010))
model17_sample <- list(court_fipscode_list  = c(13, 41, 87, 107, 153, 155, 169, 177, 179, 185, 195, 550,
                                                700, 710, 740, 760, 770, 810, 999),
                       case_count_cutoff    = 100,
                       case_count_window    = "3yr",
                       min_judges_window    = "Y",
                       min_judges_per_court = 2,
                       court_yr_min         = 3)

# White and Black samples (race-specific instruments): judges with at least this many
# cases of each race in the case's 3-year window (06); robustness of the primary analysis
# (07): no minimum, 25 and 100 (06 flags judge_races_secondary_min<n>)
min_race_cases <- 50
min_race_cases_robustness <- c(0, 25, 100)

# seed for random tiebreaks in the SC-CC case match (04) and the score model (05)
match_seed <- 123                                # every seed is 123 (user decision 2026-10-26)

# instrument versions analysed in 07-08: conviction (non-carceral conviction)
# and dismissal instruments, and the construction controls and fixed effects to
# include (convicted_only: the version compares non-carceral with carceral sentences
# among the convicted cases with a matched score, the only ones that enter it;
# z_conv_race, z_dism_race: the instruments built within the defendant's race, used for
# the White and Black samples; race_sample: 1 for the cases of judges with enough cases
# of each race, which the White and Black samples keep, 06)
#   primary          convicted cases with a matched score, one indicator per guidelines
#                    offense group x score value
#   secondary        all cases (no score)
#   secondary_score  all cases, one indicator per rounded value of the predicted score
#                    (05), the same prediction for every case (instruments residualized
#                    with the predicted-score indicators as well: loo_d*_jyr_pred, 06)
iv_versions <- list(
  primary         = list(label = "Primary", z_conv = "loo_d1_cond_jyr_rv", z_dism = "loo_d0_jyr",
                         controls = resid_controls, fe = resid_fe_cells, convicted_only = TRUE,
                         z_conv_race = "loo_d1_cond_jyr_rv_race", z_dism_race = "loo_d0_jyr_race",
                         race_sample = "judge_races_secondary"),
  secondary       = list(label = "Secondary", z_conv = "loo_d1_jyr", z_dism = "loo_d0_jyr", controls = resid_controls,
                         fe = resid_fe, convicted_only = FALSE, z_conv_race = "loo_d1_jyr_race",
                         z_dism_race = "loo_d0_jyr_race", race_sample = "judge_races_main"),
  secondary_score = list(label = "Secondary, predicted score", z_conv = "loo_d1_jyr_pred", z_dism = "loo_d0_jyr_pred",
                         controls = resid_controls, fe = resid_fe_pred, convicted_only = FALSE,
                         z_conv_race = "loo_d1_jyr_pred_race", z_dism_race = "loo_d0_jyr_pred_race",
                         race_sample = "judge_races_main"))
# the analyses of the tables (07-08): the primary one, and the secondary one without and
# with the predicted-score control, side by side; names: versions, values: column labels
analyses <- list(primary   = c(primary = "Primary"),
                 secondary = c(secondary = "No score", secondary_score = "Predicted score"))
analysis_label <- c(primary = "Primary", secondary = "Secondary")
# robustness checks of the primary analysis (07): the primary version with these changes
#   ujive     UJIVE instruments (06), residualized on the controls and fixed effects with
#             the defendant left out of the residualization
#   min<n>    the White and Black samples restricted to judges with at least n cases of
#             each race (min_race_cases_robustness) instead of min_race_cases
robustness <- list(
  ujive = list(label = "UJIVE", z_conv = "loo_d1_cond_jyr_rv_ujive", z_dism = "loo_d0_jyr_ujive",
               z_conv_race = "loo_d1_cond_jyr_rv_race_ujive", z_dism_race = "loo_d0_jyr_race_ujive"))
robustness_min_cases <- set_names(
  map(min_race_cases_robustness, \(n) list(label = if (n == 0) "No minimum" else str_c("At least ", n, " cases"),
                                           race_sample = str_c("judge_races_secondary_min", n))),
  str_c("min", min_race_cases_robustness))
# the fixed effects of a version, for table notes
fe_note <- \(v) str_c("court x year, court x calendar month, court x day of week",
                      if ("runningvariable" %in% v$fe) ", sentencing score (one indicator per value)" else "",
                      if ("offense_score_cell" %in% v$fe)
                        ", guidelines offense group x sentencing score (one indicator per cell)" else "",
                      if ("rv_pred_value" %in% v$fe) ", predicted sentencing score (one indicator per rounded value)" else "")
# the cases a version is built from and estimated on, and the restriction of its
# White and Black samples, for table notes
cases_note <- \(v) if (!v$convicted_only) "cases" else if (any(c("runningvariable", "offense_score_cell") %in% v$fe))
  "convicted cases with a matched score" else "convicted cases"
race_sample_note <- \(v) str_c(
  " White and Black defendants: only the cases of judges with at least ", min_race_cases, " ", cases_note(v),
  " of White and ", min_race_cases, " of Black defendants in the case's 3-year window.")
# every instrument variable, score fixed effect and race-sample flag of the analysed
# versions and robustness checks (columns read by 07-08)
iv_instruments <- unique(unlist(c(
  map(iv_versions, \(v) c(v[c("z_conv", "z_dism", "z_conv_race", "z_dism_race", "race_sample")], setdiff(v$fe, resid_fe))),
  map(c(robustness, robustness_min_cases),
      \(r) c(r[intersect(names(r), c("z_conv", "z_dism", "z_conv_race", "z_dism_race", "race_sample"))],
             setdiff(r$fe, resid_fe))))))

# a version in a sample ("all", "white", "black"): for White and Black defendants,
# the instruments built by race, if the version has them, and no race controls
# (race is constant). All defendants (as Arnold, Dobbie and Yang 2018; user decision
# 2026-10-25): White and Black defendants together, each with the conviction and
# dismissal instruments built within their race (one coefficient each; user decision
# 2026-10-26), and every control and fixed effect specific to each race (columns
# <name>_bk = <name> within race, version_samples), so that the coefficient on the
# conviction instrument is a weighted average of the White and Black ones. The controls
# enter every regression as fixed effects (iv_2sls.R fe_of)
for_sample <- \(v, s) {
  if (!s %in% c("all", "white", "black") || is.null(v$z_conv_race) && s == "all") return(v)
  v$controls <- setdiff(v$controls, race_controls)
  if (!is.null(v$z_conv_race)) v <- modifyList(v, list(z_conv = v$z_conv_race, z_dism = v$z_dism_race))
  if (s == "all") v <- modifyList(v, list(controls = str_c(v$controls, "_bk"), fe = str_c(v$fe, "_bk")))
  v
}

# standard errors of the IV analysis (07-09): clustered by defendant, the unit the
# instruments leave out (a defendant's cases share their outcomes); cases are assigned to
# judges case by case within court and period, so assignment adds no other clusters
# (user decision 2026-10-16; was two-way by defendant and judge x year)
iv_cluster <- ~person_id
iv_cluster_note <- "clustered by defendant"

# follow-up windows of the recidivism outcomes cc_post<window>_... (04, 07, 99): after
# the case's first disposition, from year `from` (exclusive) to year `to` (inclusive),
# years of 365 days. The CC record ends in cc_last_year, so the analyses of a set of
# windows (07-09, 99) keep the cases first disposed in or before cc_last_year minus
# the longest window's `to` (full_follow_up_year): full follow-up for every outcome,
# on the same cases across outcomes (1-3 years: 2016; years 1-7: 2012)
recid_windows <- tribble(
  ~window,  ~from, ~to, ~label,
  "1yr",        0,   1, "1 year",
  "2yr",        0,   2, "2 years",
  "3yr",        0,   3, "3 years",
  "2to4yr",     1,   4, "Years 2--4",
  "5to7yr",     4,   7, "Years 5--7",
  "7yr",        0,   7, "Years 1--7")
cc_last_year <- 2019
full_follow_up_year <- \(windows) cc_last_year - max(recid_windows$to[match(windows, recid_windows$window)])

# outcomes measured from release (04, 07): within 1, 2 and 3 years after release from
# the focal sentence, estimated as the first disposition plus release_share of the
# active carceral time (as the Revolving Door replication code; credit for pretrial
# detention is not known), with time served capped at release_caps years (user choice: 4)
# so that every case keeps a release date; the analyses keep the cases first disposed by
# cc_last_year - 3 - cap (full follow-up for 3 years after release: 2012); robustness of
# the primary analysis (07): caps of 3 and 5 years (cases first disposed by 2013 and 2011)
release_share <- 0.9
release_caps  <- 4
release_caps_robustness <- c(3, 5)

walk(c("subset_judgeiv_data.R", "gen_loo_instruments.R", "offense_types.R", "cc_case_groups.R", "tex_tables.R",
       "iv_2sls.R", "ujive.R", "person_demog.R", "revocation_kinds.R"),
     \(file) source(file.path(code_dir, "functions", file)))

# ---- run ----------------------------------------------------------------------
steps <- c("01_merging_ids_cc_sc.R", "02_cc_cases_full.R", "03_judge_iv_cases.R", "04_merge_matched_ids.R",
           "05_predict_score.R", "06_final_judge_iv_data_clean.R", "07_judge_iv_regressions.R",
           "08_judge_iv_validations.R", "09_cross_check_revolving_door.R", "10_revocation_judges.R", "11_data_notes.R",
           "99_checks.R")

log_con <- file(file.path(log_dir, "pipeline_r.log"), open = "wt")
sink(log_con, split = TRUE)
tryCatch({
  cat("R pipeline started", format(Sys.time()), "\n")
  walk(steps, \(step) {
    cat("\n==================== ", step, " ====================\n", sep = "")
    t0 <- Sys.time()
    # each step in its own environment (its objects are freed afterwards); warnings
    # and errors are also written to the log
    withCallingHandlers(source(file.path(code_dir, step), local = new.env()),
                        warning = \(w) cat("  warning:", conditionMessage(w), "\n"),
                        error   = \(e) cat("  ERROR:", conditionMessage(e), "\n"))
    gc()
    cat(sprintf("[%s done in %.1f min]\n", step, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  })
  cat("\nR pipeline finished", format(Sys.time()), "\n")
}, finally = {
  sink()
  close(log_con)
})
