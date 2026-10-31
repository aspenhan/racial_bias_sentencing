# ==============================================================================
# 09_cross_check_revolving_door.R
#
# Cross-check with the "Revolving Door" paper: from our 2SLS estimates to its Table
# E.7 (2SLS for Black and non-Black defendants), as its replication package makes it
# (code/analysis/judge_iv_analysis_convict_and_incar_multinomial.do, block
# make_noncumcum_regs_black, on data/final_judge_iv_data.dta): our estimates (judges
# with at least min_race_cases cases of each race, as the analysis with instruments by
# race), then all judges, then the paper's
# specification on our own analysis sample, one element at a time (ending with pooled
# and then unresidualized instruments: ours are residualized leave-out means, the
# paper's raw ones), then the
# paper's data in turn: its cases (ours matched to them), its outcome, treatment and
# instruments, its race indicator, and its controls, fixed effects, clusters and misc.
# (years of first disposition, and the duplicated case below).
# Outcome: any future felony charge in year 1, years
# 2-4, years 5-7 and years 1-7 after the case's first disposition. Two treatments,
# each instrumented by the judge's leave-out rate of that treatment and controlling
# for the rate of the other margin, as the paper:
#   conviction     non-carceral conviction (d1), instrumented by conviction leniency,
#                  controlling for incarceration leniency
#   incarceration  incarceration (d2), instrumented by incarceration leniency,
#                  controlling for dismissal leniency
# All rows: the cases first disposed in or before 2012, full follow-up for every
# window (full_follow_up_year), as the paper. The paper's specification: Black
# vs non-Black (its own race indicator); raw (unresidualized) pooled leave-out means;
# controls: its prior-conviction flag (SC sentencing event 31 days to 5 years before),
# female, black, any misdemeanor charge, offense type; fixed effects: court x year,
# court x calendar month, day of week; standard errors clustered by judge x year x
# court (Stata ivregress, no small-sample adjustment). Its regressions count each race
# group's 6th case twice: ivregcomplier2 labels the first 15 rows of the data with its
# results (row 6: "E[Y | D = 0, Complier]"), the code copies that row to the end of the
# data (expand 2) and drops the copy with drop if _n == 183382, its position only in
# the full sample, so in each race group the copy stays in the 2SLS that follows. The
# last row reproduces this (the log gives its estimates without the copy). The overlap of the two samples is described (log): the
# replication data's case ids are hashed; they are mapped to the original case groups
# (hash_xwalk), whose charges (court x person x case number, in the original
# judge-IV data) give our cases (cc_case_xwalk, 02). The original case groups are
# formed over the judge-IV charges only and ours over the full circuit-court record,
# so an original case group can span several of our cases, or one of our cases
# several original case groups; only the pairs linked one-to-one are compared.
#
# Inputs:  data_dir/final_judge_iv_analysis.csv, data_dir/cc_case_xwalk.csv
#          paper_data (the replication package's analysis data), hash_xwalk (its
#          hashed case ids -> original case groups), below
#          orig_dir/cases_IV_casenumber_part1only.dta (the original case groups' charges)
# Outputs: table_dir/cross_validation_<conviction|incarceration>.tex
# ==============================================================================

paper_data <- file.path(paper_data_dir, "final_judge_iv_data.dta")
hash_xwalk <- file.path(dirname(orig_dir), "administrative", "hash_croswalk_case_group.dta")
windows <- c("1yr", "2to4yr", "5to7yr", "7yr")
paper_fe <- c("gr_Y_crt", "gr_M_crt", "dow")
stata_ssc <- ssc(adj = FALSE, cluster.adj = TRUE)       # ivregress 2sls, vce(cluster), no -small-

# ---- data: ours; the paper's (prefix p_), with its future-charge outcome by window
# (years 1-7: any of the three windows)
ours <- read_csv(file.path(data_dir, "final_judge_iv_analysis.csv"), show_col_types = FALSE,
                 col_select = c(case_group, person_id, race, black, Y, d0, d1, d2, judge_races_main, judgeid, fipscode,
                                starts_with(c("loo_d0_jyr", "loo_d1_jyr", "loo_d2_jyr")),
                                all_of(unique(c(resid_controls, covariates, resid_fe, paper_fe))), gr_jdg_Y, gr_jdg_Y_crt,
                                all_of(str_c("cc_post", windows, "_charged_all_any"))))
paper <-read_dta(paper_data, col_select = c(case_group, Y, d0, d1, d2, loo_d0_jyr, loo_d1_jyr, loo_d2_jyr, all_of(covariates),
                                             all_of(paper_fe), gr_jdg_Y_crt, recidCC1_F_eva, recidCC2_4_F_eva,
                                             recidCC5_7_F_eva)) |>
  zap_labels() |>
  mutate(recid_1yr = recidCC1_F_eva, recid_2to4yr = recidCC2_4_F_eva, recid_5to7yr = recidCC5_7_F_eva,
         recid_7yr = pmax(recidCC1_F_eva, recidCC2_4_F_eva, recidCC5_7_F_eva), .keep = "unused") |>
  rename_with(\(v) str_c("p_", v))

# ---- overlap of the two samples: the paper's cases -> original case groups -> their
# charges -> our cases
orig_groups <- read_dta(hash_xwalk) |>
  transmute(p_case_group = case_group_hash, orig_group = case_group) |>
  semi_join(paper, by = "p_case_group")
stopifnot(nrow(orig_groups) == nrow(paper))
pairs <- read_dta(file.path(orig_dir, "cases_IV_casenumber_part1only.dta"),
                  col_select = c(case_group, fipscode_raw, person_id, casenumber)) |>
  semi_join(orig_groups, by = c("case_group" = "orig_group")) |>
  distinct(orig_group = case_group, fips = fipscode_raw, person_id, casenumber) |>
  inner_join(read_csv(file.path(data_dir, "cc_case_xwalk.csv"), col_types = cols(casenumber = "c")),
             by = c("fips", "person_id", "casenumber"), relationship = "many-to-one") |>
  distinct(orig_group, case)
cat("the paper's cases whose charges are in:", "one of our cases",
    format(sum(count(pairs, orig_group)$n == 1), big.mark = ","), "| several",
    format(sum(count(pairs, orig_group)$n > 1), big.mark = ","), "| our cases holding several of them:",
    format(sum(count(pairs, case)$n > 1), big.mark = ","), "\n")
link <- pairs |>
  filter(n() == 1, .by = orig_group) |>
  filter(n() == 1, .by = case) |>
  inner_join(orig_groups, by = "orig_group", relationship = "one-to-one")
linked <- ours |>
  inner_join(link, by = c("case_group" = "case"), relationship = "one-to-one") |>
  inner_join(paper, by = "p_case_group", relationship = "one-to-one")
cat("cases linked one-to-one:", format(nrow(linked), big.mark = ","), "of", format(nrow(ours), big.mark = ","),
    "(ours) and", format(nrow(paper), big.mark = ","), "(the paper's)\n")
cat("on the linked cases -- agreement: treatment (d0/d1/d2)",
    round(mean(linked$d0 == linked$p_d0 & linked$d1 == linked$p_d1 & linked$d2 == linked$p_d2), 4),
    "| Black indicator", round(mean(linked$black == linked$p_black), 4), "| year of first disposition",
    round(mean(linked$Y == linked$p_Y), 4), "| future charge, year 1 / 2-4 / 5-7 / 1-7:",
    str_flatten(map_chr(windows, \(w) sprintf("%.4f", mean(linked[[str_glue("cc_post{w}_charged_all_any")]] ==
                                                           linked[[str_glue("p_recid_{w}")]]))), " / "), "\n")
cat("  correlation of the pooled raw leave-out instruments, ours and the paper's: dismissal",
    round(cor(linked$loo_d0_jyr_raw, linked$p_loo_d0_jyr), 3), "| conviction",
    round(cor(linked$loo_d1_jyr_raw, linked$p_loo_d1_jyr), 3),
    "| incarceration", round(cor(linked$loo_d2_jyr_raw, linked$p_loo_d2_jyr), 3), "\n")
cat("  our cases not linked:", format(nrow(ours) - nrow(linked), big.mark = ","), "(Black",
    format(sum(ours$race %in% "Black") - sum(linked$race %in% "Black"), big.mark = ","), ") | the paper's not linked:",
    format(nrow(paper) - nrow(linked), big.mark = ","), "\n")
samples <- list(full = ours, linked = linked, paper = paper)

# ---- estimation --------------------------------------------------------------------
# 2SLS of outcomes ys on treat instrumented by z, controlling for zc (the other
# margin's instrument), ctrl and fixed effects fe; one row per outcome
fit <- \(data, ys, treat, z, zc, ctrl, fe, cl, ssc_) {
  m <- feols(as.formula(str_glue("c({str_flatten(ys, ', ')}) ~ {rhs(c(zc, ctrl))} | {rhs(fe)} | {treat} ~ {z}")),
             data = data, cluster = cl, ssc = ssc_, notes = FALSE)
  term <- str_c("fit_", treat)
  map(if (inherits(m, "fixest_multi")) as.list(m) else list(m),
      \(m) tibble(est = coef(m)[[term]], se = se(m)[[term]], p = pvalue(m)[[term]], n = nobs(m))) |>
    list_rbind()
}

# one step: both race groups x the four windows, all on the cases first disposed in
# or before 2012 (full follow-up for years 1-7)
last_year <- full_follow_up_year(windows)
run_step <- \(sp) {
  data <- samples[[sp$sample]]
  if (sp$restrict) data <- filter(data, judge_races_main == 1)   # judges with enough cases of each race
  groups <- if (sp$groups == "ours") {
    list(white = filter(data, race %in% "White"), black = filter(data, race %in% "Black"))
  } else {
    list(white = filter(data, p_black == 0), black = filter(data, p_black == 1))   # non-Black, Black
  }
  imap(groups, \(g, group) {
    x <- filter(g, .data[[sp$year_var]] <= last_year)
    if (sp$dup6) x <- bind_rows(x, x[6, ])     # as the replication code
    fit(x, str_glue(sp$y, w = windows), sp$treat, sp$z, sp$zc, sp$ctrl, sp$fe, sp$cl, sp$ssc) |>
      mutate(window = windows, group = group)
  }) |>
    list_rbind()
}

# the steps, from ours to the paper's, each changing one element of the previous one
# (names: row labels of the tables); tr: the treatment's variables. Our estimates
# keep, as the analysis with instruments by race, the judges with at least
# min_race_cases cases of each race in the window; then all judges; then the paper's
# specification on our own analysis sample, ending with its pooled instruments, then
# the paper's data
steps_for <- \(tr) {
  base <- list(sample = "full", year_var = "Y", groups = "ours", treat = tr$treat,
               z = tr$z_race, zc = tr$zc_race, ctrl = setdiff(resid_controls, race_controls), fe = resid_fe,
               cl = iv_cluster, ssc = ssc(), y = "cc_post{w}_charged_all_any", dup6 = FALSE, restrict = TRUE)
  changes <- list(
    "Our estimates"                                 = list(),
    "All judges"                                    = list(restrict = FALSE),
    # the paper's prior-conviction control: its felony flag from the SC records only,
    # and no misdemeanor-only flag
    "Same prior conviction controls"                = list(ctrl = setdiff(covariates, race_controls)),
    "Day-of-week FE"                                = list(fe = paper_fe),
    # the paper's judge x year x court clusters are the judge x year cells here: this
    # drops the defendant dimension (and the small-sample adjustment)
    "Clustered by judge $\\times$ year only"        = list(cl = ~gr_jdg_Y_crt, ssc = stata_ssc),
    "Pooled instruments"                            = list(z = tr$z_pooled, zc = tr$zc_pooled),
    # (the paper's specification on our sample: its instruments are raw leave-out means)
    "Unresidualized instruments"                    = list(z = tr$z_raw, zc = tr$zc_raw),
    # the paper's data, in turn: its cases (ours matched to them, with our variables),
    # its outcome, treatment and raw leave-out instruments, its race indicator (Black vs
    # non-Black), and the rest -- its own data file, which also adds its cases not
    # matched to ours, with its controls, fixed-effect cells, clusters and years, and
    # each race group's 6th case counted twice (Table E.7 exactly)
    "Paper's cases"                                 = list(sample = "linked"),
    "Paper's outcome, treatment and instruments"    = list(y = "p_recid_{w}", treat = str_c("p_", tr$treat),
                                                           z = str_c("p_", tr$z_pooled), zc = str_c("p_", tr$zc_pooled)),
    "Paper's race indicator"                        = list(groups = "paper"),
    "Paper's controls, FE, clusters and misc."      = list(sample = "paper", year_var = "p_Y",
                                                           ctrl = str_c("p_", setdiff(covariates, race_controls)),
                                                           fe = str_c("p_", paper_fe), cl = ~p_gr_jdg_Y_crt, dup6 = TRUE))
  accumulate(changes, \(spec, change) modifyList(spec, change), .init = base)[-1]
}

treatments <- list(
  conviction    = list(label = "Non-carceral conviction", treat = "d1", z_race = "loo_d1_jyr_race",
                       zc_race = "loo_d2_jyr_race", z_pooled = "loo_d1_jyr", zc_pooled = "loo_d2_jyr",
                       z_raw = "loo_d1_jyr_raw", zc_raw = "loo_d2_jyr_raw",
                       instruments = "conviction leniency, controlling for incarceration leniency"),
  incarceration = list(label = "Incarceration", treat = "d2", z_race = "loo_d2_jyr_race", zc_race = "loo_d0_jyr_race",
                       z_pooled = "loo_d2_jyr", zc_pooled = "loo_d0_jyr", z_raw = "loo_d2_jyr_raw", zc_raw = "loo_d0_jyr_raw",
                       instruments = "incarceration leniency, controlling for dismissal leniency"))

# ---- tables ----------------------------------------------------------------------
# rows: steps (coefficient, (se), {observations}); columns: White (non-Black from the
# paper's race groups on) and Black x windows
window_labels <- c("Year 1", "Years 2--4", "Years 5--7", "Years 1--7")
iwalk(treatments, \(tr, tn) {
  steps <- steps_for(tr)
  res <- imap(steps, \(sp, step) run_step(sp) |> mutate(step = step)) |>
    list_rbind() |>
    mutate(step = factor(step, levels = names(steps)), group = factor(group, levels = c("white", "black")),
           window = factor(window, levels = windows)) |>
    arrange(step, group, window)
  cat(str_glue("\n{tr$label}: future felony charge, from our estimates to Table E.7 of the paper"), "\n")
  res |>
    mutate(cell = sprintf("%6.3f%-3s(%.3f) n=%d", est, str_remove_all(stars(p), "[\\^{}]"), se, n)) |>
    select(step, group, window, cell) |>
    pivot_wider(names_from = c(group, window), values_from = cell) |>
    print(n = Inf, width = Inf)
  # the last row without the duplicated 6th case
  d <- res |>
    filter(step == last(names(steps))) |>
    mutate(across(c(group, window), as.character)) |>
    inner_join(run_step(modifyList(last(steps), list(dup6 = FALSE))), by = c("group", "window"), suffix = c("", "_nodup"))
  cat("  last row without the duplicated 6th case: largest change in coefficient", sprintf("%.4f", max(abs(d$est - d$est_nodup))),
      "| in standard error", sprintf("%.4f", max(abs(d$se - d$se_nodup))), "| observations:",
      str_flatten(unique(str_c(d$group, " ", d$n_nodup)), ", "), "\n")

  first_paper_groups <- which(names(steps) == "Paper's race indicator")
  body <- imap(split(res, res$step), \(r, step) {
    cells <- coef_cells(r$est, r$se, r$p)
    c(tex_row(str_c("(", match(step, names(steps)), ") ", step), cells$est),
      tex_row("", cells$se), tex_row("", str_c("\\{", fmt(r$n, 0), "\\}")))
  }) |> unlist()
  write_tex(
    file.path(table_dir, str_glue("cross_validation_{tn}.tex")), "lcccccccc",
    header = c(str_c(" & \\multicolumn{4}{c}{White (non-Black from (", first_paper_groups, "))} & ",
                     "\\multicolumn{4}{c}{Black} \\\\"),
               "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}",
               tex_row("", rep(window_labels, 2))),
    body = body,
    notes = str_glue(
      "{tr$label} (2SLS) on any future felony charge in year 1, years 2-4, years 5-7 and years 1-7 after the case's ",
      "first disposition, instrumented by {tr$instruments}: from our estimates (1) to Table E.7 of the Revolving Door ",
      "paper ({length(steps)}), as its replication package makes it, each row changing one element of the row above; ",
      "coefficient, (standard error), {{observations}}. (1): instruments built within the defendant's race; controls: ",
      "offense type, any misdemeanor charge, prior felony conviction (circuit-court record or Sentencing Commission) ",
      "and prior misdemeanor-only conviction between 31 days and 5 years before, female; fixed effects: court x year, ",
      "court x calendar month, court x day of week; standard errors {iv_cluster_note}; ",
      "only the cases of judges with at least {min_race_cases} cases of White and {min_race_cases} of Black defendants ",
      "in the case's 3-year window, as the analysis with instruments by race; (2) all judges. ",
      "All rows on the cases first disposed in or before {last_year} (full follow-up for every window). Paper's cases: ours ",
      "matched one-to-one to the paper's (hashed case ids -> original case groups -> their charges -> our cases); ",
      "({length(steps)}): the paper's own data, which also adds its cases not matched to ours, its controls, FE cells, ",
      "clusters and years, and each race group's 6th case counted twice, as the replication code does."))
})
