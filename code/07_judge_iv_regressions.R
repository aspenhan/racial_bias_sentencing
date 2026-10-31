# ==============================================================================
# 07_judge_iv_regressions.R
#
# First stages and 2SLS of the judge IV (iv_versions and analyses, 00_master.R).
# Treatment: non-carceral conviction (d1). Two analyses:
#   primary    convicted cases with a matched score: conviction instrument = the
#              judge's leave-out rate of non-carceral conviction among them
#              (loo_d1_cond_jyr_rv), one indicator per score value
#   secondary  all cases: conviction instrument = the judge's leave-out rate of
#              non-carceral conviction (loo_d1_jyr), shown side by side without and with
#              the predicted-score control (secondary_score: instruments and regressions
#              with one indicator per rounded predicted score)
# Dismissal instrument: the judge's leave-out dismissal rate (loo_d0_jyr). All
# instruments are residualized leave-out means (06): the mean, over the judge's cases of
# other defendants in the 3-year window, of the decision residualized on the controls and
# fixed effects. The White and Black samples
# use the instruments built within the defendant's race (suffix _race) and keep only the
# cases of judges with at least min_race_cases cases of each race in the 3-year window
# (race_sample, 06); all defendants use the pooled ones. Every 2SLS controls for the
# dismissal instrument; the first stages are shown without and with it. Every regression
# includes the controls and fixed effects: offense type,
# any misdemeanor charge, prior felony conviction, prior misdemeanor-only conviction (31
# days to 5 years before), female, Black (all defendants only; for_sample, 00_master.R);
# court x year, court x calendar month and court x day-of-week fixed effects, plus one per
# value of the matched score (primary) or of the rounded predicted score (secondary with
# the score control); standard errors clustered by defendant (iv_cluster).
#   First stages  d1 on the conviction instrument, without and with the dismissal
#                 instrument; column groups all, White and Black defendants, and within
#                 each, for the secondary analysis, without and with the score control
#                 (all defendants: White and Black defendants with the race-specific
#                 instruments and race-specific controls and fixed effects, for_sample)
#   Figures       per version and sample: distribution of the residualized conviction
#                 instrument (histogram, 1st-99th percentiles) and a local linear
#                 regression (Epanechnikov kernel, bandwidth = SD of the instrument;
#                 fitted on the 0.1st-99.9th percentiles, shown on the 1st-99th) of
#                 residualized d1 on it with its 95% confidence interval (clustered as the
#                 regressions); and conviction against dismissal leniency (the two
#                 instruments residualized on the controls and fixed effects and averaged
#                 by judge x year x court, case-weighted linear fit)
#   2SLS          recidivism on d1 instrumented by the conviction instrument,
#                 controlling for the dismissal instrument; White and Black defendants
#                 and the White - Black difference from a fully interacted pooled
#                 regression; for the secondary analysis, the version without and with the
#                 score control as the outer column groups. Outcomes: any new felony CC
#                 case charged / convicted / incarcerated within 1, 2 and 3 years after the
#                 case's first disposition (cc_post<k>yr_<status>_all_any), the same for
#                 revocations of the person's other felony sentences set off after the
#                 case's first disposition (cc_post<k>yr_revother_..., the primary
#                 revocation measure; 04) and of the sentences imposed at the focal hearing
#                 (cc_post<k>yr_revfocal_..., secondary); and the same outcomes after
#                 release from the focal sentence (cc_rel<cap>_..., 04: first disposition +
#                 release_share x active carceral time, capped at cap years), on the cases
#                 first disposed by cc_last_year - 3 - cap (full follow-up after release)
#   Parts         per analysis (secondary: without the score control), revocation
#                 proceedings within 1 and 3 years of the disposition and of release: the
#                 two measures and their parts (filed under the focal case; under another
#                 case sentenced at the focal hearing; later sentences; earlier or
#                 unidentified ones after a new case)
#   Comparisons   primary and secondary (no score), the outcomes of the 2SLS tables: OLS
#                 (on d1 and d0, so that d1 compares non-carceral conviction with
#                 incarceration)
#   Robustness    primary analysis (robustness, robustness_min_cases,
#                 release_caps_robustness, 00_master.R): first stages and the 2SLS of the
#                 key outcomes, with UJIVE instruments, with other minimums of cases of
#                 each race per judge, and with other caps on time served; the primary
#                 revocation measure without revocation-only case numbers; and the standard
#                 errors of key outcomes under other clusterings
#   The 2SLS of a table are on the same cases for all its outcomes, those with full
#   follow-up in the CC record (ends 2019) for the longest window: first disposed in
#   or before 2016. The first stages and figures are on all analysis cases.
#   (The 2SLS restricted by incarceration length are kept in archive/, not run.)
#
# Input:   data_dir/final_judge_iv_analysis.csv
# Outputs: table_dir/first_stage_<analysis>.tex, first_stage_primary_robustness.tex
#          table_dir/iv_<analysis>_<new_cases|revocations_other|revocations_focal>.tex
#          table_dir/iv_<analysis>_<new_cases|revocations_other|revocations_focal>_from_release_cap<cap>.tex
#          table_dir/iv_<analysis>_revocation_parts.tex
#          table_dir/iv_primary_revocations_other_no_revonly[_from_release].tex
#          table_dir/iv_primary_release_caps[_same_cases].tex
#          table_dir/iv_<primary|secondary>_<new_cases|revocations_other|revocations_focal>_ols.tex
#          table_dir/first_stage_primary_<robustness|min_cases>.tex,
#          table_dir/iv_primary_<robustness|min_cases>[_from_release].tex, iv_primary_release_caps.tex
#          table_dir/iv_primary_clusters.tex (standard errors under other clusterings)
#          fig_dir/first_stage_<version>_<all|white|black>.pdf,
#          fig_dir/leniency_scatter_<version>_<all|white|black>.pdf (one figure per sample)
# ==============================================================================

d <- read_csv(file.path(data_dir, "final_judge_iv_analysis.csv"), guess_max = Inf, show_col_types = FALSE,
              col_select = c(case_group, person_id, race, Y, month, d0, d1, d2, carceral_length_inbtw, runningvariable, judgeid,
                             gr_jdg_Y, gr_jdg_Y_crt, all_of(c(resid_fe, resid_controls, iv_instruments)),
                             matches(str_c("^cc_(rel[0-9]+_)?post[0-9to]+yr_",
                                           "(revother_|revfocal_|revfcase_|revfsame_|revlater_|revearly_|revothnr_)?",
                                           "(charged|conv|incar)_all_any$"))))
race_note <- \(v) if (is.null(v$z_conv_race)) "" else
  str_c(str_glue(" The instruments built within the defendant's race ({v$z_conv_race}, {v$z_dism_race}). All ",
                 "defendants: White and Black defendants together, each with their race's instruments (one ",
                 "coefficient each) and every control and fixed effect specific to each race, so that the ",
                 "coefficient on conviction leniency is a weighted average of the White and Black ones (Arnold, ",
                 "Dobbie and Yang 2018)."), race_sample_note(v))
sample_labels <- c(all = "All defendants", white = "White defendants", black = "Black defendants")
samples <- names(sample_labels)
# the controls of a version, for the table notes (race: all defendants only)
controls_note <- \(v) str_c(str_flatten(setdiff(v$controls, race_controls), ", "), " (as fixed effects)")
# the samples of each version (all, White, Black)
version_data <- map(iv_versions, \(v) version_samples(v, d))


# ---- first stages -------------------------------------------------------------------
first_stage <- \(v, data, with_dism) {
  feols(as.formula(str_glue("d1 ~ {rhs(v$z_conv, if (with_dism) v$z_dism)} | {fe_of(v)}")),
        data = data, cluster = iv_cluster, notes = FALSE)
}
cell <- \(m, z) if (z %in% names(coef(m))) coef_cells(coef(m)[[z]], se(m)[[z]], pvalue(m)[[z]]) else list(est = "", se = "")

# one table per analysis: column groups all, White and Black defendants; within each,
# the analysis's versions (secondary: without and with the score control), each without
# and with the dismissal instrument
iwalk(analyses, \(vns, an) {
  fits <- expand_grid(sample = samples, vn = names(vns), with_dism = c(FALSE, TRUE)) |>
    mutate(v_s = map2(vn, sample, \(vn, s) for_sample(iv_versions[[vn]], s)),
           m   = pmap(list(v_s, vn, sample, with_dism), \(v_s, vn, s, w) first_stage(v_s, version_data[[vn]][[s]], w)))
  conv <- map2(fits$m, fits$v_s, \(m, v_s) cell(m, v_s$z_conv))
  dism <- map2(fits$m, fits$v_s, \(m, v_s) cell(m, v_s$z_dism))
  fstat <- map2_dbl(fits$m, fits$v_s, \(m, v_s) wald(m, keep = str_c("^", v_s$z_conv, "$"), print = FALSE)$stat)
  cat(str_glue("\n{analysis_label[[an]]} first stages (d1): conviction instrument coef / F"), "\n")
  print(tibble(sample = fits$sample, version = fits$vn, with_dismissal = fits$with_dism,
               coef = map2_dbl(fits$m, fits$v_s, \(m, v_s) coef(m)[[v_s$z_conv]]), F = round(fstat, 1)), n = Inf)
  k <- 2 * length(vns)
  header <- c(group_header(c("All", "White", "Black"), k),
              if (length(vns) > 1) group_header(rep(unname(vns), 3), 2),
              tex_row("", str_c("(", seq_len(3 * k), ")")))
  v1 <- iv_versions[[names(vns)[1]]]
  write_tex(
    file.path(table_dir, str_glue("first_stage_{an}.tex")), str_c("l", strrep("c", 3 * k)),
    header = header,
    body = c(tex_row("Conviction leniency", map_chr(conv, "est")), tex_row("", map_chr(conv, "se")),
             tex_row("Dismissal leniency", map_chr(dism, "est")), tex_row("", map_chr(dism, "se")),
             "\\midrule",
             tex_row("F-statistic, conviction leniency", fmt(fstat, 1)),
             tex_row("Mean of dependent variable", fmt(map_dbl(fits$m, \(m) fitstat(m, "my")[[1]]))),
             tex_row("Observations", fmt(map_dbl(fits$m, nobs), 0)),
             tex_row("Dismissal leniency control", yes_no(fits$with_dism)),
             if (length(vns) > 1) tex_row("Predicted-score control", yes_no(fits$vn != names(vns)[1]))),
    notes = str_glue("{analysis_label[[an]]} instruments. First stages: non-carceral conviction regressed on the ",
                     "conviction instrument ({v1$z_conv}), without and with the dismissal instrument ({v1$z_dism}). ",
                     "Controls: {controls_note(v1)}; fixed effects: {fe_note(v1)}.{score_note(vns)} ",
                     "{if (v1$convicted_only) str_c(str_to_sentence(cases_note(v1)), ' only. ') else ''}",
                     "Standard errors {iv_cluster_note}. F-statistic: Wald test of the ",
                     "conviction instrument.{race_note(v1)}"))
})


# ---- figure: distribution of the conviction instrument and the first stage -------------
# residuals on the construction controls and fixed effects (NA where a control is missing)
resid_on <- \(y, v, data) resid(feols(as.formula(str_glue("{y} ~ 1 | {fe_of(v)}")), data = data,
                                      notes = FALSE), na.rm = FALSE)

# local linear regression (Epanechnikov kernel, bandwidth h) of y on x at the grid
# points of data p (columns x, y and the cluster variables), with the standard
# error of the fit clustered as the regressions (kernel-weighted least squares:
# sandwich variance)
local_linear <- \(p, grid, h) map(grid, \(g) {
  x <- mutate(p, w = pmax(1 - ((z - g) / h)^2, 0), zg = z - g) |> filter(w > 0)
  m <- feols(y ~ zg, data = x, weights = ~w, cluster = iv_cluster, notes = FALSE)
  tibble(x = g, fit = coef(m)[[1]], se = se(m)[[1]])
}) |> list_rbind()

fs_panel <- \(v, data) {
  p <- tibble(z = resid_on(v$z_conv, v, data), y = resid_on("d1", v, data), d1 = data$d1,
              gr_jdg_Y = data$gr_jdg_Y, person_id = data$person_id) |>
    drop_na() |>
    mutate(y = y + mean(d1))                      # residualized rate, around the mean rate
  lim <- quantile(p$z, c(0.01, 0.99))             # shown: 1st-99th percentiles
  p <- filter(p, between(z, quantile(z, 0.001), quantile(z, 0.999)))   # fitted: 0.1st-99.9th
  h <- sd(p$z)                                    # bandwidth: SD of the instrument
  breaks <- seq(lim[1], lim[2], length.out = 41)
  hist <- p |>
    filter(between(z, lim[1], lim[2])) |>
    count(bin = cut(z, breaks, include.lowest = TRUE, labels = FALSE)) |>
    mutate(mid = (breaks[bin] + breaks[bin + 1]) / 2, share = n / sum(n))
  ll <- local_linear(p, seq(lim[1], lim[2], length.out = 100), h = h)
  # right axis (rate) mapped onto the left (share): the CI band fills the middle of the panel
  band <- range(c(ll$fit - 1.96 * ll$se, ll$fit + 1.96 * ll$se))
  lo <- band[1] - 0.5 * diff(band); hi <- band[2] + 0.5 * diff(band)
  top <- 1.1 * max(hist$share)
  to_left <- \(r) (r - lo) / (hi - lo) * top
  ggplot() +
    geom_col(aes(mid, share), data = hist, fill = "grey60", colour = "white", width = diff(breaks)[1]) +
    geom_line(aes(x, to_left(fit)), data = ll, linewidth = 0.8) +
    geom_line(aes(x, to_left(fit - 1.96 * se)), data = ll, linetype = "dashed", colour = "grey45") +
    geom_line(aes(x, to_left(fit + 1.96 * se)), data = ll, linetype = "dashed", colour = "grey45") +
    scale_y_continuous("Fraction of sample", limits = c(0, top), expand = c(0, 0),
                       sec.axis = sec_axis(\(l) lo + l / top * (hi - lo), name = "Predicted rate of conviction")) +
    labs(x = "Resid. conviction leniency") +
    theme_classic(base_size = 13) +
    theme(axis.title.y.right = element_text(angle = 90))          # read bottom to top, as the left axis
}

# one figure per version and sample (all, White, Black)
iwalk(iv_versions, \(v, vn) iwalk(version_data[[vn]][samples], \(x, s) {
  f <- file.path(fig_dir, str_glue("first_stage_{vn}_{s}.pdf"))
  ggsave(f, fs_panel(for_sample(v, s), x), width = 5, height = 3.9)
  cat("  wrote", f, "\n")
}))


# ---- figure: conviction against dismissal leniency ----------------------------------
# the sample's instruments (by race for White and Black defendants, if the version
# has them), residualized on the controls and fixed effects (the 2SLS compares judges
# within court and period), averaged over each judge x year x court cell (the cells of
# the leave-out means), with the cell's number of cases as weight; all, White, Black
leniency <- imap(iv_versions, \(v, vn) {
  imap(version_data[[vn]][samples], \(x, s) {
    v_s <- for_sample(v, s)
    x |>
      mutate(conv = resid_on(v_s$z_conv, v_s, x), dism = resid_on(v_s$z_dism, v_s, x)) |>
      filter(!is.na(conv), !is.na(dism)) |>
      summarise(conv = mean(conv), dism = mean(dism), n = n(), .by = gr_jdg_Y_crt) |>
      mutate(sample = s)
  }) |>
    list_rbind() |>
    mutate(version = vn)
}) |>
  list_rbind()
corrs <- leniency |>
  summarise(r = cov.wt(cbind(conv, dism), wt = n, cor = TRUE)$cor[1, 2], cells = n(), .by = c(version, sample)) |>
  mutate(label = sprintf("Correlation = %.2f\n(%d cells, weighted)", r, cells))
cat("\nconviction vs dismissal leniency, by judge x year x court:\n"); print(corrs)

# one figure per version and sample (all, White, Black); axes shown over the
# 0.5-99.5 percentiles of the cells (the fit uses all cells)
shown <- \(x) quantile(x, c(0.005, 0.995))
walk(names(iv_versions), \(vn) walk(samples, \(s) {
  cells <- filter(leniency, version == vn, sample == s)
  scatter <- ggplot(cells, aes(dism, conv)) +
    geom_point(aes(size = n), shape = 21, fill = "grey60", colour = "white", alpha = 0.8) +
    geom_smooth(aes(weight = n), method = "lm", formula = y ~ x, se = TRUE, colour = "black", linewidth = 0.8) +
    geom_text(aes(x = Inf, y = Inf, label = label), data = filter(corrs, version == vn, sample == s),
              hjust = 1.05, vjust = 1.3, size = 3.5) +
    scale_size_area(max_size = 3, guide = "none") +
    coord_cartesian(xlim = shown(cells$dism), ylim = shown(cells$conv)) +
    labs(x = "Resid. dismissal leniency", y = "Resid. conviction leniency") +
    theme_classic(base_size = 13)
  f <- file.path(fig_dir, str_glue("leniency_scatter_{vn}_{s}.pdf"))
  ggsave(f, scatter, width = 5, height = 3.9)
  cat("  wrote", f, "\n")
}))


# ---- 2SLS: recidivism on non-carceral conviction ---------------------------------------
# (2SLS helpers iv_by_race, version_samples: functions/iv_2sls.R)
families <- list(
  new_cases   = list(pattern = "cc_post{w}_{s}_all_any",
                     status  = c(charged = "Charged", conv = "Convicted", incar = "Incarcerated"),
                     label   = "any new felony CC case"),
  revocations_other = list(pattern = "cc_post{w}_revother_{s}_all_any",
                           status  = c(charged = "Proceeding", conv = "Revoked"),
                           label   = str_c("any revocation of the person's other felony sentences set off after the ",
                                           "case's first disposition (sentences imposed after the day of the case's first ",
                                           "disposition; earlier or unidentified ones only after a new case)")),
  revocations_focal = list(pattern = "cc_post{w}_revfocal_{s}_all_any",
                           status  = c(charged = "Proceeding", conv = "Revoked"),
                           label   = str_c("any revocation of the focal felony sentences, those imposed at the focal ",
                                           "hearing (filed under the case, if convicted, or under another case of the ",
                                           "person first disposed the same day)")))
first_years <- c("1yr", "2yr", "3yr")          # windows of the tables (recid_windows)

# 2SLS table: rows the outcomes, by status (e.g. charged / convicted / incarcerated),
# then window; columns, for each estimate set in rs (the versions of an analysis: outer
# column groups labelled groups), White, Black (coefficient, (se), [outcome mean]) and
# White - Black (coefficient, (se)); one sample per set for all outcomes, so one row of
# observations; labels: the windows' row labels
write_iv_table <- \(file, rs, fam, notes, groups = NULL, windows = first_years,
                    labels = recid_windows$label[match(windows, recid_windows$window)]) {
  walk(rs, \(r) stopifnot(n_distinct(r$white$n) == 1, n_distinct(r$black$n) == 1))
  cells <- map(rs, \(r) map(r, \(x) coef_cells(x$est, x$se, x$p)))
  mean_y <- map(rs, \(r) map(r[c("white", "black")], \(x) str_c("[", fmt(x$mean_y), "]")))
  row <- \(i, label) c(tex_row(label, unlist(map(cells, \(cv) map_chr(cv, \(x) x$est[i])))),
                       tex_row("", unlist(map(cells, \(cv) map_chr(cv, \(x) x$se[i])))),
                       tex_row("", unlist(map(mean_y, \(m) c(m$white[i], m$black[i], "")))))
  nc <- 3 * length(rs)
  body <- imap(unname(fam$status), \(status, s) {
    c(str_c("\\multicolumn{", nc + 1, "}{l}{\\textit{", status, "}} \\\\"),
      unlist(map(seq_along(windows), \(w) row((s - 1) * length(windows) + w, str_c("\\quad ", labels[w])))))
  })
  write_tex(
    file, str_c("l", strrep("c", nc)),
    header = c(if (length(rs) > 1) group_header(groups, 3),
               tex_row("", rep(c("White", "Black", "White $-$ Black"), length(rs)))),
    body = c(head(unlist(map(body, \(b) c(b, "\\addlinespace"))), -1), "\\midrule",
             tex_row("Observations", unlist(map(rs, \(r) c(fmt(r$white$n[1], 0), fmt(r$black$n[1], 0), ""))))),
    notes = notes)
}
iv_notes <- \(v, fam, with_dism = TRUE, when = "within 1, 2 and 3 years", windows = first_years,
              after = "the case's first disposition", last_year = full_follow_up_year(windows), label = v$label) {
  v <- for_sample(v, "white")
  str_glue(
    "{label} instruments. 2SLS of {fam$label} {when} after {after} on ",
    "non-carceral conviction, instrumented by the conviction instrument ({v$z_conv})",
    "{if (with_dism) str_c(', controlling for the dismissal instrument (', v$z_dism, ')') else ''}",
    "{if (str_ends(v$z_conv, '_race')) ', built within the defendant\\'s race' else ''}. ",
    "Rows: outcomes; columns: White and Black defendants and White minus Black. Standard errors in parentheses; ",
    "outcome mean among the column's defendants in brackets. White minus Black: from one regression of White and ",
    "Black defendants with all controls, fixed effects, the treatment and the instrument interacted with a White ",
    "indicator. Controls: {controls_note(v)}; fixed effects: {fe_note(v)}. ",
    "{if (v$convicted_only) str_c(str_to_sentence(cases_note(v)), ' only. ') else ''}",
    "Standard errors {iv_cluster_note}. ",
    "All outcomes on the same cases, first disposed in or before {last_year} (full follow-up for ",
    "every window in the circuit-court record, which ends in {cc_last_year}).{race_sample_note(v)}")
}
# outcome variables in the table order: status, then window
outcomes <- \(fam, windows = first_years) {
  expand_grid(s = names(fam$status), w = windows) |> mutate(y = str_glue(fam$pattern)) |> pull(y)
}
show <- \(r, title) {
  cat("\n", title, "\n", sep = "")
  print(tibble(outcome = r$white$outcome, white = r$white$est, black = r$black$est, white_minus_black = r$diff$est,
               se_diff = r$diff$se, p_diff = r$diff$p, n_white = r$white$n, n_black = r$black$n), digits = 3)
}

# 2SLS by race (iv_by_race) of a family's outcomes in `windows`, in the table order
# (status, then window), all on the same cases: those of the samples vs with full
# follow-up for the longest window (full_follow_up_year, 00_master.R), further
# restricted by keep; controlling for the dismissal instrument
iv_windows <- \(v, fam, vs, windows, keep = identity) {
  vs <- map(vs, \(x) keep(filter(x, Y <= full_follow_up_year(windows))))
  iv_by_race(v, outcomes(fam, windows), vs$white, vs$black, with_dism = TRUE)
}

# an analysis's table: the 2SLS of each of its versions (outer column groups)
iv_analysis <- \(vns, an, fam, fn, file, keep = identity, notes_args = list(), title = "") {
  rs <- map(names(vns), \(vn) {
    v <- iv_versions[[vn]]
    r <- iv_windows(v, fam, version_data[[vn]], first_years, keep = keep)
    show(r, str_glue("{v$label} 2SLS{title}, {fn}: non-carceral conviction (1, 2, 3 years)"))
    r
  })
  v1 <- iv_versions[[names(vns)[1]]]
  notes <- str_c(do.call(iv_notes, c(list(v1, fam, label = analysis_label[[an]]), notes_args)), score_note(vns))
  write_iv_table(file, rs, fam, notes, groups = unname(vns))
}

iwalk(analyses, \(vns, an) walk(names(families), \(fn) {
  iv_analysis(vns, an, families[[fn]], fn, file.path(table_dir, str_glue("iv_{an}_{fn}.tex")))
}))


# ---- 2SLS, outcomes from release ------------------------------------------------------
# The same outcomes within 1, 2 and 3 years after release from the focal sentence
# (cc_rel<cap>_..., 04: the first disposition plus release_share of the active carceral
# time, capped at cap years; the first disposition if not incarcerated), for each cap in
# release_caps, on the cases first disposed by cc_last_year - 3 - cap (full follow-up for
# 3 years after release, whatever the time served)
iwalk(analyses, \(vns, an) walk(release_caps, \(cap) walk(names(families), \(fn) {
  fam <- families[[fn]]
  fam$pattern <- str_replace(fam$pattern, "^cc_post", str_c("cc_rel", cap, "_post"))
  last_year <- full_follow_up_year(first_years) - cap
  iv_analysis(vns, an, fam, fn, file.path(table_dir, str_glue("iv_{an}_{fn}_from_release_cap{cap}.tex")),
              keep = \(x) filter(x, Y <= last_year), title = str_glue(" from release (time served capped at {cap} years)"),
              notes_args = list(last_year = last_year,
                                after = str_glue("release from the case's sentence (the first disposition plus ",
                                                 "{100 * release_share}% of the active carceral time, at most {cap} years; ",
                                                 "the first disposition if not incarcerated)")))
})))


# ---- revocation measures and their parts ------------------------------------------------
# Per analysis (secondary: without the score control): 2SLS of any revocation proceeding
# within 1 and 3 years of the disposition (cases first disposed by 2016) and of release
# (by 2012), on the two measures and their parts (04)
parts <- c(revother_ = "Other sentences (primary measure)", revlater_ = "\\quad later sentences only",
           revearly_ = "\\quad earlier or unidentified sentences, after a new case, only",
           revothnr_ = "\\quad without revocation-only case numbers",
           revfocal_ = "Focal sentences (secondary measure)", revfcase_ = "\\quad focal case only",
           revfsame_ = "\\quad other cases sentenced at the focal hearing only")
iwalk(analyses, \(vns, an) {
  vn <- names(vns)[1]
  v <- iv_versions[[vn]]
  # dropping revocation-only case numbers is a robustness check of the primary analysis only
  parts <- if (an == "primary") parts else parts[names(parts) != "revothnr_"]
  panels <- list(disposition = list(stem = "cc_post", last_year = full_follow_up_year(first_years),
                                    title = str_c("Measured from the disposition (cases first disposed by ",
                                                  full_follow_up_year(first_years), ")")),
                 release = list(stem = str_c("cc_rel", release_caps, "_post"),
                                last_year = full_follow_up_year(first_years) - release_caps,
                                title = str_c("Measured from release, time served capped at ", release_caps,
                                              " years (cases first disposed by ",
                                              full_follow_up_year(first_years) - release_caps, ")")))
  body <- imap(panels, \(pn, pname) {
    ys <- expand_grid(p = names(parts), w = c("1yr", "3yr")) |> mutate(y = str_c(pn$stem, w, "_", p, "charged_all_any"))
    vs <- map(version_data[[vn]], \(x) filter(x, Y <= pn$last_year))
    r <- iv_by_race(v, ys$y, vs$white, vs$black, with_dism = TRUE)
    show(r, str_glue("{v$label} 2SLS, revocation proceedings by part, {pname}"))
    rows <- imap(parts, \(label, p) {
      i <- which(ys$p == p)                                        # 1 and 3 years
      cells <- coef_cells(c(r$white$est[i], r$black$est[i]), c(r$white$se[i], r$black$se[i]),
                          c(r$white$p[i], r$black$p[i]))
      c(tex_row(label, cells$est), tex_row("", cells$se))
    }) |> unlist()
    c(str_c("\\multicolumn{5}{l}{\\textit{", pn$title, "}} \\\\"), rows,
      tex_row("Observations", c(fmt(r$white$n[1], 0), "", fmt(r$black$n[1], 0), "")))
  })
  write_tex(file.path(table_dir, str_glue("iv_{an}_revocation_parts.tex")), "lcccc",
            header = c(group_header(c("White", "Black"), 2), tex_row("", rep(c("1 year", "3 years"), 2))),
            body = c(body$disposition, "\\addlinespace", body$release),
            notes = str_glue("{analysis_label[[an]]} analysis{if (length(vns) > 1) ', without the score control' else ''}. ",
                             "2SLS of any felony revocation proceeding within 1 and 3 years on non-carceral conviction, ",
                             "instrumented by conviction leniency and controlling for dismissal leniency (built within ",
                             "the defendant's race), for the two revocation measures and their parts: other sentences = ",
                             "later sentences (cases first disposed after the day of the focal disposition) + earlier or ",
                             "unidentified (revocation-only case numbers) ones when the defendant has a new case between ",
                             "the focal disposition and the filing",
                             "{if (an == 'primary') ' (without revocation-only case numbers: later sentences + earlier ones after a new case)' else ''}",
                             "; focal sentences = filed under the focal case + under ",
                             "another case first disposed the same day. Standard errors in parentheses, ",
                             "{iv_cluster_note}. Controls: {controls_note(for_sample(v, 'white'))}; ",
                             "fixed effects: {fe_note(v)}.{race_sample_note(v)}"))
})


# ---- comparisons: OLS -------------------------------------------------------------------
# The same outcomes and cases as the 2SLS tables of the primary analysis and of the
# secondary one without the score control: OLS on non-carceral conviction, controlling for
# dismissal (so that it compares non-carceral conviction with incarceration, as the 2SLS
# with the dismissal-leniency control; all primary cases are convicted), with the same
# controls and fixed effects
walk(c("primary", "secondary"), \(vn) {
  v <- iv_versions[[vn]]
  vs <- map(version_data[[vn]], \(x) filter(x, Y <= full_follow_up_year(first_years)))
  walk(names(families), \(fn) {
    fam <- families[[fn]]
    ys <- outcomes(fam, first_years)
    r <- ols_by_race(v, ys, vs$white, vs$black)
    show(r, str_glue("{v$label} OLS, {fn} (1, 2, 3 years)"))
    write_iv_table(file.path(table_dir, str_glue("iv_{vn}_{fn}_ols.tex")), list(r), fam,
                   str_glue("OLS of {fam$label} within 1, 2 and 3 years after the case's first disposition on ",
                            "non-carceral conviction, controlling for dismissal (the coefficient compares non-carceral ",
                            "conviction with incarceration), on the cases of the {str_to_lower(v$label)} 2SLS. Rows: ",
                            "outcomes; columns: White and Black defendants and White minus Black (from one regression ",
                            "with every regressor and fixed effect interacted with a White indicator). Standard errors in ",
                            "parentheses; outcome mean in brackets. Controls: {controls_note(for_sample(v, 'white'))}; ",
                            "fixed effects: {fe_note(v)}. Standard errors {iv_cluster_note}. ",
                            "Cases first disposed in or before {full_follow_up_year(first_years)}.",
                            "{race_sample_note(v)}"))
  })
})


# ---- robustness of the primary analysis --------------------------------------------------
# Three sets of checks, each set side by side (00_master.R): robustness (UJIVE instruments),
# robustness_min_cases (other minimums of cases of each race per judge for the White and
# Black samples) and release_caps_robustness (other caps on time served, outcomes from
# release). First stages, and the 2SLS of the key outcomes -- any new felony charge and
# any revocation proceeding on other sentences and on the focal sentences, within 1, 2 and
# 3 years -- from the disposition and from release
key <- list(pattern = "cc_post{w}_{s}_all_any",
            status  = c(charged = "New felony case charged", revother_charged = "Revocation proceeding, other sentences",
                        revfocal_charged = "Revocation proceeding, focal sentences"),
            label   = str_c("any new felony charge, any revocation proceeding on the person's other sentences and on ",
                            "the focal sentences"))
at_release <- \(fam, cap) { fam$pattern <- str_replace(fam$pattern, "^cc_post", str_c("cc_rel", cap, "_post")); fam }
release_after <- \(cap) str_glue("release from the case's sentence (the first disposition plus {100 * release_share}% of ",
                                 "the active carceral time, at most {cap} years; the first disposition if not incarcerated)")
primary_with <- \(r) modifyList(iv_versions$primary, r[setdiff(names(r), "label")])
check_sets <- list(
  robustness = list(versions = map(robustness, primary_with), labels = map_chr(robustness, "label"),
                    note = str_glue(" UJIVE instruments ({robustness$ujive$z_conv_race}, ",
                                    "{robustness$ujive$z_dism_race}: residualized on the controls and fixed effects, the ",
                                    "defendant left out of the residualization).")),
  min_cases  = list(versions = map(robustness_min_cases, primary_with), labels = map_chr(robustness_min_cases, "label"),
                    note = str_c(" Columns: the White and Black samples restricted to the cases of judges with at least ",
                                 "the given number of convicted cases with a matched score of each race in the case's ",
                                 "3-year window (none, 25, 100; the main analysis: ", min_race_cases, ").")))

# first stages (with the dismissal instrument, as in the 2SLS), White and Black defendants
iwalk(check_sets, \(cs, cn) {
  fits <- expand_grid(sample = c("white", "black"), vn = names(cs$versions)) |>
    mutate(v_s = map2(vn, sample, \(vn, s) for_sample(cs$versions[[vn]], s)),
           x   = map2(vn, sample, \(vn, s) version_samples(cs$versions[[vn]], d)[[s]]),
           m   = pmap(list(v_s, x), \(v_s, x) first_stage(v_s, x, TRUE)))
  conv  <- map2(fits$m, fits$v_s, \(m, v_s) cell(m, v_s$z_conv))
  fstat <- map2_dbl(fits$m, fits$v_s, \(m, v_s) wald(m, keep = str_c("^", v_s$z_conv, "$"), print = FALSE)$stat)
  judges <- map2_dbl(fits$m, fits$x, \(m, x) n_distinct(x$judgeid[obs(m)]))
  cat(str_glue("\nPrimary first stages, {cn} (with the dismissal instrument): coef / F / N / judges"), "\n")
  print(tibble(sample = fits$sample, check = fits$vn, coef = map2_dbl(fits$m, fits$v_s, \(m, v_s) coef(m)[[v_s$z_conv]]),
               F = round(fstat, 1), n = map_dbl(fits$m, nobs), judges = judges), n = Inf)
  write_tex(
    file.path(table_dir, str_glue("first_stage_primary_{cn}.tex")), str_c("l", strrep("c", nrow(fits))),
    header = c(group_header(c("White", "Black"), length(cs$versions)), tex_row("", rep(unname(cs$labels), 2))),
    body = c(tex_row("Conviction leniency", map_chr(conv, "est")), tex_row("", map_chr(conv, "se")),
             "\\midrule",
             tex_row("F-statistic, conviction leniency", fmt(fstat, 1)),
             tex_row("Observations", fmt(map_dbl(fits$m, nobs), 0)),
             tex_row("Judges", fmt(judges, 0))),
    notes = str_c("Primary analysis. First stages of non-carceral conviction on the conviction instrument built within ",
                  "the defendant's race, controlling for the dismissal instrument (the first stage of the 2SLS), all ",
                  "analysis years.", cs$note, " Standard errors ", iv_cluster_note, "."))
})

# 2SLS of the key outcomes, from the disposition and from release
show_set <- \(rs, title) iwalk(rs, \(r, vn) show(r, str_glue("Primary 2SLS, {title}, {vn}")))
iwalk(check_sets, \(cs, cn) {
  data_of <- map(cs$versions, \(v) version_samples(v, d))
  rs <- imap(cs$versions, \(v, vn) iv_windows(v, key, data_of[[vn]], first_years))
  show_set(rs, str_c(cn, ", from the disposition"))
  write_iv_table(file.path(table_dir, str_glue("iv_primary_{cn}.tex")), rs, key,
                 str_c(iv_notes(iv_versions$primary, key), cs$note), groups = unname(cs$labels))
  last_year <- full_follow_up_year(first_years) - release_caps
  rs <- imap(cs$versions, \(v, vn) iv_windows(v, at_release(key, release_caps), data_of[[vn]], first_years,
                                             keep = \(x) filter(x, Y <= last_year)))
  show_set(rs, str_c(cn, ", from release"))
  write_iv_table(file.path(table_dir, str_glue("iv_primary_{cn}_from_release.tex")), rs, at_release(key, release_caps),
                 str_c(iv_notes(iv_versions$primary, key, last_year = last_year, after = release_after(release_caps)),
                       cs$note), groups = unname(cs$labels))
})

# outcomes from release under each cap on time served (release_caps and
# release_caps_robustness), two ways: (a) each cap on its own full-follow-up cases (first
# disposed by cc_last_year - 3 - cap), so that a column changes everything the choice of
# cap implies, the windows of long sentences and the cohorts in the sample; (b) every cap
# on the same cases (full follow-up under the largest cap), so that the columns differ only
# by the cap
vp <- iv_versions$primary
caps <- sort(c(release_caps, release_caps_robustness))
caps_note <- str_c(" Columns: release = the first disposition plus time served, ", 100 * release_share,
                   "% of the active carceral time capped at ", str_flatten(caps, ", ", last = " or "),
                   " years (the first disposition if not incarcerated).")
caps_main <- \(cap) if_else(cap == release_caps, " (main cap)", "")
rs <- map(set_names(caps), \(cap) iv_windows(vp, at_release(key, cap), version_data$primary, first_years,
                                             keep = \(x) filter(x, Y <= full_follow_up_year(first_years) - cap)))
show_set(rs, "release caps, own cases")
write_iv_table(file.path(table_dir, "iv_primary_release_caps.tex"), rs, key,
               str_c(iv_notes(vp, key, after = "release from the case's sentence",
                              last_year = "the year given in the column header"),
                     caps_note, " Each column on the cases with full follow-up for 3 years after release under its cap."),
               groups = str_c("At most ", caps, " years", caps_main(caps), ", disposed by ",
                              full_follow_up_year(first_years) - caps))
caps_last_year <- full_follow_up_year(first_years) - max(caps)
rs <- map(set_names(caps), \(cap) iv_windows(vp, at_release(key, cap), version_data$primary, first_years,
                                             keep = \(x) filter(x, Y <= caps_last_year)))
show_set(rs, "release caps, same cases")
write_iv_table(file.path(table_dir, "iv_primary_release_caps_same_cases.tex"), rs, key,
               str_c(iv_notes(vp, key, after = "release from the case's sentence", last_year = caps_last_year),
                     caps_note, " Every column on the cases first disposed by ", caps_last_year,
                     " (full follow-up for 3 years after release under every cap)."),
               groups = str_c("At most ", caps, " years", caps_main(caps)))

# the primary revocation measure without revocation-only case numbers (later sentences, and
# earlier ones after a new case: 04 revothnr_), all windows, from the disposition and from
# release; same cases as the main tables
no_revonly <- modifyList(families$revocations_other, list(
  pattern = "cc_post{w}_revothnr_{s}_all_any",
  label = str_c("any revocation of the person's other felony sentences set off after the case's first disposition, ",
                "without revocation-only case numbers (sentences imposed after the day of the case's first ",
                "disposition; earlier ones only after a new case)")))
r <- iv_windows(vp, no_revonly, version_data$primary, first_years)
show(r, "Primary 2SLS, other sentences without revocation-only case numbers, from the disposition")
write_iv_table(file.path(table_dir, "iv_primary_revocations_other_no_revonly.tex"), list(r), no_revonly,
               iv_notes(vp, no_revonly))
last_year <- full_follow_up_year(first_years) - release_caps
r <- iv_windows(vp, at_release(no_revonly, release_caps), version_data$primary, first_years,
                keep = \(x) filter(x, Y <= last_year))
show(r, "Primary 2SLS, other sentences without revocation-only case numbers, from release")
write_iv_table(file.path(table_dir, "iv_primary_revocations_other_no_revonly_from_release.tex"), list(r),
               at_release(no_revonly, release_caps),
               iv_notes(vp, no_revonly, last_year = last_year, after = release_after(release_caps)))


# ---- standard errors under other clusterings (primary analysis) --------------------------
# Key outcomes of the primary 2SLS, from the disposition and from release: the estimate
# and its standard error clustered by defendant (iv_cluster), and two-way by defendant and
# judge x year, judge x month (a proxy for assignment in blocks of time), judge x 3-year
# period (2000-02, 2003-05, ...: blocks the length of the instruments' window) or judge,
# and heteroskedasticity-robust only (judge ids are court-specific)
cl_schemes <- list("Defendant (baseline)" = iv_cluster, "Defendant and judge $\\times$ year" = ~person_id + gr_jdg_Y,
                   "Defendant and judge $\\times$ month" = ~person_id + gr_jdg_ym,
                   "Defendant and judge $\\times$ 3-year period" = ~person_id + gr_jdg_3y,
                   "Defendant and judge" = ~person_id + judgeid, "Robust, no clusters" = "hetero")
cl_outcomes <- c(charged = "Charged, 1 year", revother = "Other sentences, 3 years",
                 revfocal = "Focal sentences, 3 years")
cl_panel <- \(stem, last_year) {
  ys <- c(str_c(stem, "1yr_charged_all_any"), str_c(stem, "3yr_revother_charged_all_any"),
          str_c(stem, "3yr_revfocal_charged_all_any"))
  v <- for_sample(iv_versions$primary, "white")
  map(c("white", "black"), \(g) {
    x <- version_data$primary[[g]] |> filter(Y <= last_year) |>
      mutate(gr_jdg_ym = str_c(judgeid, "_", Y, "_", month), gr_jdg_3y = str_c(judgeid, "_", (Y - 2000) %/% 3))
    m <- feols(as.formula(str_glue("c({str_flatten(ys, ', ')}) ~ {v$z_dism} | {fe_of(v)} | ",
                                   "d1 ~ {v$z_conv}")), data = x, notes = FALSE)
    map(as.list(m), \(mm) c(est = coef(mm)[["fit_d1"]], map_dbl(cl_schemes, \(cl) se(summary(mm, vcov = cl))[["fit_d1"]])))
  }) |> list_flatten()                                     # white: 3 outcomes, then black
}
cl_res <- list(disposition = cl_panel("cc_post", full_follow_up_year(first_years)),
               release = cl_panel(str_c("cc_rel", release_caps, "_post"), full_follow_up_year(first_years) - release_caps))
cl_body <- imap(cl_res, \(r, pn) {
  c(str_c("\\multicolumn{7}{l}{\\textit{", if (pn == "disposition") str_c("Measured from the disposition (cases first ",
          "disposed by ", full_follow_up_year(first_years), ")") else str_c("Measured from release, time served capped at ",
          release_caps, " years (cases first disposed by ", full_follow_up_year(first_years) - release_caps, ")"), "}} \\\\"),
    tex_row("Estimate", fmt(map_dbl(r, "est"))),
    imap_chr(cl_schemes, \(cl, nm) tex_row(nm, str_c("(", fmt(map_dbl(r, nm)), ")"))))
})
cat("\nPrimary 2SLS, standard errors under other clusterings:\n"); print(map(cl_res, \(r) round(do.call(cbind, r), 3)))
write_tex(file.path(table_dir, "iv_primary_clusters.tex"), "lcccccc",
          header = c(group_header(c("White", "Black"), 3), tex_row("", rep(unname(cl_outcomes), 2))),
          body = c(cl_body$disposition, "\\addlinespace", cl_body$release),
          notes = str_c("Primary analysis. 2SLS coefficient on non-carceral conviction (dismissal-leniency control) for being ",
                        "charged with a new felony case within one year and for any revocation proceeding on other ",
                        "sentences and on the focal sentences within three years, and its standard error ",
                        "under each clustering: by defendant (the baseline), two-way by defendant and judge x year, judge x ",
                        "calendar month of the first disposition, judge x 3-year period (2000-02, 2003-05, ...) or judge, ",
                        "and heteroskedasticity-robust without clusters.",
                        race_sample_note(iv_versions$primary)),
          star_note = FALSE)
