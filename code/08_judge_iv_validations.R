# ==============================================================================
# 08_judge_iv_validations.R
#
# Validations of the judge IV (instrument versions as in 07):
#   Balance      non-carceral conviction (d1) and the conviction instrument
#                regressed on defendant covariates -- female, offense type, any
#                misdemeanor charge, prior felony conviction (CC or SC), prior
#                misdemeanor-only conviction (CC), both 31 days to 5 years before the
#                case's first offense; no age, which comes from the
#                SC records, mostly of convicted defendants -- with the fixed
#                effects of the instrument construction (court x year, court x
#                calendar month, court x day of week; in the primary analysis also one
#                per guidelines offense group x matched score cell, and convicted cases
#                with a matched score only); all, White and Black defendants (the
#                instrument built within the defendant's race; all defendants: White
#                and Black together, the fixed effects specific to each race, common
#                covariate coefficients, as Arnold, Dobbie and Yang 2018); joint F-test
#                of the covariates.
#                Standard errors clustered by defendant (iv_cluster).
#   (the sentence-length tables are kept in archive/sentence_length.R, not run)
#   Monotonicity split-sample test: for White vs Black defendants (all
#                defendants only), sex and prior felony conviction, the conviction
#                instrument built from one group, first stage estimated on the
#                other, controlling for the dismissal instrument built from the
#                same group; all, White and Black defendants (all defendants, sex and
#                prior felony: the instruments built within each race, the first stage
#                on both races with race-specific controls and fixed effects)
#   Leniency by race (primary analysis)   each judge's leniency toward White and
#                toward Black defendants (all years, and per 3-year window): correlation, corrected
#                for sampling error; rank regression; same ordering of judges within court
#                (and year) by the two races, against random halves of the same race
#   Recidivism of the incarcerated   among incarcerated (d2) White and Black
#                defendants of each analysis's sample (primary: convicted cases with
#                a matched score; secondary: all cases), the share with any new
#                felony CC case charged / convicted / incarcerated, with any
#                revocation proceeding / revocation of the person's other felony
#                sentences, and of the focal felony sentences, within 1, 2
#                and 3 years after the case's first disposition (cases first
#                disposed in or before 2016, full follow-up for 3 years, as the 2SLS
#                of 07) and after release from the focal sentence (time served
#                capped at release_caps years; cases first disposed in or before
#                2019 - 3 - cap); each share tested against zero, standard errors
#                clustered by person.
#   Sentence length of the incarcerated   distribution of the active carceral time
#                of incarcerated White and Black defendants of each analysis's sample
#                (cases first disposed in or before 2016): a figure of the shares by
#                length; mean, median, shares of 1, 3 and 5 years or more, and the share
#                still in custody 1, 2 and 3 years after the disposition (estimated
#                release, as the outcomes from release) in the log.
#
# Input:   data_dir/final_judge_iv_analysis.csv
# Tables per analysis (analyses, 00_master.R): primary, and secondary with columns
# without and with the predicted-score control within each column group.
# Outputs: table_dir/balance_<analysis>.tex, table_dir/monotonicity_<analysis>.tex,
#          table_dir/leniency_by_race_primary.tex, fig_dir/leniency_by_race_primary.pdf,
#          table_dir/recidivism_incarcerated_<primary|secondary>[_from_release_cap<cap>].tex,
#          fig_dir/sentence_length_<primary|secondary>.pdf
# ==============================================================================

d <- read_csv(file.path(data_dir, "final_judge_iv_analysis.csv"), guess_max = Inf, show_col_types = FALSE,
              col_select = c(case_group, person_id, race, d0, d1, d2, carceral_length_inbtw, runningvariable,
                             judgeid, fipscode, Y, gr_jdg_Y, all_of(c(resid_fe, resid_controls, iv_instruments)),
                             matches(str_c("^cc_(rel", release_caps, "_)?post[1-3]yr_(revother_|revfocal_)?(charged|conv|incar)_all_any$"))))
# (all, White and Black defendants of a version: version_samples, functions/iv_2sls.R;
# its fixed effects: v$fe)

# in the table order: sex, prior record, offense type, any misdemeanor charge (no age: it
# comes from the SC records, which exist mostly for convicted defendants; no race: constant
# within the White and Black samples, and the all-defendants regressions have fixed effects
# specific to each race)
covariates_bal <- c(female = "Female",
                    flag_prior_fel_conv = "Prior felony conviction (5 years)",
                    flag_prior_mis_only_conv = "Prior misdemeanor only conviction (5 years)",
                    set_names(str_c("Offense: ", str_replace_all(str_remove(offense_type, "^any_"), "_", " ")),
                              offense_type),
                    any_mis = "Any misdemeanor charge")


# ---- balance ---------------------------------------------------------------------
# one table per analysis: column groups all, White and Black defendants; within each,
# the analysis's versions (secondary: without and with the score control), each with
# conviction and the conviction instrument as dependent variables
iwalk(analyses, \(vns, an) {
  # dependent variables: conviction, and the sample's conviction instrument;
  # covariates: without race for White and Black defendants
  fits <- expand_grid(sample = c("all", "white", "black"), vn = names(vns), dv = c("conviction", "leniency")) |>
    mutate(y = pmap_chr(list(vn, sample, dv), \(vn, s, dv) if (dv == "conviction") "d1" else for_sample(iv_versions[[vn]], s)$z_conv),
           m = pmap(list(vn, sample, y), \(vn, s, y) {
      v <- iv_versions[[vn]]
      feols(as.formula(str_glue("{y} ~ {rhs(names(covariates_bal))} | {rhs(for_sample(v, s)$fe)}")),
            data = version_samples(v, d)[[s]], cluster = iv_cluster, notes = FALSE)
    }),
    joint   = map(m, \(m) wald(m, keep = str_c("^(", str_flatten(names(covariates_bal), "|"), ")$"), print = FALSE)),
    joint_F = map_dbl(joint, "stat"),
    joint_p = map_dbl(joint, "p"))
  cat(str_glue("\n{analysis_label[[an]]} balance: joint F-tests of the covariates"), "\n")
  print(select(fits, sample, vn, y, joint_F, joint_p), n = Inf)

  rows <- imap(covariates_bal, \(label, x) {
    cells <- map(fits$m, \(m) coef_cells(coef(m)[x], se(m)[x], pvalue(m)[x], digits = 4))   # NA if dropped
    c(tex_row(label, map_chr(cells, "est")), tex_row("", map_chr(cells, "se")))
  }) |> unlist()
  k <- 2 * length(vns)
  v1 <- iv_versions[[names(vns)[1]]]
  write_tex(
    file.path(table_dir, str_glue("balance_{an}.tex")), str_c("l", strrep("c", 3 * k)),
    header = c(group_header(c("All", "White", "Black"), k),
               if (length(vns) > 1) group_header(rep(unname(vns), 3), 2),
               tex_row("", rep(c("Conviction", "Leniency"), 3 * length(vns))),
               tex_row("", str_c("(", seq_len(3 * k), ")"))),
    body = c(rows, "\\midrule",
             tex_row("Joint F-statistic", fmt(fits$joint_F, 2)),
             tex_row("Joint F-test p-value", fmt(fits$joint_p)),
             tex_row("Mean of dependent variable", fmt(map_dbl(fits$m, \(m) fitstat(m, "my")[[1]]), 4)),
             tex_row("Observations", fmt(map_dbl(fits$m, nobs), 0)),
             if (length(vns) > 1) tex_row("Predicted-score control", yes_no(fits$vn != names(vns)[1]))),
    notes = str_glue("{analysis_label[[an]]} instruments. Balance: non-carceral conviction (Conviction) and the conviction ",
                     "instrument ({str_flatten(unique(fits$y[fits$dv == 'leniency']), ' / ')}; Leniency) regressed on the ",
                     "covariates, with fixed effects: {fe_note(v1)}.{score_note(vns)} All defendants: White and Black ",
                     "defendants, the fixed effects specific to each race. Any misdemeanor charge: in the case. Prior convictions between ",
                     "31 days and 5 years before the case's first ",
                     "offense: felony, a convicted felony case in the full circuit-court record or a Sentencing Commission ",
                     "sentencing event of the defendant; misdemeanor only, a convicted misdemeanor circuit-court case and ",
                     "no felony conviction.",
                     "{if (v1$convicted_only) str_c(' ', str_to_sentence(cases_note(v1)), ' only.') else ''} ",
                     "Standard errors {iv_cluster_note}; joint F-test of all covariates.",
                     "{race_sample_note(v1)}"))
})


# ---- split-sample monotonicity --------------------------------------------------------
# For a split of a sample into two groups by a covariate: the conviction instrument
# built from one group only -- the residualized leave-out rate of non-carceral conviction
# over the judge's cases of that group in the 3-year window (residualized on the controls
# varying within the group and the version's fixed effects, in a regression on the group's
# cases), leaving out the defendant's own cases -- is given to the cases of the other group, and the first stage of
# non-carceral conviction on it is estimated there (controls, fixed effects and
# clustering as in the first stages of 07), controlling for the dismissal instrument
# built from the same group (as the first stage of the 2SLS). A positive coefficient:
# judges more lenient toward one group are also more lenient toward the other. The
# conviction instrument is built from the cases the sample's instrument is built from
# (primary: convicted cases with a matched score; secondary: all cases), within the
# defendant's race, the dismissal instrument from those of the version's dismissal
# instrument (all cases of the race). All defendants (White and Black): the race split
# builds the instrument from one race's cases and estimates the first stage on the other
# race; the other splits build each race's instruments from that race's cases of the group
# and estimate the first stage on both races, with every control and fixed effect
# specific to each race and one coefficient on each instrument (for_sample,
# 00_master.R), so the coefficient is a weighted average of the White and Black ones. Splits: White vs Black
# (all defendants only), sex, prior felony conviction; controls constant within a group
# are left out. Race and sex are defendant-level, so each defendant is on one side; the
# prior felony conviction is case-level by design (a defendant's later cases can follow a
# felony conviction), and the leave-out mean leaves the defendant's own cases out of their
# instrument (user decision 2026-10-26).
splits <- list(black               = list(label = "Race", groups = c("White", "Black")),
               female              = list(label = "Sex", groups = c("Male", "Female")),
               flag_prior_fel_conv = list(label = "Prior felony conviction (5 years)",
                                          groups = c("No prior felony", "Prior felony")))
varying <- \(x, vars) keep(vars, \(v) n_distinct(x[[v]], na.rm = TRUE) > 1)

# coefficient on the conviction instrument built from group `from` (0/1 value of
# `split`), in the first stage on the sample's cases of the other group, controlling
# for the dismissal instrument built from the same group
races_of <- list(all = c("White", "Black"), white = "White", black = "Black")
mono_fit <- \(v, s, split, from) {
  vs <- version_samples(v, d)
  v_s <- for_sample(v, s)
  by_race <- !is.null(v$z_conv_race)
  ctrl <- if (by_race) setdiff(v$controls, race_controls) else v$controls
  # sample r's cases of the other group, with the instruments built from the group's cases
  # of r (dismissal, in the primary version: all cases of r's races, as its dismissal
  # instrument is over all cases)
  with_split <- \(r) {
    est <- filter(vs[[r]], .data[[split]] != from)
    # leave-out mean of treat over the judge's cases of `built` (group `from`)
    split_instrument <- \(built, treat, fe) {
      built <- filter(built, .data[[split]] == from)
      built$r <- residualize(built, treat, varying(built, ctrl), fe)
      x <- bind_rows(built, mutate(est, r = NA_real_))
      tail(loo_mean(x, x$r, window = "3yr", loo_excl_person = TRUE), nrow(est))
    }
    est$z_split <- split_instrument(vs[[r]], "d1", v$fe)
    fe0 <- setdiff(v$fe, "offense_score_cell")               # d0 is defined for every case
    pop <- if (by_race) filter(d, race %in% races_of[[r]]) else d
    est$z0_split <- split_instrument(if (v$convicted_only) pop else vs[[r]], "d0", fe0)
    est
  }
  est <- if (s == "all" && by_race && split != race_controls)
    race_interacted(bind_rows(with_split("white"), with_split("black")), v) else with_split(s)
  m <- feols(as.formula(str_glue("d1 ~ z_split + z0_split | {rhs(v_s$fe, varying(est, v_s$controls))}")),
             data = est, cluster = iv_cluster, notes = FALSE)
  tibble(est = coef(m)[["z_split"]], se = se(m)[["z_split"]], p = pvalue(m)[["z_split"]], n = nobs(m))
}

# one table per analysis: columns all, White and Black defendants; within each, the
# analysis's versions (secondary: without and with the score control)
iwalk(analyses, \(vns, an) {
  mono <- imap(splits, \(sp, sv) {
    expand_grid(sample = c("all", "white", "black"), vn = names(vns), from = 0:1) |>
      filter(sv != "black" | sample == "all") |>
      mutate(pmap(list(vn, sample, from), \(vn, s, f) mono_fit(iv_versions[[vn]], s, sv, f)) |> list_rbind(), split = sv)
  }) |>
    list_rbind()
  cat(str_glue("\n{analysis_label[[an]]} split-sample monotonicity: first stage on one group, instrument built from the ",
               "other (controlling for the dismissal instrument built from the same group)"), "\n")
  print(mono, n = Inf, digits = 3)

  # rows: split, direction (built from -> estimated on)
  cols <- expand_grid(sample = c("all", "white", "black"), vn = names(vns))
  body <- imap(splits, \(sp, sv) {
    rows <- map(0:1, \(f) {
      x <- left_join(cols, filter(mono, split == sv, from == f), by = c("sample", "vn"))
      cells <- coef_cells(x$est, x$se, x$p)
      c(tex_row(str_c(sp$groups[f + 1], " $\\rightarrow$ ", sp$groups[2 - f]), cells$est),
        tex_row("", cells$se))
    }) |> unlist()
    c(str_c("\\multicolumn{", nrow(cols) + 1, "}{l}{\\textit{", sp$label, "}} \\\\"), rows, "\\addlinespace")
  }) |> unlist()
  v1 <- iv_versions[[names(vns)[1]]]
  write_tex(
    file.path(table_dir, str_glue("monotonicity_{an}.tex")), str_c("l", strrep("c", nrow(cols))),
    header = if (length(vns) > 1) {
      c(group_header(c("All", "White", "Black"), length(vns)),
        tex_row("Instrument built from $\\rightarrow$ first stage on", rep(unname(vns), 3)))
    } else tex_row("Instrument built from $\\rightarrow$ first stage on", c("All", "White", "Black")),
    body = head(body, -1),
    notes = str_glue("{analysis_label[[an]]} instruments. Split-sample monotonicity: for each split, the conviction ",
                     "instrument is built from one group only and the first stage of non-carceral conviction on it is ",
                     "estimated on the other group, controlling for the dismissal instrument built from the same group; ",
                     "coefficient on the conviction instrument and (standard error). Controls and fixed ",
                     "effects as in the first stages, without the controls constant within the group; standard errors ",
                     "{iv_cluster_note}. The conviction instrument is built from ",
                     "the {cases_note(v1)} of the defendant's race (as in the analysis), restricted to the group; the ",
                     "dismissal instrument from all cases of the race. All defendants (White and Black): the race split ",
                     "builds the instruments from one race's cases and estimates the first stage on the other race; the ",
                     "other splits build each race's instruments from that race's cases of the group and estimate the ",
                     "first stage on both races, every control and fixed effect specific to each race and one ",
                     "coefficient on each instrument, so the coefficient is a weighted average of the White and Black ",
                     "ones.{score_note(vns)}",
                     "{race_sample_note(v1)}"))
})


# ---- leniency by race (primary analysis) ------------------------------------------------
# Do judges rank the same way in leniency toward White and Black defendants? (Arnold, Dobbie
# and Yang 2018, p. 1909: a regression of the rank of each judge's leniency toward whites on
# that toward blacks, their leniency being measured per judge and year.) A judge's leniency
# toward a race: the mean residualized non-carceral sentence -- the first-step residual the
# race-specific instrument averages (06: within race, on the primary controls and fixed
# effects) -- over the judge's convicted cases with a matched score of that race in the
# primary race samples, per judge x court x 3-year window centered on each year with cases
# (the cases the instrument averages, without the leave-out), relative to the other judges of
# the same court and center year, as the instrument compares judges within court and year
# (user decisions 2026-10-26; the per-judge all-years comparison was dropped: it averages
# over years in which the orderings may differ). Statistics: the correlation of the White
# and Black means, weighted by the smaller of the two case counts; the reliability of each
# mean (Spearman-Brown 2r/(1+r) from random halves of the judge's defendants); the
# correlation corrected for sampling error, from the halves: the correlation between a White
# and a Black half over the square root of the product of the correlations between the two
# halves of each race; the regression of a judge-window's percentile rank in White leniency
# on that in Black leniency (unweighted); and, for pairs of judges in the same court and
# window year -- the comparisons the instrument makes -- the share ordered the same way by
# White and by Black leniency: with all cases, and in random halves across races against two
# halves of the same race, whose disagreements are sampling error alone. Each half is residualized in its own
# regression, as each race is: in one regression, the residuals of cases sharing a fixed-effect
# cell are negatively correlated (covariance -sigma^2 h_jk), which would make two halves of the
# same race disagree more than sampling error, and the reliability look lower. Halves:
# averages over n_splits random splits of the defendants.
n_splits <- 20
wcor <- \(a, b, w) cov.wt(cbind(a, b), wt = w, cor = TRUE)$cor[1, 2]
# share of the pairs within each group g ordered the same way by a and by b
concord <- \(a, b, g) tibble(a, b, g) |>
  filter(n() > 1, .by = g) |>
  summarise(same = { p <- combn(n(), 2); sum(sign(a[p[1, ]] - a[p[2, ]]) == sign(b[p[1, ]] - b[p[2, ]])) },
            pairs = choose(n(), 2), .by = g) |>
  summarise(share = sum(same) / sum(pairs), pairs = sum(pairs))

race_pop <- map(c(white = "White", black = "Black"), \(r)
  filter(d, race == r) |> mutate(d1_cond = if_else(d0 == 0 & !is.na(runningvariable), d1, NA_real_)))
# first-step residuals of a set of the race's cases (in its own regression), kept for the
# cases of the race sample
race_resid <- \(x, r) {
  v <- iv_versions$primary
  x$e <- residualize(x, "d1_cond", setdiff(v$controls, race_controls), v$fe)
  x |>
    semi_join(version_samples(v, d)[[r]], by = "case_group") |>
    filter(!is.na(e)) |>
    select(person_id, judgeid, fipscode, Y, e)
}
lenience <- imap(race_pop, race_resid) |> list_rbind(names_to = "race")
units <- list(judge_windows = c("judgeid", "fipscode", "Y"))
pair_group <- list(judge_windows = c("fipscode", "Y"))
# judge-windows: the judge's cases in the 3-year window centered on each year in which the
# judge has cases -- the cases the instrument averages (without the leave-out)
as_unit <- \(x, un)
  map(-1:1, \(o) mutate(x, Y = Y + o)) |> list_rbind() |> semi_join(distinct(x, judgeid, fipscode, Y), by = units[[un]])
# White and Black means per unit (and half h), units with cases of both races
race_means <- \(x, u, h = NULL) x |>
  summarise(m = mean(e), n = n(), .by = all_of(c(u, "race", h))) |>
  pivot_wider(names_from = all_of(c("race", h)), values_from = c(m, n)) |>
  drop_na()
# leniency relative to the other judges of the same court and year -- the comparisons the
# instrument makes (court x year fixed effects): each mean minus its group's mean, weighted
# by the smaller of the unit's case counts (w_cols); units without another judge in their
# group are dropped
within_group <- \(x, un, m_cols, w_cols) x |>
  mutate(.w = reduce(pick(all_of(w_cols)), pmin)) |>
  filter(n() > 1, .by = all_of(pair_group[[un]])) |>
  mutate(across(all_of(m_cols), \(m) m - weighted.mean(m, .w)), .by = all_of(pair_group[[un]])) |>
  select(-.w)
unit_means <- \(x, un) race_means(as_unit(x, un), units[[un]]) |>
  within_group(un, c("m_white", "m_black"), c("n_white", "n_black"))

set.seed(123)
halves <- map(seq_len(n_splits), \(s) imap(race_pop, \(x, r) {
  x <- left_join(x, distinct(x, person_id) |> mutate(half = sample(0:1, n(), replace = TRUE)), by = "person_id")
  map(0:1, \(k) race_resid(filter(x, half == k), r) |> mutate(half = k)) |> list_rbind()
}) |>
  list_rbind(names_to = "race"))
half_cols <- c("m_white_0", "m_white_1", "m_black_0", "m_black_1")
half_means <- \(x, un) race_means(as_unit(x, un), units[[un]], "half") |>
  within_group(un, half_cols, str_replace(half_cols, "^m_", "n_")) |>
  mutate(wh = pmin(n_white_0, n_white_1, n_black_0, n_black_1))
# the half correlations of one split, and the corrected correlation from the splits' averages
half_corrs <- \(hm) c(ww = wcor(hm$m_white_0, hm$m_white_1, hm$wh), bb = wcor(hm$m_black_0, hm$m_black_1, hm$wh),
                      wb = mean(c(wcor(hm$m_white_0, hm$m_black_1, hm$wh), wcor(hm$m_white_1, hm$m_black_0, hm$wh))))
corrected_corr <- \(hms) { r <- reduce(map(hms, half_corrs), `+`) / length(hms); r[["wb"]] / sqrt(r[["ww"]] * r[["bb"]]) }
# 95% interval of the corrected correlation: bootstrap over judges (the splits' half means
# kept, judges resampled with replacement)
n_boot <- 1000
lenience_stats <- imap(units, \(u, un) {
  full <- unit_means(lenience, un)
  w <- pmin(full$n_white, full$n_black)
  g <- interaction(full[pair_group[[un]]], drop = TRUE)
  # ranks of the leniency relative to the group, across all units
  rank_fit <- lm(percent_rank(m_white) ~ percent_rank(m_black), data = full)
  hms <- map(halves, \(x) half_means(x, un))
  split <- map(hms, \(hm) {
    gh <- interaction(hm[pair_group[[un]]], drop = TRUE)
    tibble(!!!as.list(half_corrs(hm)),
           same_cross = mean(c(concord(hm$m_white_0, hm$m_black_1, gh)$share, concord(hm$m_white_1, hm$m_black_0, gh)$share)),
           same_within = mean(c(concord(hm$m_white_0, hm$m_white_1, gh)$share, concord(hm$m_black_0, hm$m_black_1, gh)$share)))
  }) |>
    list_rbind() |>
    summarise(across(everything(), mean)) |>
    rename(r_ww = ww, r_bb = bb, r_wb = wb)
  set.seed(123)
  judges <- unique(hms[[1]]$judgeid)
  boot <- map_dbl(seq_len(n_boot), \(b) {
    jb <- tibble(judgeid = sample(judges, replace = TRUE))
    corrected_corr(map(hms, \(hm) inner_join(jb, hm, by = "judgeid", relationship = "many-to-many")))
  })
  rel <- \(r) 2 * r / (1 + r)
  cc <- concord(full$m_white, full$m_black, g)
  tibble(unit = un, units = nrow(full), median_white = median(full$n_white), median_black = median(full$n_black),
         corr = wcor(full$m_white, full$m_black, w), rel_white = rel(split$r_ww), rel_black = rel(split$r_bb),
         corr_corrected = split$r_wb / sqrt(split$r_ww * split$r_bb),
         corr_lo = quantile(boot, 0.025), corr_hi = quantile(boot, 0.975), boot_above_1 = mean(boot > 1),
         rank_coef = coef(rank_fit)[[2]], rank_se = sqrt(diag(vcov(rank_fit)))[[2]],
         pairs = cc$pairs, same_full = cc$share, same_cross = split$same_cross, same_within = split$same_within)
}) |>
  list_rbind()
cat("\nPrimary analysis: judge leniency toward White and Black defendants\n")
print(lenience_stats, width = Inf)

write_tex(
  file.path(table_dir, "leniency_by_race_primary.tex"), "lc",
  header = tex_row("", "Judge $\\times$ 3-year window"),
  body = c(tex_row("Units", fmt(lenience_stats$units, 0)),
           tex_row("Median cases per unit, White", fmt(lenience_stats$median_white, 0)),
           tex_row("Median cases per unit, Black", fmt(lenience_stats$median_black, 0)),
           "\\addlinespace",
           tex_row("Correlation of White and Black leniency", fmt(lenience_stats$corr, 2)),
           tex_row("Reliability of White leniency", fmt(lenience_stats$rel_white, 2)),
           tex_row("Reliability of Black leniency", fmt(lenience_stats$rel_black, 2)),
           tex_row("Correlation corrected for sampling error", fmt(lenience_stats$corr_corrected, 2)),
           tex_row("", str_c("[", fmt(lenience_stats$corr_lo, 2), ", ", fmt(lenience_stats$corr_hi, 2), "]")),
           tex_row("Rank regression, White on Black", fmt(lenience_stats$rank_coef, 3)),
           tex_row("", str_c("(", fmt(lenience_stats$rank_se, 3), ")")),
           "\\addlinespace",
           tex_row("Pairs of judges compared", fmt(lenience_stats$pairs, 0)),
           tex_row("Same ordering by White and Black leniency", fmt(lenience_stats$same_full, 2)),
           tex_row("Same ordering in random halves: White vs Black", fmt(lenience_stats$same_cross, 2)),
           tex_row("Same ordering in random halves: same race", fmt(lenience_stats$same_within, 2))),
  notes = str_c("Primary analysis. Leniency of a judge toward a race: mean residualized non-carceral sentence (the ",
                "first-step residual of the race-specific instrument) over the judge's convicted cases with a matched ",
                "score of that race in the race samples, per judge x court over the 3-year window centered on each ",
                "year with cases (the cases the instrument averages), relative to the other judges of the same court ",
                "and center year, the comparisons the instrument makes: minus the group's mean weighted by the ",
                "smaller of the two case counts, judge-windows without another judge in their group dropped. ",
                "Correlation weighted by the smaller of the two case counts. Reliability: Spearman-Brown from random halves of ",
                "the judge's defendants, each residualized in its own regression (", n_splits, " splits); corrected ",
                "correlation: correlation between a White and a Black half over the square root of the product of ",
                "the correlations between the two halves of each race (the halves' leniency relative to the group ",
                "likewise); in brackets, its 95% interval from ", n_boot, " bootstrap draws of judges (the splits' ",
                "half means kept). Rank regression: percentile rank (across judge-windows) in White leniency on percentile ",
                "rank in Black leniency, unweighted. Pairs: judges in the same court and window year; same ",
                "ordering: share of pairs ranked the same way."),
  star_note = FALSE)

# judge-windows' White against Black leniency (relative to the court-year's other judges),
# point size by cases
fig <- unit_means(lenience, "judge_windows") |>
  ggplot(aes(m_black, m_white)) +
  geom_hline(yintercept = 0, colour = "grey80") + geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(aes(size = n_white + n_black), shape = 21, fill = "grey60", colour = "grey30", alpha = 0.45) +
  geom_smooth(aes(weight = pmin(n_white, n_black)), method = "lm", formula = y ~ x, colour = "black", linewidth = 0.7) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.5, size = 3.6,
           label = sprintf("%d judge-windows; correlation %.2f (corrected for sampling error: %.2f)",
                           lenience_stats$units[1], lenience_stats$corr[1], lenience_stats$corr_corrected[1])) +
  scale_size_area(max_size = 3.5, guide = "none") +
  labs(x = "Leniency toward Black defendants (relative to the court-year)",
       y = "Leniency toward White defendants (relative to the court-year)") +
  theme_classic(base_size = 13)
f <- file.path(fig_dir, "leniency_by_race_primary.pdf")
ggsave(f, fig, width = 6, height = 4.5)
cat("  wrote", f, "\n")


# ---- recidivism of the incarcerated, by race ----------------------------------------
families <- list(
  "New felony CC case"    = list(pattern = "{stem}{k}yr_{s}_all_any",
                                 status  = c(charged = "Charged", conv = "Convicted", incar = "Incarcerated")),
  "Revocation of the person's other sentences" = list(pattern = "{stem}{k}yr_revother_{s}_all_any",
                                 status  = c(charged = "Proceeding", conv = "Revoked")),
  "Revocation of the focal sentences" = list(pattern = "{stem}{k}yr_revfocal_{s}_all_any",
                                 status  = c(charged = "Proceeding", conv = "Revoked")))

# shares among the incarcerated White and Black defendants of a version's sample (the
# cases of its 2SLS: convicted cases with a matched score in the primary version,
# White and Black only of judges with enough cases of each race), each tested against
# zero (regression on a constant, standard errors clustered by person); all outcomes on
# the same cases, those with full follow-up for 3 years (first disposed in or before
# last_y): measured from the disposition (stem cc_post) or from release (cc_rel<cap>_post)
recid_by_race <- \(v, stem = "cc_post", last_y = full_follow_up_year(str_c(1:3, "yr"))) {
  vs <- version_samples(v, d)
  imap(families, \(fam, family) {
    expand_grid(s = names(fam$status), k = 1:3) |>
      mutate(family = family, y = str_glue(fam$pattern), label = fam$status[s])
  }) |>
    list_rbind() |>
    mutate(order = row_number()) |>
    cross_join(tibble(group = c("white", "black"))) |>
    group_split(group) |>
    map(\(r) {
      x <- vs[[r$group[1]]] |>
        filter(d2 == 1, Y <= last_y) |>
        drop_na(all_of(c(v$controls, v$fe)))
      m <- map(r$y, \(y) feols(as.formula(str_glue("{y} ~ 1")), data = x, cluster = ~person_id, notes = FALSE))
      mutate(r, mean = map_dbl(m, \(m) coef(m)[[1]]), se = map_dbl(m, \(m) se(m)[[1]]),
             p = map_dbl(m, \(m) pvalue(m)[[1]]), n = nrow(x))
    }) |>
    list_rbind() |>
    pivot_wider(names_from = group, values_from = c(mean, se, p, n)) |>
    arrange(order)
}

# table: rows = outcomes by family (share with stars, (se)); columns 1, 2, 3 years,
# each with White and Black; the numbers of incarcerated defendants
write_recid <- \(file, recid, notes) {
  by_year <- \(white, black) c(rbind(white, black))        # w1, b1, w2, b2, w3, b3
  body <- imap(split(recid, recid$family)[names(families)], \(r, family) {
    r <- arrange(r, order)
    rows <- map(unique(r$label), \(l) {
      x <- filter(r, label == l)                           # k = 1, 2, 3
      cells <- coef_cells(by_year(x$mean_white, x$mean_black), by_year(x$se_white, x$se_black),
                          by_year(x$p_white, x$p_black))
      c(tex_row(l, cells$est), tex_row("", cells$se))
    }) |> unlist()
    c(str_c("\\multicolumn{7}{l}{\\textit{", family, "}} \\\\"), rows, "\\addlinespace")
  }) |> unlist()
  ns <- distinct(recid, k, n_white, n_black) |> arrange(k)
  write_tex(file, "lcccccc",
            header = c(" & \\multicolumn{2}{c}{1 year} & \\multicolumn{2}{c}{2 years} & \\multicolumn{2}{c}{3 years} \\\\",
                       "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
                       tex_row("", rep(c("White", "Black"), 3))),
            body = c(head(body, -1), "\\midrule",
                     tex_row("Incarcerated defendants (cases)", fmt(by_year(ns$n_white, ns$n_black), 0))),
            notes = notes)
}
recid_notes <- \(v, from_release = FALSE) str_c(
  v$label, " instruments' sample. Share of incarcerated defendants (d2 = 1, ",
  if (v$convicted_only) cases_note(v) else "analysis sample",
  ") with any new felony CC case charged / convicted / incarcerated, any revocation proceeding / revocation ",
  "of the person's other felony sentences, or of the focal felony sentences (04), within 1, 2 and 3 years ",
  if (from_release) str_c("after release from the focal sentence (first disposition plus ", release_share,
                          " x active carceral time, at most ", release_caps, " years); all outcomes on the cases first ",
                          "disposed in or before ", cc_last_year - 3 - release_caps)
  else str_c("after the case's first disposition; all outcomes on the cases first disposed in or before ",
             full_follow_up_year(str_c(1:3, "yr"))),
  " (full follow-up for 3 years in the circuit-court record, which ends in ", cc_last_year, "). Stars: test that ",
  "the share is zero; standard errors, in parentheses, clustered by person.", race_sample_note(v))

# the primary sample (convicted defendants with a matched score) and the secondary one
# (all defendants; the same with the score control)
walk(c("primary", "secondary"), \(vn) {
  v <- iv_versions[[vn]]
  recid <- recid_by_race(v)
  cat(str_glue("\nRecidivism of incarcerated defendants, by race, {v$label} instruments' sample"), "\n")
  print(select(recid, family, label, mean_white, p_white, mean_black, p_black, n_white, n_black), n = Inf)
  write_recid(file.path(table_dir, str_glue("recidivism_incarcerated_{vn}.tex")), recid, recid_notes(v))

  # the same outcomes measured from release (time served capped at release_caps years)
  recid_rel <- recid_by_race(v, str_c("cc_rel", release_caps, "_post"), cc_last_year - 3 - release_caps)
  cat(str_glue("\nRecidivism of incarcerated defendants after release, by race, {v$label} instruments' sample"), "\n")
  print(select(recid_rel, family, label, mean_white, p_white, mean_black, p_black, n_white, n_black), n = Inf)
  write_recid(file.path(table_dir, str_glue("recidivism_incarcerated_{vn}_from_release_cap{release_caps}.tex")), recid_rel,
              recid_notes(v, from_release = TRUE))
})


# ---- sentence length of the incarcerated, by race -----------------------------------
# active carceral time (carceral_length_inbtw, days: the case's sentences consecutive
# unless the court made them concurrent; Inf for life or death) of the incarcerated
# White and Black defendants of each analysis's 2SLS sample, cases first disposed in or
# before 2016: a figure of the distribution by length (share of each race's incarcerated
# defendants per bin), and in the log the mean (life and death sentences, and any time
# above the 99th percentile, set to the 99th percentile), the median, the shares
# sentenced to 1, 3 and 5 years or more, and the share still in custody 1, 2 and 3 years
# after the disposition (estimated release: release_share x active time, without the cap)
len_bins <- tribble(~lo, ~hi, ~label,
                    0, 0.25, "< 3 months", 0.25, 0.5, "3-6 months", 0.5, 1, "6-12 months",
                    1, 2, "1-2 years", 2, 3, "2-3 years", 3, 5, "3-5 years", 5, 10, "5-10 years",
                    10, Inf, "10+ years, life")
walk(c("primary", "secondary"), \(vn) {
  v <- iv_versions[[vn]]
  vs <- version_samples(v, d)
  yrs <- map(c(White = "white", Black = "black"), \(r) vs[[r]] |>
               filter(d2 == 1, Y <= full_follow_up_year(str_c(1:3, "yr"))) |>
               drop_na(all_of(c(v$controls, v$fe))) |> pull(carceral_length_inbtw) / 365)
  stats <- imap(yrs, \(y, r) {
    capped <- pmin(y, quantile(y, 0.99))
    tibble(race = r, incarcerated = length(y), mean = mean(capped), median = median(y),
           at_least_1 = mean(y >= 1), at_least_3 = mean(y >= 3), at_least_5 = mean(y >= 5),
           custody_1 = mean(release_share * y > 1), custody_2 = mean(release_share * y > 2),
           custody_3 = mean(release_share * y > 3), life_death = mean(is.infinite(y)))
  }) |> list_rbind()
  cat(str_glue("\nSentence length of the incarcerated, {v$label} analysis (years; shares):"), "\n")
  print(stats |> mutate(across(where(is.double), \(x) round(x, 3))), width = Inf)
  shares <- imap(yrs, \(y, r) tibble(race = r, bin = factor(len_bins$label, levels = len_bins$label),
                                     share = map_dbl(seq_len(nrow(len_bins)), \(i)
                                       mean(y >= len_bins$lo[i] & (y < len_bins$hi[i] | i == nrow(len_bins)))))) |>
    list_rbind() |>
    mutate(race = factor(race, levels = c("White", "Black")))
  fig <- ggplot(shares, aes(bin, share, fill = race)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    scale_fill_manual(NULL, values = c(White = "grey70", Black = "grey25")) +
    scale_y_continuous("Share of incarcerated defendants", labels = scales::percent, expand = expansion(c(0, 0.05))) +
    labs(x = "Active carceral time") +
    theme_classic(base_size = 13) +
    theme(legend.position = "inside", legend.position.inside = c(0.85, 0.85), axis.text.x = element_text(angle = 30, hjust = 1))
  f <- file.path(fig_dir, str_glue("sentence_length_{vn}.pdf"))
  ggsave(f, fig, width = 6.5, height = 3.9)
  cat("  wrote", f, "\n")
})
