# ==============================================================================
# archive/by_incarceration_length.R  (not part of the pipeline; kept for a possible revisit)
#
# 2SLS restricted by incarceration length (formerly in 07): only cases not incarcerated
# or with a carceral sentence of at least 3 years (the longest window), for every
# instrument version (iv_versions); otherwise as the 2SLS of 07. This restriction
# conditions on a sentence the judge sets, so it is not a valid design (see the report).
# Run after loading the specification of 00_master.R (up to "# ---- run"); writes
# table_dir/archive/iv_<version>_<family>_by_incarceration_length.tex.
# ==============================================================================

table_dir_archive <- file.path(table_dir, "archive")
dir.create(table_dir_archive, showWarnings = FALSE)
d <- read_csv(file.path(data_dir, "final_judge_iv_analysis.csv"), guess_max = Inf, show_col_types = FALSE,
              col_select = c(case_group, person_id, race, Y, d0, d1, d2, carceral_length_inbtw, runningvariable, judgeid,
                             gr_jdg_Y, gr_jdg_Y_crt, all_of(c(resid_fe, resid_controls, iv_instruments)),
                             
                             matches("^cc_(rel[0-9]+_)?post[0-9to]+yr_(rev_|revfocal_)?(charged|conv|incar)_all_any$")))
race_note <- \(v) if (is.null(v$z_conv_race)) "" else
  str_c(str_glue(" White and Black defendants: the instruments built within the defendant's race ({v$z_conv_race}, ",
                 "{v$z_dism_race})."), race_sample_note(v))
sample_labels <- c(all = "All defendants", white = "White defendants", black = "Black defendants")
# the controls of a version, for the table notes (race: all defendants only)
controls_note <- \(v) str_c(str_flatten(setdiff(v$controls, race_controls), ", "),
                            if (any(race_controls %in% v$controls))
                              str_c(" and, all defendants only, ", str_flatten(race_controls, ", ")) else "")



families <- list(
  new_cases   = list(pattern = "cc_post{w}_{s}_all_any",
                     status  = c(charged = "Charged", conv = "Convicted", incar = "Incarcerated"),
                     label   = "any new felony CC case"),
  revocations_focal = list(pattern = "cc_post{w}_revfocal_{s}_all_any",
                           status  = c(charged = "Proceeding", conv = "Revoked"),
                           label   = str_c("any revocation of the case's own felony sentence (filed under the case, or ",
                                           "under a revocation-only case number with at most one other conviction since the ",
                                           "case's; 0 for dismissed cases)")),
  revocations = list(pattern = "cc_post{w}_rev_{s}_all_any",
                     status  = c(charged = "Proceeding", conv = "Revoked"),
                     label   = str_c("any revocation of a felony sentence set off after the case's first disposition ",
                                     "(of the case's own or a later sentence; of an earlier or unidentified one only ",
                                     "after a new charge)")))
first_years <- c("1yr", "2yr", "3yr")          # windows of the tables (recid_windows)

# 2SLS table: rows the outcomes, by status (e.g. charged / convicted / incarcerated),
# then window; columns White, Black (coefficient, (se), [outcome mean]) and White -
# Black (coefficient, (se)); one sample for all outcomes, so one row of observations;
# labels: the windows' row labels
write_iv_table <- \(file, r, fam, notes, windows = first_years,
                    labels = recid_windows$label[match(windows, recid_windows$window)]) {
  stopifnot(n_distinct(r$white$n) == 1, n_distinct(r$black$n) == 1)
  cells <- map(r, \(x) coef_cells(x$est, x$se, x$p))
  mean_y <- map(r[c("white", "black")], \(x) str_c("[", fmt(x$mean_y), "]"))
  row <- \(i, label) c(tex_row(label, map_chr(cells, \(x) x$est[i])), tex_row("", map_chr(cells, \(x) x$se[i])),
                       tex_row("", c(mean_y$white[i], mean_y$black[i], "")))
  body <- imap(unname(fam$status), \(status, s) {
    c(str_c("\\multicolumn{4}{l}{\\textit{", status, "}} \\\\"),
      unlist(map(seq_along(windows), \(w) row((s - 1) * length(windows) + w, str_c("\\quad ", labels[w])))))
  })
  write_tex(
    file, "lccc",
    header = tex_row("", c("White", "Black", "White $-$ Black")),
    body = c(head(unlist(map(body, \(b) c(b, "\\addlinespace"))), -1), "\\midrule",
             tex_row("Observations", c(fmt(r$white$n[1], 0), fmt(r$black$n[1], 0), ""))),
    notes = notes)
}
iv_notes <- \(v, fam, with_dism = TRUE, when = "within 1, 2 and 3 years", windows = first_years,
              after = "the case's first disposition", last_year = full_follow_up_year(windows)) {
  label <- v$label
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
    "Standard errors two-way clustered by defendant and judge x year. ",
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


# ---- 2SLS by incarceration length ------------------------------------------------------
# Only cases not incarcerated or with a carceral sentence at least as long as the
# longest window, 3 years (carceral_length_inbtw: days of active time, the case's
# sentences consecutive unless the court made them concurrent), the same cases for
# all outcomes; each version, on its samples
min_years <- max(recid_windows$to[match(first_years, recid_windows$window)])
walk(names(iv_versions), \(vn) {
  v <- iv_versions[[vn]]
  vs <- version_samples(v, d)
  walk(names(families), \(fn) {
    fam <- families[[fn]]
    r <- iv_windows(v, fam, vs, first_years, keep = \(x) filter(x, d2 == 0 | carceral_length_inbtw >= 365 * min_years))
    show(r, str_glue("{v$label} 2SLS by incarceration length, {fn}: non-carceral conviction (1, 2, 3 years)"))
    write_iv_table(file.path(table_dir_archive, str_glue("iv_{vn}_{fn}_by_incarceration_length.tex")), r, fam,
                   str_glue("{iv_notes(v, fam)} Only the cases among them not incarcerated or with a ",
                            "carceral sentence of at least {min_years} years (the longest window)."))
  })
})


