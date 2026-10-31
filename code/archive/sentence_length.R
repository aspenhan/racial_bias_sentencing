# ==============================================================================
# archive/sentence_length.R  (not part of the pipeline; kept for a possible revisit)
#
# Sentence length and conviction leniency (formerly in 08): the conviction instrument's
# relation with the length of the carceral sentence, which the judge also sets --
# sentence length in years (0 if not incarcerated), and among the incarcerated, in years
# and in logs (life, death and sentences above the 99th percentile set to it), regressed
# on the instrument with the fixed effects of the analysis: alone, with the construction
# controls, and also with the dismissal instrument; all, White and Black defendants; one
# table per instrument version. Run after loading the specification of 00_master.R (up
# to "# ---- run"); writes table_dir/archive/sentence_length_<version>.tex.
# ==============================================================================

table_dir_archive <- file.path(table_dir, "archive")
dir.create(table_dir_archive, showWarnings = FALSE)
d <- read_csv(file.path(data_dir, "final_judge_iv_analysis.csv"), guess_max = Inf, show_col_types = FALSE,
              col_select = c(case_group, person_id, race, d0, d1, d2, carceral_length_inbtw, runningvariable,
                             judgeid, fipscode, Y, gr_jdg_Y, all_of(c(resid_fe, resid_controls, iv_instruments))))

# ---- sentence length -------------------------------------------------------------
# carceral sentence in years (active time, the case's sentences consecutive unless the
# court made them concurrent); life, death and sentences above the 99th percentile of
# the incarcerated (Inf for life or death) are set to that percentile
cap <- quantile(d$carceral_length_inbtw[d$d2 == 1], 0.99)
d <- mutate(d, length_years = pmin(carceral_length_inbtw, cap) / 365,
            log_length = if_else(d2 == 1, log(pmax(pmin(carceral_length_inbtw, cap), 1)), NA))
lengths <- list(all   = list(y = "length_years", label = "Sentence length (years), all cases (0 if not incarcerated)",
                             keep = \(x) x),
                incar = list(y = "length_years", label = "Sentence length (years), incarcerated",
                             keep = \(x) filter(x, d2 == 1)),
                log   = list(y = "log_length", label = "Log sentence length (days), incarcerated",
                             keep = \(x) filter(x, d2 == 1)))

# columns: all, White, Black x three specifications, all with the construction fixed
# effects: (a) the conviction instrument alone; (b) with the construction controls;
# (c) also with the dismissal instrument (which fixes the dismissal margin, so the
# conviction instrument moves cases between non-carceral and carceral sentences)
specs_length <- tribble(~spec, ~controls, ~dism,
                        "a",   FALSE,     FALSE,
                        "b",   TRUE,      FALSE,
                        "c",   TRUE,      TRUE)
walk(names(iv_versions), \(vn) {
  v <- iv_versions[[vn]]
  vs <- version_samples(v, d)
  fits <- expand_grid(len = names(lengths), sample = names(vs), specs_length) |>
    mutate(v_s = map(sample, \(s) for_sample(v, s)),
           m   = pmap(list(len, sample, v_s, controls, dism), \(l, s, v_s, ctrl, w) {
             feols(as.formula(str_glue("{lengths[[l]]$y} ~ {rhs(v_s$z_conv, if (w) v_s$z_dism, if (ctrl) v_s$controls)} | ",
                                       "{rhs(v_s$fe)}")),
                   data = lengths[[l]]$keep(vs[[s]]), cluster = iv_cluster, notes = FALSE)
           }),
           conv = map2(m, v_s, \(m, v_s) coef_cells(coef(m)[[v_s$z_conv]], se(m)[[v_s$z_conv]], pvalue(m)[[v_s$z_conv]])))
  cat(str_glue("\n{v$label} instruments: sentence length on conviction leniency (a: alone; b: + controls; ",
               "c: + controls and dismissal leniency)"), "\n")
  print(transmute(fits, len, sample, spec, coef = map2_dbl(m, v_s, \(m, v_s) coef(m)[[v_s$z_conv]]),
                  p = map2_dbl(m, v_s, \(m, v_s) pvalue(m)[[v_s$z_conv]]), n = map_int(m, nobs)), digits = 3)

  yes_no <- \(x) if_else(x, "Yes", "No")
  body <- imap(lengths, \(l, ln) {
    f <- filter(fits, len == ln)
    c(str_c("\\multicolumn{10}{l}{\\textit{", l$label, "}} \\\\"),
      tex_row("\\quad Conviction leniency", map_chr(f$conv, "est")), tex_row("", map_chr(f$conv, "se")),
      tex_row("\\quad Mean of dependent variable", fmt(map_dbl(f$m, \(m) fitstat(m, "my")[[1]]))),
      tex_row("\\quad Observations", fmt(map_dbl(f$m, nobs), 0)), "\\addlinespace")
  }) |> unlist()
  cols <- filter(fits, len == "all")
  write_tex(
    file.path(table_dir_archive, str_glue("sentence_length_{vn}.tex")), "lccccccccc",
    header = c(" & \\multicolumn{3}{c}{All} & \\multicolumn{3}{c}{White} & \\multicolumn{3}{c}{Black} \\\\",
               "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}",
               tex_row("", str_c("(", 1:9, ")"))),
    body = c(head(body, -1), "\\midrule",
             tex_row("Construction controls", yes_no(cols$controls)),
             tex_row("Dismissal leniency", yes_no(cols$dism))),
    notes = str_glue("{v$label} instruments. Length of the carceral sentence (active time; the case's sentences ",
                     "consecutive unless the court made them concurrent; life, death and sentences above the 99th ",
                     "percentile of the incarcerated, {round(cap / 365, 1)} years, set to it) regressed on the conviction ",
                     "instrument ({str_flatten(unique(map_chr(fits$v_s, 'z_conv')), ' / ')}), with the fixed effects of ",
                     "the instrument construction ({fe_note(v)}); construction controls: ",
                     "{str_flatten(setdiff(v$controls, race_controls), ', ')} and, all defendants only, black; dismissal ",
                     "leniency: the dismissal instrument ({str_flatten(unique(map_chr(fits$v_s, 'z_dism')), ' / ')}). ",
                     "All cases (0 if not incarcerated) and the incarcerated only.",
                     "{if (v$convicted_only) str_c(' ', str_to_sentence(cases_note(v)), ' only.') else ''} Standard ",
                     "errors two-way clustered by defendant and judge x year.{race_sample_note(v)}"))
})


