# ==============================================================================
# 10_revocation_judges.R
#
# Who decides the analysis defendants' later revocation proceedings and new cases,
# and is a judge's sentencing leniency related to their revocation decisions? Both
# bear on the exclusion restriction for the revocation outcomes.
#   Exposure     White and Black defendants of each analysis's race samples (cases
#                first disposed in or before 2016): their felony revocation
#                proceedings (case x filing date) filed within 3 years after the focal
#                disposition, by the revocation measure that counts them and the
#                sentence they revoke (functions/revocation_kinds.R: other sentences =
#                later sentences, and earlier or unidentified ones after a new case;
#                focal sentences = the focal case and other cases
#                sentenced the same day; neither), and their new felony cases (judge-IV
#                cases) charged within 3 years: the share decided by the focal case's
#                judge, and the share expected if the deciding judge were drawn in
#                proportion to the judges' caseloads in the court and year (all felony
#                revocation proceedings with a judge, or all judge-IV cases; zero for a
#                proceeding or case in another court)
#   Average exclusion   felony revocation proceedings of White and Black defendants
#                filed 2000-2019 before a judge of the analysis in a year in which the
#                judge also sentences analysis cases of the defendant's race: not revoked
#                (no charge revoked) regressed on the leniency of the judge deciding the
#                proceeding toward the defendant's race -- their mean race-specific
#                conviction instrument of the primary analysis (over convicted cases
#                with a matched score) that year over the analysis cases of that race
#                they sentence, also with dismissal leniency; separately for White and
#                Black defendants, court x year fixed effects, standard errors
#                clustered by judge
# The judge of a proceeding: the modal judge on the pleadings of its charges (order
# pleadings first), initials within the court mapped to the judge ids of 03.
#
# Inputs:  data_dir/cc_revocations_full.csv, cc_cases_full.csv, cc_cases_recid.csv, judges.csv,
#          judge_iv_cases.csv, final_judge_iv_analysis.csv; the circuit-court DB (Cases, Pleadings)
# Outputs: table_dir/revocation_judge_exposure_<primary|secondary>.tex,
#          table_dir/revocation_decisions_leniency.tex
# ==============================================================================

an <- read_csv(file.path(data_dir, "final_judge_iv_analysis.csv"), show_col_types = FALSE,
               col_select = c(case_group, person_id, race, Y, d0, runningvariable, judgeid, fipscode, first_dispositiondate,
                              judge_races_main, judge_races_secondary, loo_d1_jyr, loo_d0_jyr, loo_d1_cond_jyr_rv,
                              loo_d1_jyr_race, loo_d0_jyr_race, loo_d1_cond_jyr_rv_race))
judge_iv <- read_csv(file.path(data_dir, "judge_iv_cases.csv"), show_col_types = FALSE,
                     col_select = c(case_group, fipscode, Y, modal_judge_5))
# judge initials are strings (one judge's are "NA"): no NA conversion, and joins never match missing values
judges <- read_csv(file.path(data_dir, "judges.csv"), show_col_types = FALSE, na = character(), col_types = cols(judge = "c"))

# ---- felony revocation proceedings and their judge ---------------------------------
rv <- read_csv(file.path(data_dir, "cc_revocations_full.csv"), show_col_types = FALSE,
               col_types = cols(casenumber_raw = "c"),
               col_select = c(case, person_id, fips, casenumber_raw, filed, class, revoked)) |>
  filter(class == "fel", !is.na(filed))
read_db <- \(years, file, cols) {
  map(years, \(y) read_csv(file.path(orig_dir, "circuit_data", str_glue("DB_CircuitCriminal_{y}"), file),
                           col_select = all_of(cols), col_types = cols(.default = col_character()),
                           na = character())) |>
    list_rbind()
}
link <- read_db(c(2000, 2010:2019), "Cases.csv", c("id", "fips", "CaseNumber")) |>
  transmute(case_id = id, fips = as.numeric(fips), casenumber_raw = CaseNumber) |>
  inner_join(distinct(rv, case, fips, casenumber_raw, filed), by = c("fips", "casenumber_raw"), relationship = "one-to-many")
pl <- read_db(c(2000, 2010:2020), "Pleadings.csv", c("case_id", "Type", "Judge")) |>
  semi_join(link, by = "case_id") |>
  transmute(case_id, type = Type, judge = str_remove_all(if_else(Judge %in% c("XXXX", "3-D", "***"), "", Judge), "[^A-Z]")) |>
  filter(judge != "")
order_types <- c("Revocation Order", "Show Cause Order", "Sentencing Order", "Judgement Order", "Final Order",
                 "Conviction Order", "Commitment Order")
modal <- \(x) x |> count(case, fips, filed, judge) |> arrange(case, fips, filed, desc(n), judge) |>
  distinct(case, fips, filed, .keep_all = TRUE) |> select(case, fips, filed, judge)
x <- inner_join(link, pl, by = "case_id", relationship = "many-to-many")
proc_judge <- bind_rows(mutate(modal(filter(x, type %in% order_types)), step = 1), mutate(modal(x), step = 2)) |>
  arrange(step) |> distinct(case, fips, filed, .keep_all = TRUE) |> select(-step)
proc <- rv |>
  summarise(revoked = as.integer(any(revoked == 1)), .by = c(case, person_id, fips, filed)) |>
  left_join(proc_judge, by = c("case", "fips", "filed"), relationship = "many-to-one") |>
  mutate(fipscode = if_else(fips %in% c(93, 175, 620, 800), 999, fips), year = year(filed)) |>
  left_join(judges, by = c("judge", "fipscode"), relationship = "many-to-one", na_matches = "never")
cat("felony revocation proceedings:", format(nrow(proc), big.mark = ","), "| with a judge:",
    round(mean(!is.na(proc$judge)), 3), "| with a judge of the analysis:", round(mean(!is.na(proc$judge_id)), 3), "\n")
rm(x, pl, link); gc()

# caseload shares of the judges in each court and year (proceedings with a judge)
rev_load <- proc |> filter(!is.na(judge)) |>
  count(fipscode, year, judge) |> mutate(share = n / sum(n), .by = c(fipscode, year)) |>
  left_join(judges, by = c("judge", "fipscode"), na_matches = "never") |> filter(!is.na(judge_id)) |> select(fipscode, year, judge_id, share)
case_load <- judge_iv |> count(fipscode, Y, modal_judge_5) |> mutate(share = n / sum(n), .by = c(fipscode, Y)) |> select(-n)

# ---- exposure: proceedings and new cases of the analysis defendants ----------------
# what each proceeding revokes relative to the focal case, and whether a new case came
# before it (functions/revocation_kinds.R)
person_cases <- person_cases_of(unique(an$person_id))
convicted_cases <- an$case_group[an$d0 == 0]
recid <- read_csv(file.path(data_dir, "cc_cases_recid.csv"), show_col_types = FALSE, col_select = c(case, person_id, case_date))
# rows: the kinds of proceedings, grouped by the measure that counts them, and new cases
kinds <- tribble(
  ~kind,          ~measure,  ~label,
  "later",        "other",   "Revocation proceedings: later sentences",
  "earlier_new",  "other",   "Revocation proceedings: earlier sentences, after a new case",
  "unknown_new",  "other",   "Revocation proceedings: revocation-only case nos., after a new case",
  "other",        "other",   "Revocation proceedings: all of the primary measure",
  "focal",        "focal",   "Revocation proceedings: filed under the focal case",
  "same_day",     "focal",   "Revocation proceedings: other cases sentenced at the focal hearing",
  "focal_all",    "focal",   "Revocation proceedings: all of the secondary measure",
  "neither",      "neither", "Revocation proceedings: earlier or unidentified, no new case",
  "newcase",      "newcase", "New felony cases (not revocation proceedings)")
measure_titles <- c(other   = "Counted in the primary revocation measure (other sentences)",
                    focal   = "Counted in the secondary revocation measure (focal sentences)",
                    neither = "Counted in neither revocation measure",
                    newcase = "Not revocation proceedings")
exposure <- \(vn) {
  v <- iv_versions[[vn]]
  foc <- an |> filter(race %in% c("White", "Black"), Y <= full_follow_up_year(str_c(1:3, "yr")),
                      .data[[v$race_sample]] == 1, !is.na(.data[[v$z_conv_race]]),
                      !v$convicted_only | (d0 == 0 & !is.na(runningvariable))) |>
    select(case_group, person_id, focal_judge = judgeid, focal_court = fipscode, focal_disp = first_dispositiondate)
  p <- proc |>
    inner_join(foc, join_by(person_id, filed > focal_disp), relationship = "many-to-many") |>
    filter(filed <= focal_disp + 3 * 365) |>
    mutate(date = filed, focal_dispdate = focal_disp) |>
    revocation_kind(person_cases, convicted_cases) |>
    mutate(kind = case_when(revokes %in% c("later", "focal", "same_day") ~ revokes,
                            revokes %in% c("earlier", "unknown") & new_charge ~ str_c(revokes, "_new"),
                            revokes %in% c("earlier", "unknown") ~ "neither"),
           measure = case_when(kind %in% c("later", "earlier_new", "unknown_new") ~ "other",
                               kind %in% c("focal", "same_day") ~ "focal", kind == "neither" ~ "neither")) |>
    filter(!is.na(kind)) |>                                  # filed under a dismissed focal case
    left_join(rename(rev_load, focal_judge = judge_id, chance = share),
              by = c("fipscode", "year", "focal_judge")) |>
    mutate(same_judge = coalesce(fipscode == focal_court & judge_id == focal_judge, FALSE),
           chance = if_else(fipscode == focal_court, coalesce(chance, 0), 0))
  shares <- \(x) summarise(x, n = n(), same = mean(same_judge), chance = mean(chance))
  with_judge <- filter(p, !is.na(judge))
  rows <- bind_rows(shares(group_by(with_judge, kind)),
                    shares(group_by(filter(with_judge, measure == "other"), kind = "other")),
                    shares(group_by(filter(with_judge, measure == "focal"), kind = "focal_all")))
  nc <- recid |>
    inner_join(foc, join_by(person_id, case_date > focal_disp), relationship = "many-to-many") |>
    filter(case != case_group, case_date <= focal_disp + 3 * 365) |>
    inner_join(rename(judge_iv, case = case_group, new_judge = modal_judge_5, new_court = fipscode, new_Y = Y), by = "case") |>
    left_join(rename(case_load, new_court = fipscode, new_Y = Y, focal_judge = modal_judge_5, chance = share),
              by = c("new_court", "new_Y", "focal_judge")) |>
    summarise(n = n(), same = mean(new_court == focal_court & new_judge == focal_judge),
              chance = mean(if_else(new_court == focal_court, coalesce(chance, 0), 0))) |>
    mutate(kind = "newcase")
  cat(sprintf("\n%s analysis: proceedings within 3 years %d, with a judge %.3f; share of the proceedings by kind:\n",
              vn, nrow(p), mean(!is.na(p$judge))))
  print(count(p, measure, kind) |> mutate(share = round(n / sum(n), 3)))
  left_join(select(kinds, kind), bind_rows(rows, nc), by = "kind")
}
ex <- map(c(primary = "primary", secondary = "secondary"), exposure)
print(ex)
pct <- \(z) sprintf("%.0f", 100 * z)
# one table per analysis (revocation_judge_exposure_<primary|secondary>.tex)
iwalk(ex, \(e, an) {
  ex_row <- \(i, label) tex_row(label, c(fmt(e$n[i], 0), pct(e$same[i]), pct(e$chance[i])))
  body <- imap(measure_titles, \(title, m) {
    i <- which(kinds$measure == m)
    c(str_c("\\multicolumn{4}{l}{\\textit{", title, "}} \\\\"), map_chr(i, \(j) ex_row(j, str_c("\\quad ", kinds$label[j]))))
  }) |> unname()
  write_tex(file.path(table_dir, str_glue("revocation_judge_exposure_{an}.tex")), "lccc",
          header = tex_row("", c("N", "Focal judge (\\%)", "At random (\\%)")),
          body = head(unlist(map(body, \(b) c(b, "\\addlinespace"))), -1),
          notes = str_c(str_to_sentence(an), " analysis. White and Black defendants of the race samples, cases first disposed in or before ",
                        full_follow_up_year(str_c(1:3, "yr")), ": their felony revocation proceedings (charges of a case ",
                        "filed on one date) filed within 3 years after the focal disposition, by the revocation measure ",
                        "that counts them and the case they are filed under, and their new felony cases (judge-IV cases) ",
                        "charged within 3 years. N: proceedings (cases) with judge information on their pleadings. Focal ",
                        "judge: share decided by the focal case's judge. At random: share expected if the deciding judge ",
                        "were drawn in proportion to the judges' caseloads in the court and year (all felony revocation ",
                        "proceedings, or all judge-IV cases), zero in another court."),
          star_note = FALSE)
})

# ---- average exclusion: revocation decisions and the judge's leniency --------------
# race of the proceeding's defendant, as in 04: modal over the person's charges in the
# full CC record (functions/person_demog.R)
proc_race <- cc_charge_demog(file.path(orig_dir, "Circuit criminal 2000-2019", "circuit_2000_2019.csv")) |>
  semi_join(proc, by = "person_id") |>
  person_demog() |>
  filter(var == "race") |>
  select(person_id, race = value)
# the deciding judge's leniency toward the defendant's race, as the race-specific
# instruments of the primary analysis: their mean race-specific instrument in the year
# over the analysis cases of that race they sentence (conviction: convicted cases with
# a matched score; dismissal: all cases; empty means are NaN, dropped below)
jl <- an |> filter(race %in% c("White", "Black")) |>
  summarise(conv_len = mean(loo_d1_cond_jyr_rv_race[d0 == 0 & !is.na(runningvariable)], na.rm = TRUE),
            dism_len = mean(loo_d0_jyr_race, na.rm = TRUE), .by = c(judgeid, fipscode, Y, race))
# outcome: not revoked (the sentence kept), so that a positive coefficient means a judge
# lenient in sentencing is also lenient in revoking; separately for White and Black
# defendants, court x year fixed effects
pa <- proc |> filter(!is.na(judge_id), year <= cc_last_year) |>
  inner_join(proc_race, by = "person_id", relationship = "many-to-one") |>
  inner_join(jl, by = c("judge_id" = "judgeid", "fipscode", "year" = "Y", "race")) |>
  mutate(court_year = str_c(fipscode, "_", year), not_revoked = 1L - revoked)
cat("\nproceedings before a judge of the analysis, by the defendant's race:\n")
print(count(proc |> filter(!is.na(judge_id), year <= cc_last_year) |> left_join(proc_race, by = "person_id"), race))
specs <- list(c("conv_len"), c("conv_len", "dism_len"))
# columns: White (1)-(2), Black (3)-(4)
cols <- expand_grid(race = c("White", "Black"), spec = seq_along(specs)) |>
  mutate(z = specs[spec], data = map2(race, z, \(r, z) filter(pa, race == r, if_all(all_of(z), \(q) !is.na(q)))))
fits <- map2(cols$z, cols$data, \(z, dd) feols(as.formula(str_glue("not_revoked ~ {str_flatten(z, ' + ')} | court_year")),
                                               dd, cluster = ~judge_id, notes = FALSE))
cell <- \(m, z) if (z %in% names(coef(m))) coef_cells(coef(m)[[z]], se(m)[[z]], pvalue(m)[[z]]) else list(est = "", se = "")
rows_for <- \(z, label) { cs <- map(fits, \(m) cell(m, z))
  c(tex_row(label, map_chr(cs, "est")), tex_row("", map_chr(cs, "se"))) }
# SD of the conviction leniency within court x year (the variation the regressions use)
sd_len <- map2_chr(cols$z, cols$data, \(z, dd) sprintf("%.3f", sd(dd[[z[1]]] - ave(dd[[z[1]]], dd$court_year))))
write_tex(file.path(table_dir, "revocation_decisions_leniency.tex"), str_c("l", strrep("c", nrow(cols))),
          header = c(group_header(c("White defendants", "Black defendants"), length(specs)),
                     tex_row("", str_c("(", seq_len(nrow(cols)), ")"))),
          body = c(rows_for("conv_len", "Deciding judge's conviction leniency"),
                   rows_for("dism_len", "Deciding judge's dismissal leniency"),
                   "\\midrule",
                   tex_row("SD of conviction leniency within court $\\times$ year", sd_len),
                   tex_row("Mean of not revoked", map2_chr(fits, cols$data, \(m, dd) sprintf("%.3f", mean(dd$not_revoked[obs(m)])))),
                   tex_row("Proceedings", map_chr(fits, \(m) fmt(nobs(m), 0))),
                   tex_row("Judges", map2_chr(fits, cols$data, \(m, dd) fmt(n_distinct(dd$judge_id[obs(m)]), 0)))),
          notes = str_c("Felony revocation proceedings of White (columns 1-2) and Black (columns 3-4) defendants filed ",
                        "2000-", cc_last_year, " before a judge of the analysis in a year in which the judge also ",
                        "sentences analysis cases of the defendant's race. Dependent variable: not revoked (no charge of ",
                        "the proceeding ends with the sentence revoked or the violation found). Regressors: the leniency ",
                        "of the judge deciding the proceeding (not of the judge who imposed the revoked sentence) toward ",
                        "the defendant's race, measured by their sentencing decisions: their mean race-specific instrument ",
                        "of the primary analysis in the year over the analysis cases of that race they sentence ",
                        "(conviction leniency: over convicted cases with a matched score; dismissal leniency: over all ",
                        "cases). Court x year fixed effects; standard errors, in parentheses, clustered by judge."))
cat("\nAverage exclusion: not revoked on the deciding judge's leniency\n")
etable(fits)
