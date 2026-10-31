# ==============================================================================
# 11_data_notes.R
#
# Tables of the data notes (Overleaf: data_notes.tex, section "Racial shares in the sample"):
#   data_notes_race_steps.tex    White, Black and other defendants at each step from
#                                the full circuit-court record to the analysis samples
#                                (race: the defendant's modal race over all of their
#                                charges, 04; for charges, the race recorded on the charge)
#   data_notes_race_courts.tex   the same by court: the 19 courts of the analysis sample,
#                                and the courts the model-17 court list leaves out
#   data_notes_race_paper.tex    Black share at the model-17 steps in our data and in the
#                                Revolving Door replication data (its case file before the
#                                restrictions and its final file; Black: from the case's
#                                own charges), in the replication code's order (courts,
#                                then judge caseloads) and in the reverse order of the
#                                paper's appendix. The judge rules are computed within
#                                court, so both orders end with the same cases
#
# Inputs:  orig_dir/Circuit criminal 2000-2019/circuit_2000_2019.csv   (race)
#          circuit_xwalk_path                                           (court names)
#          data_dir/cc_cases_full.csv, cc_felony_charges.csv, cc_person_level_data.csv,
#          final_judge_iv_analysis.csv
#          paper_data_dir/person_level_data_casenumber_part1only.dta, final_judge_iv_data.dta
# Outputs: table_dir/data_notes_race_steps.tex, data_notes_race_courts.tex,
#          data_notes_race_paper.tex
# ==============================================================================

race_groups <- c("White", "Black", "Other or unknown")
race_group  <- \(race) factor(if_else(race %in% c("White", "Black"), race, "Other or unknown"), race_groups)

# counts by race group (unit: one row of x), the White and Black shares, and the
# number of courts (if x has them)
composition <- \(x) x |>
  count(race = race_group(race), .drop = FALSE) |>
  pivot_wider(names_from = race, values_from = n) |>
  transmute(n = White + Black + `Other or unknown`, white = White, black = Black, other = `Other or unknown`,
            white_share = white / n, black_share = black / n,
            courts = if ("fipscode" %in% names(x)) n_distinct(x$fipscode) else NA_integer_)
pct <- \(x) num(100 * x, 1)
# cells of a count that may be missing or zero (blank)
num_or_blank <- \(x) if_else(is.na(x) | x == 0, "", num(x, 0))


# ---- race of each charge and defendant in the full record -----------------------
charges <- cc_charge_demog(file.path(orig_dir, "Circuit criminal 2000-2019", "circuit_2000_2019.csv"))
persons <- distinct(charges, person_id) |>
  left_join(person_demog(charges) |> filter(var == "race") |> select(person_id, race = value),
            by = "person_id", relationship = "one-to-one")             # NA: no race recorded
with_race <- \(x) left_join(x, persons, by = "person_id", relationship = "many-to-one")

cases   <- read_csv(file.path(data_dir, "cc_cases_full.csv"), col_select = c(case, person_id),
                    show_col_types = FALSE) |>
  distinct()
felony  <- read_csv(file.path(data_dir, "cc_felony_charges.csv"), col_select = c(case, person_id),
                    show_col_types = FALSE) |>
  distinct()
stopifnot(!anyDuplicated(cases$case), !anyDuplicated(felony$case))     # one defendant per case
judge_iv <- read_csv(file.path(data_dir, "cc_person_level_data.csv"),
                     col_select = c(case_group, person_id, fipscode, Y, modal_judge_5, race), show_col_types = FALSE) |>
  mutate(judgeid = modal_judge_5)
an <- read_csv(file.path(data_dir, "final_judge_iv_analysis.csv"), show_col_types = FALSE,
               col_select = c(case_group, race, fipscode, Y, d0, runningvariable, judge_races_main,
                              judge_races_secondary))

# the analysis's race (04) is the one rebuilt here
stopifnot(with(left_join(judge_iv, persons, by = "person_id", suffix = c("", "_record")),
               all(coalesce(race, "") == coalesce(race_record, ""))))


# ---- race at each step ---------------------------------------------------------
# the model-17 steps of 06 (00_master: model17_years, model17_sample); the judge rules
# on all courts (the reverse order) end with the same cases in the 19 courts
in_years   <- model17_years(judge_iv)
in_courts  <- filter(in_years, fipscode %in% model17_sample$court_fipscode_list)
in_judges  <- exec(subset_judgeiv_data, in_years, !!!model17_sample)
all_courts <- \(d) modifyList(model17_sample, list(court_fipscode_list = unique(d$fipscode)))
judges_all <- exec(subset_judgeiv_data, in_years, !!!all_courts(in_years))
stopifnot(all(an$case_group %in% in_judges$case_group),
          setequal(in_judges$case_group,
                   filter(judges_all, fipscode %in% model17_sample$court_fipscode_list)$case_group))
primary   <- version_samples(iv_versions$primary, filter(an, !is.na(runningvariable)))
secondary <- version_samples(iv_versions$secondary, an)

steps <- list(
  `Full circuit-court record, 2000--2019` = list(
    "Charges"                                            = charges,
    "Defendants"                                         = persons,
    "Cases"                                              = with_race(cases),
    "Cases with a felony charge (not a revocation)"      = with_race(felony)),
  `Judge-IV cases` = list(
    "Dated, weekday disposition, judge identified"       = judge_iv,
    "First disposed by 2018"                             = in_years,
    "In the 19 courts of the original sample"            = in_courts,
    "Judges with enough cases"                           = in_judges,
    "With the instruments: analysis sample"              = an),
  `Analysis samples` = list(
    "Secondary analysis: White and Black samples"        = bind_rows(secondary[c("white", "black")]),
    "Primary analysis: convicted with a matched score"   = primary$full,
    "Primary analysis: White and Black samples"          = bind_rows(primary[c("white", "black")]))) |>
  map(\(panel) imap(panel, \(x, label) mutate(composition(x), label = label)) |> list_rbind()) |>
  list_rbind(names_to = "panel")
print(steps, width = Inf)
# the cases the judge rules drop within the 19 courts
dropped <- anti_join(in_courts, in_judges, by = "case_group")
cat("judge rules within the 19 courts drop", nrow(dropped), "cases, White share", round(mean(dropped$race %in% "White"), 3), "\n")
# the reverse order: the judge rules on all courts, before the court list
cat("judge rules on all courts first:\n")
print(composition(judges_all), width = Inf)
# the race samples' rule: share of each race's cases kept, and the White share among White
# and Black defendants before and after it
list(secondary = secondary, primary = primary) |>
  imap(\(s, vn) tibble(version = vn, kept_white = nrow(s$white) / sum(s$full$race %in% "White"),
                       kept_black = nrow(s$black) / sum(s$full$race %in% "Black"),
                       white_share_before = mean(s$full$race[s$full$race %in% c("White", "Black")] == "White"),
                       white_share_after = nrow(s$white) / (nrow(s$white) + nrow(s$black)))) |>
  list_rbind() |>
  print(width = Inf)
# the primary analysis's restriction, by race: conviction, and a matched score among the convicted
an |>
  mutate(race = race_group(race)) |>
  summarise(cases = n(), convicted = mean(d0 == 0), score_if_convicted = mean(!is.na(runningvariable[d0 == 0])),
            .by = race) |>
  print()

# one row of the table (x: one row of steps); the race samples have no other race
step_row <- \(x) tex_row(str_c("\\quad ", x$label),
                         c(num(x$n, 0), num_or_blank(x$courts), num(x$white, 0), num(x$black, 0), num_or_blank(x$other),
                           pct(x$white_share), pct(x$black_share)))
write_tex(file.path(table_dir, "data_notes_race_steps.tex"),
          cols = "lrrrrrrr",
          header = c(" & & & \\multicolumn{3}{c}{Defendants' race} & \\multicolumn{2}{c}{Share (\\%)} \\\\",
                     "\\cmidrule(lr){4-6}\\cmidrule(lr){7-8}",
                     " & Total & Courts & White & Black & Other & White & Black \\\\"),
          body = split(steps, fct_inorder(steps$panel)) |>
            map(\(p) c(str_c("\\multicolumn{8}{l}{\\textit{", p$panel[1], "}} \\\\"),
                       map_chr(seq_len(nrow(p)), \(i) step_row(p[i, ])))) |>
            unlist(use.names = FALSE),
          notes = str_c("Race at each step of the sample. Race: the defendant's modal race over all of their charges ",
                        "(charges: the race recorded on the charge). Other: other races and unknown. Courts: ",
                        "circuit-court jurisdictions (circuit 5 is one court)."),
          star_note = FALSE)


# ---- race by court -------------------------------------------------------------
court_names <- haven::read_dta(circuit_xwalk_path, col_select = c(countyfip, name)) |>
  distinct(fipscode = countyfip, court = name) |>
  bind_rows(tibble(fipscode = 999, court = "Circuit 5 (Isle of Wight, Southampton, Franklin City, Suffolk)"))
# each court's share of all White and of all Black defendants' cases of a sample (NA:
# the court has none of the race's cases in it)
race_dist <- \(x, prefix) x |>
  filter(race %in% c("White", "Black")) |>
  count(fipscode, race) |>
  mutate(share = n / sum(n), .by = race) |>
  pivot_wider(id_cols = fipscode, names_from = race, values_from = share) |>
  rename_with(\(v) str_c(prefix, str_to_lower(v)), c(White, Black))
race_samples <- list(all = an, sec = bind_rows(secondary[c("white", "black")]), prim = bind_rows(primary[c("white", "black")]))
by_court <- an |>
  nest(.by = fipscode) |>
  mutate(comp = map(data, composition)) |>
  unnest(comp) |>
  left_join(court_names, by = "fipscode", relationship = "one-to-one") |>
  left_join(reduce(imap(race_samples, \(x, s) race_dist(x, str_c(s, "_"))), \(a, b) full_join(a, b, by = "fipscode")),
            by = "fipscode", relationship = "one-to-one") |>
  arrange(desc(black_share))
stopifnot(!anyNA(by_court$court))
left_out <- filter(in_years, !fipscode %in% model17_sample$court_fipscode_list)
cat("courts left out by the court list:", n_distinct(left_out$fipscode), "| mean cases per court: left out",
    round(nrow(left_out) / n_distinct(left_out$fipscode)), ", in the list", round(nrow(in_courts) / 19), "\n")
print(select(by_court, court, n, white_share, black_share, matches("^(all|sec|prim)_")), n = Inf, width = Inf)
# majority-Black courts: their share of the cases, and of each race's cases in each sample
by_court |>
  mutate(majority_black = black_share > 0.5) |>
  summarise(courts = n(), across(c(n, matches("^(all|sec|prim)_")), \(x) sum(x, na.rm = TRUE)), .by = majority_black) |>
  mutate(share_cases = n / sum(n)) |>
  print(width = Inf)
# four cities and the southwestern counties: share of each race's cases in each sample
list(cities = c(760, 710, 740, 700), southwest = c(155, 185, 195, 169)) |>
  imap(\(f, g) by_court |> filter(fipscode %in% f) |>
         summarise(group = g, across(matches("^(all|sec|prim)_"), \(x) sum(x, na.rm = TRUE)))) |>
  list_rbind() |>
  print(width = Inf)

pct_or_dash <- \(x) if_else(is.na(x), "--", pct(x))
court_row <- \(label, x, dist = TRUE) tex_row(label, c(num(x$n, 0), pct(x$white_share), pct(x$black_share),
                                                    if (dist) pct_or_dash(c(x$sec_white, x$sec_black, x$prim_white, x$prim_black))
                                                    else rep("", 4)))
write_tex(file.path(table_dir, "data_notes_race_courts.tex"),
          cols = "lrrrrrrr",
          header = c(" & & \\multicolumn{2}{c}{Of the court (\\%)} & \\multicolumn{4}{c}{Of the race's cases (\\%)} \\\\",
                     "\\cmidrule(lr){3-4}\\cmidrule(lr){5-8}",
                     " & & & & \\multicolumn{2}{c}{Secondary} & \\multicolumn{2}{c}{Primary} \\\\",
                     "\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}",
                     "Court & Cases & White & Black & White & Black & White & Black \\\\"),
          body = c(map_chr(seq_len(nrow(by_court)), \(i) court_row(by_court$court[i], by_court[i, ])),
                   "\\midrule",
                   court_row("All 19 courts", mutate(composition(an), sec_white = 1, sec_black = 1, prim_white = 1,
                                                    prim_black = 1)),
                   court_row(str_c(n_distinct(left_out$fipscode), " other courts (cases first disposed by 2018)"),
                             composition(left_out), dist = FALSE)),
          notes = str_c("Analysis sample (cases with the instruments) by court, ordered by the Black share. Of the ",
                        "court: White and Black defendants' shares of the court's cases. Of the race's cases: the ",
                        "court's share of all White and of all Black defendants' cases in the White and Black samples ",
                        "of the secondary and primary analyses (--: none). Other courts: judge-IV cases first ",
                        "disposed by 2018 in the courts outside the original sample's court list."),
          star_note = FALSE)


# ---- the same steps in the Revolving Door replication data ------------------------
# its case file before the restrictions, prepared as make_data_judge_iv_clean.do (years,
# Virginia Beach, circuit 5 as one court, judge = modal_judge), and its final file
paper_cases <- haven::read_dta(file.path(paper_data_dir, "person_level_data_casenumber_part1only.dta"),
                               col_select = c(case_group, fipscode, Y, black, modal_judge)) |>
  haven::zap_labels() |>
  mutate(fipscode = if_else(fipscode %in% c(93, 175, 620, 800), 999, fipscode), judgeid = modal_judge)
paper_final <- haven::read_dta(file.path(paper_data_dir, "final_judge_iv_data.dta"),
                               col_select = c(case_group, fipscode, Y, black)) |>
  haven::zap_labels()
p_years      <- model17_years(filter(paper_cases, !is.na(Y)))
p_judges     <- exec(subset_judgeiv_data, p_years, !!!model17_sample)
p_judges_all <- exec(subset_judgeiv_data, p_years, !!!all_courts(p_years))
stopifnot(all(paper_final$case_group %in% p_judges$case_group),
          setequal(p_judges$case_group,
                   filter(p_judges_all, fipscode %in% model17_sample$court_fipscode_list)$case_group))

# cases, courts and Black share of a step, ours (modal race) and the paper's (black)
black_comp <- \(x, black) tibble(n = nrow(x), courts = n_distinct(x$fipscode), black_share = mean(black))
ours  <- \(x) black_comp(x, x$race %in% "Black")
paper <- \(x) black_comp(x, x$black %in% 1)
in_list <- \(x) filter(x, fipscode %in% model17_sample$court_fipscode_list)
paper_steps <- list(
  `Courts, then judge caseloads (the replication code's order)` = list(
    "Judge-IV cases with a judge"                   = list(judge_iv, paper_cases),
    "First disposed by 2018"                        = list(in_years, p_years),
    "In the 19 courts of the original sample"       = list(in_courts, in_list(p_years)),
    "Judges with enough cases"                      = list(in_judges, p_judges),
    "With the instruments: analysis sample"         = list(an, paper_final),
    "\\quad first disposed by 2012"                 = list(filter(an, Y <= 2012), filter(paper_final, Y <= 2012))),
  `Judge caseloads, then courts (the order of the paper's appendix)` = list(
    "Judges with enough cases, all courts"          = list(judges_all, p_judges_all),
    "In the 19 courts of the original sample"       = list(in_list(judges_all), in_list(p_judges_all)))) |>
  map(\(panel) imap(panel, \(x, label) bind_cols(label = label, rename_with(ours(x[[1]]), \(v) str_c("o_", v)),
                                                 rename_with(paper(x[[2]]), \(v) str_c("p_", v)))) |>
        list_rbind()) |>
  list_rbind(names_to = "panel")
print(paper_steps, width = Inf)
# in the reverse order: the courts the judge rules drop entirely, and the courts that pass
# them but are not on the list; the paper's appendix counts (465,541 cases with a judge ->
# 446,764 after the judge rules -> 324,799 after the court list) vs its case file
off_list <- \(x) filter(x, !fipscode %in% model17_sample$court_fipscode_list)
cat("courts failing the judge rules: ours", n_distinct(in_years$fipscode) - n_distinct(judges_all$fipscode),
    "of", n_distinct(in_years$fipscode), "| paper", n_distinct(p_years$fipscode) - n_distinct(p_judges_all$fipscode),
    "of", n_distinct(p_years$fipscode), "\n")
cat("courts passing the judge rules but not on the list (ours):\n")
print(composition(off_list(judges_all)), width = Inf)
cat("their cases, paper:", nrow(off_list(p_judges_all)), "Black share", round(mean(off_list(p_judges_all)$black %in% 1), 3), "\n")
cat("paper, cases first disposed by 2018: the judge rules on all courts drop", nrow(p_years) - nrow(p_judges_all), "of",
    nrow(p_years), "cases\n")
cat("paper case file:", nrow(paper_cases), "cases | judge rules on all courts, all years:",
    nrow(exec(subset_judgeiv_data, paper_cases, !!!all_courts(paper_cases))),
    "| cases in the 19 courts, all years:", nrow(in_list(paper_cases)), "\n")

paper_row <- \(x) tex_row(str_c("\\quad ", x$label),
                          c(num(x$o_n, 0), num(x$o_courts, 0), pct(x$o_black_share),
                            num(x$p_n, 0), num(x$p_courts, 0), pct(x$p_black_share)))
write_tex(file.path(table_dir, "data_notes_race_paper.tex"),
          cols = "lrrrrrr",
          header = c(" & \\multicolumn{3}{c}{Our data} & \\multicolumn{3}{c}{Revolving Door data} \\\\",
                     "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
                     " & Cases & Courts & Black (\\%) & Cases & Courts & Black (\\%) \\\\"),
          body = split(paper_steps, fct_inorder(paper_steps$panel)) |>
            map(\(p) c(str_c("\\multicolumn{7}{l}{\\textit{", p$panel[1], "}} \\\\"),
                       map_chr(seq_len(nrow(p)), \(i) paper_row(p[i, ])))) |>
            unlist(use.names = FALSE),
          notes = str_c("Black share at the model-17 steps, in our data (Black: the defendant's modal race) and in ",
                        "the Revolving Door replication package (person_level_data_casenumber_part1only.dta, prepared ",
                        "as in make_data_judge_iv_clean.do, and final_judge_iv_data.dta; Black: from the case's own ",
                        "charges). The judge rules are computed within court, so both orders end with the same cases."),
          star_note = FALSE)
