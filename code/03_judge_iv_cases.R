# ==============================================================================
# 03_judge_iv_cases.R
#
# The judge-IV cases, built from the cases of the full CC record (02) rather than
# the original case groups: every case of the full record with felony charges that
# are not revocation proceedings (cc_felony_charges); case_group = its case id. As
# in the original (Data_setup_JudgeIV_casenumber_part1only.do), only these felony
# charges define the case:
#   convict, incar   the outcome rule of 02: convicted if any charge is convicted
#                    (Guilty; Appeal Withdrawn, Resolved or no disposition with a
#                    penalty), incarcerated if a convicted charge has active jail /
#                    penitentiary time or a life or death sentence; cond_incar =
#                    incar among the convicted
#   carceral_length_concur / _consec / _inbtw   days of active time: the longest /
#                    the sum over the incarcerated charges; inbtw = the sum, or the
#                    longest if any charge is concurrent (as the original); Inf with a
#                    life or death sentence
#   first_offdate, first_dispositiondate; Y, month, dow (0 = Sunday) of the first
#                    disposition
#   nb_<type>, any_<type>   number of / any charges of each offense type
#                    (functions/offense_types.R)
#   nb_m, any_m      misdemeanor charges in the case (not revocation proceedings)
#   modal_judge_5    the judge, from the pleadings of the charges (the original's
#                    Parts 4-7, method 5): the modal judge over the pleadings of type
#                    sentencing / judgement / final / conviction / dismissal /
#                    commitment order; else over those whose remarks mention a
#                    sentencing, dismissal, nolle prosequi or final order; else over
#                    those filed on the charge's disposition date; else over all of the
#                    case's pleadings. Ties go to the larger judge id (judges numbered
#                    by initials, then court). Judges are court-specific; circuit 5 is
#                    one court (fipscode 999).
# Sample, as the original's case-level file: cases with a first offense and a first
# disposition date, first disposed on a weekday, with a judge on their pleadings
# (linked by the case_id of the Cases files), in courts where at least 80% of the
# pleadings belong to cases with a judge on some pleading.
#
# Inputs:  data_dir/cc_felony_charges.csv, data_dir/cc_cases_full.csv
#          orig_dir/circuit_data/DB_CircuitCriminal_<year>/Cases.csv, Pleadings.csv
# Output:  data_dir/judge_iv_cases.csv; data_dir/judges.csv (judge id of each court x judge initials, for 10)
# ==============================================================================

charges <- read_csv(file.path(data_dir, "cc_felony_charges.csv"), show_col_types = FALSE,
                    col_types = cols(casenumber_raw = "c", codesection = "c", felony_class = "c",
                                     dispositioncode = "c")) |>
  rename(case_group = case)

# ---- case record ------------------------------------------------------------------
cases <- charges |>
  summarise(person_id = first(person_id), fipscode_raw = first(fips), n_charges = n(),
            convict = as.integer(any(convicted == 1)), incar = as.integer(any(incarcerated == 1)),
            life_death = as.integer(any(life_death == 1)),
            carceral_length_concur = max(carceral_days), carceral_length_consec = sum(carceral_days),
            any_conc = any(concurrent == 1), .by = case_group) |>
  mutate(cond_incar = if_else(convict == 1, incar, NA),
         carceral_length_inbtw = if_else(any_conc, carceral_length_concur, carceral_length_consec),
         across(starts_with("carceral_length_"), \(x) if_else(life_death == 1, Inf, x))) |>
  select(-any_conc)

# earliest offense and disposition date: each case's first row after sorting
first_date <- \(date) charges |>
  filter(!is.na(.data[[date]])) |>
  arrange(.data[[date]]) |>
  distinct(case_group, .keep_all = TRUE) |>
  select(case_group, all_of(date))
cases <- cases |>
  left_join(rename(first_date("offensedate"), first_offdate = offensedate), by = "case_group") |>
  left_join(rename(first_date("dispositiondate"), first_dispositiondate = dispositiondate), by = "case_group")

# offense types of the charges; misdemeanor charges of the case
types <- charges |>
  mutate(type = factor(offense_type_of(codesection), levels = offense_type_names)) |>
  count(case_group, type) |>
  pivot_wider(names_from = type, values_from = n, values_fill = 0, names_expand = TRUE, names_glue = "nb_{type}") |>
  mutate(across(starts_with("nb_"), \(n) as.integer(n > 0), .names = "{str_replace(.col, '^nb_', 'any_')}"))
misdemeanors <- read_csv(file.path(data_dir, "cc_cases_full.csv"), show_col_types = FALSE,
                         col_select = c(case, class, n_charges, n_revocations)) |>
  filter(class == "mis") |>
  transmute(case_group = case, nb_m = as.integer(n_charges - n_revocations))
cases <- cases |>
  left_join(types, by = "case_group", relationship = "one-to-one") |>
  left_join(misdemeanors, by = "case_group", relationship = "one-to-one") |>
  mutate(nb_m = coalesce(nb_m, 0L), any_m = as.integer(nb_m > 0))
cat("cases with felony charges (not revocations):", format(nrow(cases), big.mark = ","), "\n")

# the original's case-level sample: first offense and disposition dates, weekday
cases <- cases |>
  filter(!is.na(first_offdate), !is.na(first_dispositiondate)) |>
  mutate(Y = year(first_dispositiondate), month = month(first_dispositiondate),
         dow = as.integer(format(first_dispositiondate, "%w"))) |>
  filter(!dow %in% c(0, 6))
cat("  with first offense and disposition dates, first disposed on a weekday:", format(nrow(cases), big.mark = ","), "\n")


# ---- judge: modal judge of the case's pleadings ----------------------------------------
read_db <- \(years, file, cols) {
  map(years, \(y) read_csv(file.path(orig_dir, "circuit_data", str_glue("DB_CircuitCriminal_{y}"), file),
                           col_select = all_of(cols), col_types = cols(.default = col_character()),
                           na = character())) |>
    list_rbind()
}
# case_id of each court x case number (one per charge), as the original: 2000, 2010-2019
case_ids <- read_db(c(2000, 2010:2019), "Cases.csv", c("id", "fips", "CaseNumber")) |>
  transmute(case_id = id, fips = as.numeric(fips), casenumber_raw = CaseNumber)
stopifnot(!anyDuplicated(case_ids$case_id), !anyDuplicated(select(case_ids, fips, casenumber_raw)))

pleadings <- charges |>
  semi_join(cases, by = "case_group") |>
  select(case_group, fips, casenumber_raw, dispositiondate) |>
  inner_join(case_ids, by = c("fips", "casenumber_raw"), relationship = "many-to-one")
cat("  charges linked to a case_id:", format(nrow(pleadings), big.mark = ","), "of",
    format(nrow(semi_join(charges, cases, by = "case_group")), big.mark = ","), "\n")
# pleadings 2000, 2010-2020; judge initials cleaned as the original
pleadings <- read_db(c(2000, 2010:2020), "Pleadings.csv", c("case_id", "Filed", "Type", "Judge", "Remarks")) |>
  semi_join(pleadings, by = "case_id") |>
  transmute(case_id, filed = as.Date(Filed, "%Y-%m-%d"), type = Type, remarks = Remarks,
            judge = str_remove_all(if_else(Judge %in% c("XXXX", "3-D", "***"), "", Judge), "[^A-Z]")) |>
  inner_join(pleadings, by = "case_id", relationship = "many-to-one") |>
  mutate(fipscode = if_else(fips %in% c(93, 175, 620, 800), 999, fips))    # circuit 5: one court

# courts: share of pleadings that belong to cases with a judge on some pleading
pleadings <- pleadings |>
  mutate(any_judge = any(judge != ""), .by = case_group) |>
  mutate(pct_filled = mean(any_judge), .by = fipscode)
cat("  courts below 80% of pleadings with judge information (dropped):",
    str_flatten(sort(unique(pleadings$fipscode[pleadings$pct_filled < 0.8])), ", "), "\n")
pleadings <- filter(pleadings, judge != "", pct_filled >= 0.8)
judges <- distinct(pleadings, judge, fipscode) |>
  arrange(judge, fipscode) |>
  mutate(judge_id = row_number())
pleadings <- left_join(pleadings, judges, by = c("judge", "fipscode"), relationship = "many-to-one")
write_csv(judges, file.path(data_dir, "judges.csv"))

# modal judge of each case over a subset of its pleadings (ties: the larger id)
modal_judge <- \(x) x |>
  count(case_group, judge_id) |>
  arrange(case_group, desc(n), desc(judge_id)) |>
  distinct(case_group, .keep_all = TRUE) |>
  select(case_group, judge_id)
order_types <- c("Sentencing Order", "Sentencing Guidelines", "Judgement Order", "Final Order", "Conviction Order",
                 "Dismissal Order", "Dismissal Order With Prejudice", "Dismissal Order Without Prejudice",
                 "Commitment Order")
modal_judge_5 <- list(
  order_type  = filter(pleadings, type %in% order_types),
  remarks     = filter(pleadings, str_detect(remarks, "SENT ORDER|SENTENCING|DISSMISSED|NOLLE PROSEQUI|DISMISSED|FINAL") |
                                    remarks == "SENT"),
  disposition = filter(pleadings, filed == dispositiondate),
  all         = pleadings) |>
  imap(\(x, step) mutate(modal_judge(x), step = step)) |>
  list_rbind() |>
  distinct(case_group, .keep_all = TRUE) |>               # the first step with a judge
  rename(modal_judge_5 = judge_id)
cat("  cases by the pleadings their judge comes from:\n"); print(count(modal_judge_5, step))
modal_judge_5 <- select(modal_judge_5, -step)

cases <- cases |>
  inner_join(modal_judge_5, by = "case_group", relationship = "one-to-one") |>
  mutate(fipscode = if_else(fipscode_raw %in% c(93, 175, 620, 800), 999, fipscode_raw)) |>
  relocate(case_group, person_id, fipscode_raw, fipscode, first_offdate, first_dispositiondate, Y, month, dow,
           modal_judge_5, convict, incar, cond_incar)

write_csv(cases, file.path(data_dir, "judge_iv_cases.csv"))
cat("judge_iv_cases:", format(nrow(cases), big.mark = ","), "cases with a judge,",
    format(n_distinct(cases$modal_judge_5), big.mark = ","), "judges; dismissed / non-carceral / incarcerated:",
    str_flatten(round(c(mean(cases$convict == 0), mean(cases$convict == 1 & cases$incar == 0), mean(cases$incar == 1)), 3),
                " / "), "\n")
