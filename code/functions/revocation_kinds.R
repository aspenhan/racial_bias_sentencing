# ==============================================================================
# revocation_kinds.R
#
# Which sentence a revocation proceeding revokes, relative to a judge-IV case of the
# same defendant (04: the revocation outcomes; 10: who decides them).
#
# person_cases_of(persons)
#   the defendants' cases with charges other than revocation proceedings (cc_cases_full,
#   02, counts the revocations with no earlier conviction in the data as convictions:
#   n_revocations), with when they were sentenced (first disposition) and charged (case
#   date: first offense, else arrest, hearing, disposition or filing); persons: the
#   person_ids to keep. A revocation filed under a case number not among them (revocation
#   proceedings only) revokes an unidentified sentence.
#
# revocation_kind(pairs, person_cases, convicted_cases)
#   pairs: (case_group, person_id, case, date, focal_dispdate), a judge-IV case and a
#   proceeding of its defendant filed (date) under case `case` after the judge-IV case's
#   first disposition (focal_dispdate); convicted_cases: the convicted judge-IV cases.
#   Adds
#     revokes     focal      filed under the case (convicted)
#                 same_day   filed under another case first disposed on the case's first
#                            disposition date: sentenced at the same hearing
#                 later      filed under a case first disposed after it
#                 earlier    filed under a case first disposed before it
#                 unknown    filed under a revocation-only case number: the sentence it
#                            revokes is unidentified
#                 dismissed  filed under the case, which was dismissed: no sentence
#     new_charge  the defendant has another case charged after the case's first
#                 disposition and on or before the filing: what can set off the revocation
#                 of an earlier or unidentified sentence other than the case's conviction
#   The revocation measures (04): focal sentences = focal and same_day;
#   other sentences = later, and earlier or unknown with new_charge.
# ==============================================================================

person_cases_of <- \(persons) {
  read_csv(file.path(data_dir, "cc_cases_full.csv"), show_col_types = FALSE,
           col_select = c(case, person_id, n_charges, n_revocations, case_date, first_dispositiondate)) |>
    filter(person_id %in% persons, n_charges > n_revocations) |>
    summarise(person_id = first(person_id), sentenced = min(first_dispositiondate), charged = min(case_date), .by = case)
}

revocation_kind <- \(pairs, person_cases, convicted_cases) {
  keys <- distinct(pairs, case_group, person_id, case, date, focal_dispdate)
  # the defendant's other cases charged after the case's first disposition and on or
  # before the filing
  new_charge <- keys |>
    inner_join(select(person_cases, person_id, ev_case = case, ev_date = charged),
               join_by(person_id, focal_dispdate < ev_date, date >= ev_date), relationship = "many-to-many") |>
    filter(ev_case != case_group) |>
    distinct(case_group, case, date) |>
    mutate(new_charge = TRUE)
  pairs |>
    left_join(select(person_cases, case, sentenced), by = "case", relationship = "many-to-one") |>
    left_join(new_charge, by = c("case_group", "case", "date"), relationship = "many-to-one") |>
    mutate(new_charge = coalesce(new_charge, FALSE),
           revokes = case_when(case == case_group & case_group %in% convicted_cases ~ "focal",
                               case == case_group ~ "dismissed",
                               is.na(sentenced) ~ "unknown",
                               sentenced == focal_dispdate ~ "same_day",
                               sentenced > focal_dispdate ~ "later",
                               .default = "earlier"))
}
