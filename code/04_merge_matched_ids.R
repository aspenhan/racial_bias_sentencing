# ==============================================================================
# 04_merge_matched_ids.R
#
# Adds to the judge-IV cases (03) the prior felony conviction flags from the SC-CC
# match, as Part 12 of Cecile code/replication/01_cleaning/judge_iv/Data_setup_JudgeIV_casenumber_part1only.do,
# and additionally
#   runningvariable  score of the case's own SC sentencing event (below)
#   rv_sc_id         SC record the score comes from
#   rv_offense_group guidelines offense group of that SC event's primary offense (the
#                    worksheet the score is computed on: DRUG, LARCENY, BUR-DWEL, ...)
#   rv_match_stage   1 = exact date match, 2 = dates within the case's range
#   race, sex        the person's modal race and sex over all of their charges in
#                    the full CC record (the original keeps only female and black,
#                    from the current case's charges); female and black are
#                    rebuilt from these, and white is added
#   age             age at the case's first offense: CC birth dates have the
#                    year masked (1004), so the birth year is the modal one on the
#                    person's SC records (fastLink link) and the birth month and
#                    day are the CC ones; missing for persons without SC records
#   sc_prior_<group>_n, _any   number of / any prior convictions (the person's
#                    SC sentencing events more than 30 days before the case's
#                    first offense, as in Part 12), by guidelines offense group
#   cc_prior_<class>_<status>_<type>_n, _any   number of / any prior CC cases,
#                    felony (class fel: the case's felony charges) and misdemeanor
#                    (mis) separately: the person's cases in the full CC record
#                    (cc_cases_full, step 02) with a case date (charged: first
#                    offense, else arrest, hearing, disposition or filing date) or a
#                    first disposition (conv, incar) before the case's first
#                    offense; never the case itself, which can start earlier through
#                    its misdemeanor charges
#   flag_prior_fel_conv, flag_prior_mis_only_conv   the prior-record controls of
#                    the judge IV, between 31 days and 5 years before the case's
#                    first offense (the original's window):
#                    any felony conviction -- a convicted felony CC case (first
#                    disposition; as above) or an SC sentencing event of the person
#                    (SC data from 1995) -- / any convicted misdemeanor CC case but no
#                    felony conviction
#   cc_post<window>_<status>_<type>_any   any later felony CC case (the case's
#                    felony charges that are not revocation proceedings or technical
#                    hearings -- failure to appear, bond, motions, ... -- 02): the person's cases
#                    whose case date is in the window after the case's first
#                    disposition (sentencing), other than the case itself; windows
#                    (recid_windows, 00_master.R; years of 365 days): within 1, 2, 3
#                    and 7 years (1yr, 2yr, 3yr, 7yr), years 2-4 (2to4yr) and
#                    years 5-7 (5to7yr). All windows count up to the end of the CC
#                    record (2019), whatever the follow-up
#                    status: charged (any case), conv (convicted, carceral or
#                    not), incar (incarcerated); type: the offense type of the
#                    charges -- of the convicted / incarcerated charges for conv /
#                    incar -- (13 code-section groups and other,
#                    functions/offense_types.R), or all (any type)
#   cc_post<window>_revother_<status>_<type>_any   (primary revocation measure) any
#                    revocation proceeding of a felony sentence (felony-coded) of the
#                    person (cc_revocations_full, step 02) filed in the window after the
#                    case's first disposition that revokes one of the person's OTHER
#                    sentences, set off after the case's first disposition, not by the
#                    case's conviction (functions/revocation_kinds.R): those of later
#                    sentences (filed under a case first disposed after the day of the
#                    case's first disposition), which only events after it can set off;
#                    and those of earlier sentences (filed under a case first disposed
#                    before the case) or of unidentified ones (revocation-only case
#                    numbers), only if the person has a new case (felony or
#                    misdemeanor, case date) after the case's first disposition and on or
#                    before the filing: the revocation records' own offense and arrest
#                    dates date the proceeding, not the violation, so they cannot tell a
#                    revocation set off by the case's conviction from a later one. The
#                    sentencing judge decides these far less often than the revocations of
#                    the focal hearing's sentences (10: a check on the exclusion
#                    restriction). Status: charged (any proceeding), conv (sentence
#                    revoked), incar (revoked, carceral); type: the offense types of the
#                    convictions of the case it is filed under (else of its own code
#                    section), or all
#   cc_post<window>_revfocal_<status>_<type>_any   (secondary measure) the same, for the
#                    revocations of the sentences imposed at the focal hearing: filed under
#                    the case itself (if convicted: a dismissed case has no sentence; the
#                    log gives the share of dismissed cases with a proceeding filed under
#                    the case, a gauge of how often such a proceeding is not a revocation
#                    of its sentence), or under another case of the person first disposed
#                    on the day of the case's first disposition
#   cc_post<1-3>yr_<revfcase_|revfsame_|revlater_|revearly_|revothnr_><status>_all_any
#                    the parts of the two measures (any type): filed under the case; under
#                    another case sentenced the same day; later sentences; earlier or
#                    unidentified ones after a new case; and other sentences without the
#                    revocation-only case numbers
#   cc_rel<cap>_post<window>_[revother_|revfocal_|<parts>]<status>_all_any   the families
#                    above (any type), within 1, 2 and 3 years after release from the
#                    case's sentence rather than after its first disposition: release =
#                    first disposition + release_share x active carceral time (0 if not
#                    incarcerated), capped at cap years (release_caps and
#                    release_caps_robustness, 00_master.R)
#
# A case is matched to an SC sentencing event of the same person: the SC ids
# linked to the CC person_id through the fastLink new_ID in cc_sc_for_merge), and
# same court: SC circuit (circno) must equal the circuit of the CC fipscode
# (circuit_xwalk_path); a missing circuit on either side does not block a match.
# Dates used: the offense and disposition dates of all the case's (felony) charges, and
# all offense dates (off1date-off10date) and the sentence date of the SC event.
#   Stage 1  Exact: one of the SC case's offense date equals one of the CC case's
#            offense dates, and the SC sentence date equals one of the CC's
#            disposition dates.
#            Tie-breaker for multiple matches: most matched offense dates, then
#            most matched disposition dates (both counted over the case's charges),
#            then the one matching the case's earliest disposition date, then its 2nd
#            earliest, and so on to its last disposition date.
#   Stage 2  Within bounds (cases without a stage-1 match): an SC offense date
#            lies between the CC's first and last offense dates, and the SC
#            sentence date lies between the CC's first and last disposition dates.
#            Tie-breaker for multiple matches: the smallest Euclidean norm of
#            (SC - CC offense date, SC sentencing - CC disposition date), over all
#            pairs of dates.
# Remaining ties are broken at random (seed match_seed). An SC event chosen by the
# cases of several defendants stays with the defendant with the most cases choosing
# it; the others' cases take their next best candidate (several cases of one
# defendant can share an event: one sentencing hearing).
#
# Inputs:  data_dir/judge_iv_cases.csv                   (step 03)
#          data_dir/cc_felony_charges.csv, cc_cases_full.csv, cc_cases_recid.csv, cc_revocations_full.csv   (step 02)
#          data_dir/cc_sc_for_merge.csv
#          orig_dir/final_wage_edu_analysis.dta         (SC birth years)
#          orig_dir/Circuit criminal 2000-2019/circuit_2000_2019.csv   (race, sex)
#          circuit_xwalk_path                           (county -> judicial circuit, 00_master.R)
# Output:  data_dir/cc_person_level_data.csv
# ==============================================================================

# the judge-IV cases (03)
judge_iv <- read_csv(file.path(data_dir, "judge_iv_cases.csv"), show_col_types = FALSE)
# release from the case's sentence, for the outcomes measured from release
# (release_cap<cap>): the first disposition plus the time served, release_share of the
# active carceral time (0 if not incarcerated; life and death sentences are infinite),
# capped at each of release_caps years and, for robustness, release_caps_robustness years
# (00_master.R)
all_caps <- c(release_caps, release_caps_robustness)
release <- map(set_names(all_caps, str_c("release_cap", all_caps)), \(cap)
  judge_iv$first_dispositiondate + pmin(round(release_share * judge_iv$carceral_length_inbtw), 365 * cap))
cases <- bind_cols(select(judge_iv, person_id, case_group, fipscode_raw, first_offdate, first_dispositiondate),
                   as_tibble(release))
stopifnot(!anyNA(select(cases, starts_with("release_cap"))))
stopifnot(!anyNA(cases$first_offdate), !anyDuplicated(cases$case_group))

# Judicial circuit of each CC court (county)
circuits <- read_dta(circuit_xwalk_path) |>
  distinct(fipscode_raw = countyfip, cc_circuit = circuit)
stopifnot(!anyDuplicated(circuits$fipscode_raw))

cases <- left_join(cases, circuits, by = "fipscode_raw", relationship = "many-to-one")

# the cases' (felony) charges
charges <- read_csv(file.path(data_dir, "cc_felony_charges.csv"), col_types = cols(dob = col_date()),
                    col_select = c(case_group = case, offensedate, dispositiondate, dob)) |>
  semi_join(cases, by = "case_group")
stopifnot(all(cases$case_group %in% charges$case_group))

# Matched CC person-SC sentencing event data
sc <- read_csv(file.path(data_dir, "cc_sc_for_merge.csv"), guess_max = Inf, show_col_types = FALSE)


# ---- Flag prior conviction/incarceration --------------------------
# the SC variables of the original match, i.e. the rows the original used (SC
# events with a sentence date)
matched <- sc |>
  filter(!is.na(sc_sentdate)) |>
  select(person_id, new_ID, sc_sentdate, sc_offensedate, sc_off1date, sc_priorcon_a, sc_prior_incar) |>
  distinct()

# one row per case x SC sentencing event of the person (NA if none);
# priors are SC sentences more than 30 days before the case's first offense
events <- cases |>
  select(person_id, case_group, first_offdate) |>
  left_join(matched, by = "person_id", relationship = "many-to-many") |>
  mutate(gap       = as.numeric(first_offdate - sc_sentdate),
         before    = coalesce(sc_sentdate < first_offdate, FALSE),
         prior     = before & gap > 30,
         prior_5yr = prior & gap <= 1825)

flags <- events |>
  summarise(flag_no_match             = as.integer(all(is.na(new_ID))),
            flag_prior_conviction     = as.integer(any(prior)),
            flag_prior_conviction_5yr = as.integer(any(prior_5yr)),
            flag_prior_incar          = as.integer(any(before & sc_prior_incar %in% 1)),
            flag_prior_sc_convic      = as.integer(any(before & sc_priorcon_a %in% 1)),
            num_prior_convic          = sum(prior_5yr),
            .by = c(case_group, first_offdate)) |>
  left_join(events |>
              filter(prior_5yr) |>
              summarise(max_sent_date = max(sc_sentdate), .by = case_group),
            by = "case_group") |>
  mutate(time_since_prior = coalesce(as.numeric(first_offdate - max_sent_date), 0)) |>
  select(-first_offdate)


# ---- Prior record by offense type ----------------------------------------------
# counts by case and type (case_group, type, n) -> <prefix><type>_n and _any
prior_wide <- \(x, prefix) x |>
  pivot_wider(names_from = type, values_from = n, values_fill = 0, names_glue = str_c(prefix, "{type}_n"),
              names_expand = TRUE) |>
  mutate(across(-case_group, \(n) as.integer(n > 0), .names = "{str_remove(.col, '_n$')}_any"))

# prior convictions: SC sentencing events more than 30 days before the case's
# first offense, by the offense group of their primary offense
sc_prior <- cases |>
  select(case_group, person_id, first_offdate) |>
  inner_join(distinct(sc, person_id, sc_id, sc_sentdate, sc_offenses), by = "person_id",
             relationship = "many-to-many") |>
  filter(as.numeric(first_offdate - sc_sentdate) > 30) |>
  mutate(type = str_to_lower(str_replace_all(coalesce(sc_offenses, "unknown"), "[^A-Za-z]", "_"))) |>
  count(case_group, type) |>
  prior_wide("sc_prior_")

# ---- Prior and later CC cases (full record, step 02) ---------------------------------
# the persons' CC cases, one row per case x class (fel, mis: the case's felony /
# misdemeanor charges) x status (charged, conv, incar) x offense type it has, plus
# type "all" for any type: all cases (cc_cases_full: priors), and the new felony
# cases of the recidivism outcomes, without revocation proceedings and technical
# hearings (cc_cases_recid)
case_status <- \(file) read_csv(file.path(data_dir, file), show_col_types = FALSE,
                                col_select = c(case, person_id, class, outcome, case_date, first_dispositiondate,
                                               starts_with(c("charged_", "conv_", "incar_")))) |>
  semi_join(cases, by = "person_id") |>
  mutate(charged_all = 1L,
         conv_all    = as.integer(outcome != "dismissal"),
         incar_all   = as.integer(outcome == "carceral")) |>
  pivot_longer(starts_with(c("charged_", "conv_", "incar_")),
               names_to = c("status", "type"), names_pattern = "^(charged|conv|incar)_(.+)$") |>
  filter(value == 1) |>
  mutate(class  = factor(class, levels = c("fel", "mis")),
         status = factor(status, levels = c("charged", "conv", "incar")),
         type   = factor(type, levels = c(offense_type_names, "all")))
cc_status <- case_status("cc_cases_full.csv")

# the case itself (case_group is its full-record case id) is never its own prior or
# later case
drop_own <- \(x) filter(x, case != case_group)

# prior, felonies and misdemeanors separately: charged cases with a case date (first
# offense, else arrest, hearing, disposition or filing), convicted / incarcerated
# cases with a first disposition, before the case's first offense
cc_prior <- cc_status |>
  mutate(prior_date = if_else(status == "charged", case_date, first_dispositiondate)) |>
  inner_join(select(cases, case_group, person_id, focal_offdate = first_offdate),
             join_by(person_id, prior_date < focal_offdate), relationship = "many-to-many") |>
  drop_own() |>
  count(case_group, class, status, type) |>
  pivot_wider(names_from = c(class, status, type), values_from = n, values_fill = 0, names_expand = TRUE,
              names_glue = "cc_prior_{class}_{status}_{type}_n") |>
  mutate(across(-case_group, \(n) as.integer(n > 0), .names = "{str_remove(.col, '_n$')}_any"))

# prior-record controls of the judge IV (00_master.R): convictions between 31 days and
# 5 years (1,825 days) before the case's first offense, the window of the original's
# prior-conviction flag, fixed so that the flags are comparable across years. Felony:
# a convicted felony CC case (first disposition), or an SC sentencing event of the
# person (SC records, all felony sentencing, start in 1995, the CC record only in
# 2000, so the window is complete from 2000 on); misdemeanor only: a convicted
# misdemeanor CC case and no felony conviction
prior_in_window <- \(x, date) x |>
  rename(prior_date = all_of(date)) |>
  inner_join(select(cases, case_group, person_id, focal_offdate = first_offdate),
             join_by(person_id, prior_date < focal_offdate), relationship = "many-to-many") |>
  filter(between(as.numeric(focal_offdate - prior_date), 31, 1825))
prior_conv_5yr <- bind_rows(
  cc_status |>
    filter(status == "conv", type == "all", !is.na(first_dispositiondate)) |>
    prior_in_window("first_dispositiondate") |>
    drop_own() |>
    transmute(case_group, class = as.character(class), source = "cc"),
  # the case's own SC event is sentenced after its first offense, so it never enters
  sc |>
    filter(!is.na(sc_sentdate)) |>
    distinct(person_id, sc_sentdate) |>
    prior_in_window("sc_sentdate") |>
    transmute(case_group, class = "fel", source = "sc")) |>
  summarise(flag_prior_fel_conv      = as.integer(any(class == "fel")),
            flag_prior_mis_only_conv = as.integer(!any(class == "fel")),
            fel_cc = any(class == "fel" & source == "cc"), fel_sc = any(source == "sc"), .by = case_group)

# later: events (person_id, date, status, type) in each window of `windows` (default
# recid_windows, 00_master.R: after year `from`, up to year `to` after the case's
# first disposition -- or after the date in column `start` of cases, e.g. the release
# from the case's sentence --, years of 365 days) -> <stem><window>_<prefix><status>_<type>_any
# (stem cc_post); keep: filter of the (case_group, event) pairs, which carry the
# case's first disposition (focal_dispdate)
post_windows <- \(events, prefix, keep = identity, start = "first_dispositiondate", windows = recid_windows,
                  stem = "cc_post") events |>
  inner_join(transmute(cases, case_group, person_id, focal_dispdate = first_dispositiondate, start = .data[[start]]),
             join_by(person_id, date > start), relationship = "many-to-many") |>
  keep() |>
  mutate(days = as.numeric(date - start)) |>
  filter(days <= 365 * max(windows$to)) |>
  cross_join(select(windows, window, from, to)) |>
  filter(days > 365 * from, days <= 365 * to) |>
  distinct(case_group, window, status, type) |>
  mutate(window = factor(window, levels = windows$window), value = 1L) |>
  pivot_wider(names_from = c(window, status, type), values_from = value, values_fill = 0L, names_expand = TRUE,
              names_glue = str_c(stem, "{window}_", prefix, "{status}_{type}_any"))

# later felony cases (the case's felony charges that are not revocation proceedings
# or technical hearings, 02), dated by their case date (first offense, else arrest,
# hearing, disposition or filing), other than the case itself
recid_events <- case_status("cc_cases_recid.csv") |>
  rename(date = case_date)
cc_post <- post_windows(recid_events, "", keep = drop_own)

# later revocation proceedings of felony sentences (the proceeding's own charge type:
# a felony-coded revocation filed under a case with convictions of one class only is
# under a felony case 99% of the time), dated by filing: charged (any), conv
# (revoked), incar (revoked, carceral); typed by the convictions of the case they are
# filed under, else by their own code section. Revocations of the case's own sentence
# (revfocal_), and of the person's other sentences set off after it (revother_)
case_conv_types <- cc_status |>
  filter(status == "conv", type != "all") |>
  distinct(case, type) |>
  mutate(type = as.character(type))
rev_events <- read_csv(file.path(data_dir, "cc_revocations_full.csv"), show_col_types = FALSE,
                       col_select = c(case, person_id, filed, class, revoked, incarcerated, own_type)) |>
  filter(class == "fel") |>
  semi_join(cases, by = "person_id") |>
  mutate(charged = 1L, conv = revoked, incar = incarcerated) |>
  left_join(case_conv_types, by = "case", relationship = "many-to-many") |>
  mutate(type = coalesce(type, own_type)) |>
  pivot_longer(c(charged, conv, incar), names_to = "status") |>
  filter(value == 1) |>
  select(person_id, case, date = filed, status, type)
rev_events <- bind_rows(rev_events, mutate(rev_events, type = "all")) |>
  distinct() |>
  mutate(status = factor(status, levels = c("charged", "conv", "incar")),
         type   = factor(type, levels = c(offense_type_names, "all")))

# what each (judge-IV case, revocation proceeding) pair of post_windows revokes, and
# whether the person has a new case before the filing (functions/revocation_kinds.R)
person_cases <- person_cases_of(unique(cases$person_id))
convicted_cases <- judge_iv$case_group[judge_iv$convict == 1]
revoked_sentence <- \(x) revocation_kind(x, person_cases, convicted_cases)
# revocations of the sentences imposed at the focal hearing (revfocal_: filed under the
# case, or under another case first disposed the same day); of the person's other
# sentences set off after the case's first disposition (revother_): later ones, and
# earlier or unidentified ones only after a new charge
keep_kinds <- \(kinds, new_charge_kinds = character()) \(x) revoked_sentence(x) |>
  filter(revokes %in% kinds | (revokes %in% new_charge_kinds & new_charge))
keep_revfocal <- keep_kinds(c("focal", "same_day"))
keep_revother <- keep_kinds("later", c("earlier", "unknown"))
cc_post_revfocal <- post_windows(rev_events, "revfocal_", keep = keep_revfocal)
cc_post_revother <- post_windows(rev_events, "revother_", keep = keep_revother)
# their parts, for the variants of the 2SLS (07; any type, windows of 1-3 years):
# filed under the case only (revfcase_), under another case sentenced the same day
# (revfsame_), later sentences (revlater_), earlier or unidentified ones after a new
# charge (revearly_); and other sentences without revocation-only case numbers
# (revothnr_: later ones, and earlier ones after a new charge)
rev_parts <- list(revfcase_ = keep_kinds("focal"), revfsame_ = keep_kinds("same_day"),
                  revlater_ = keep_kinds("later"), revearly_ = keep_kinds(character(), c("earlier", "unknown")),
                  revothnr_ = keep_kinds("later", "earlier"))

# The same three outcome families measured from release (within 1, 2 and 3 years after
# it), any type only, for each cap on time served (release_cap<cap>, above):
# cc_rel<cap>_post<window>_[revother_|revfocal_]<status>_all_any
release_windows <- filter(recid_windows, window %in% c("1yr", "2yr", "3yr"))
all_types <- \(x) x |> filter(type == "all") |> mutate(type = factor(type, levels = "all"))
join_all <- \(xs) reduce(xs, \(x, y) full_join(x, y, by = "case_group", relationship = "one-to-one"))
cc_rel <- map(all_caps, \(cap) {
  from_release <- \(events, prefix, keep) post_windows(all_types(events), prefix, keep = keep,
                                                       start = str_c("release_cap", cap), windows = release_windows,
                                                       stem = str_c("cc_rel", cap, "_post"))
  c(list(from_release(recid_events, "", drop_own), from_release(rev_events, "revfocal_", keep_revfocal),
         from_release(rev_events, "revother_", keep_revother)),
    imap(rev_parts, \(keep, prefix) from_release(rev_events, prefix, keep))) |>
    join_all()
}) |>
  join_all()
# the parts of the revocation measures from the disposition (any type, 1-3 years)
cc_post_rev_parts <- imap(rev_parts, \(keep, prefix) post_windows(all_types(rev_events), prefix, keep = keep,
                                                                  windows = release_windows)) |>
  join_all()
cat("felony revocation proceedings filed within 3 years after a judge-IV case's first disposition (case x",
    "proceeding pairs): what they revoke, and whether the person has a new case charged after the case's first",
    "disposition and on or before the filing:\n")
rev_events |>
  filter(status == "charged", type == "all") |>
  distinct(person_id, case, date) |>
  inner_join(select(cases, case_group, person_id, focal_dispdate = first_dispositiondate),
             join_by(person_id, date > focal_dispdate), relationship = "many-to-many") |>
  filter(date <= focal_dispdate + 3 * 365) |>
  revoked_sentence() |>
  count(revokes, new_charge) |>
  mutate(share = round(n / sum(n), 3)) |>
  print()

# Dismissed cases have no sentence to revoke, so proceedings filed under them do not
# count. Checks (cases first disposed in or before 2016: full follow-up for 3 years): the
# shares of cases with a revocation of the focal sentences / of the
# person's other sentences set off after the case's first disposition within 1 and 3 years, by
# treatment -- the incarcerated for 3 years or more are in custody, so their own
# sentence can hardly be revoked within 1 year; and the share of dismissed cases with a
# proceeding filed under the case itself within 3 years, a gauge of how often a
# proceeding filed under a case does not revoke its sentence
followed <- judge_iv |>
  filter(Y <= full_follow_up_year(c("1yr", "2yr", "3yr"))) |>
  transmute(case_group, person_id, focal_dispdate = first_dispositiondate,
            treatment = case_when(convict == 0 ~ "dismissed", incar == 0 ~ "non-carceral",
                                  carceral_length_inbtw < 3 * 365 ~ "incarcerated < 3 years",
                                  .default = "incarcerated >= 3 years"))
cat("share of cases with a revocation proceeding of the focal sentences (revfocal) / of other sentences set off",
    "after the case's first disposition (revother), within 1 and 3 years, by treatment (cases first disposed in or before",
    "2016):\n")
followed |>
  left_join(select(cc_post_revfocal, case_group, matches("^cc_post[13]yr_revfocal_charged_all_any$")),
            by = "case_group", relationship = "one-to-one") |>
  left_join(select(cc_post_revother, case_group, matches("^cc_post[13]yr_revother_charged_all_any$")),
            by = "case_group", relationship = "one-to-one") |>
  summarise(across(starts_with("cc_post"), \(x) round(mean(coalesce(x, 0L)), 4)), cases = n(), .by = treatment) |>
  rename_with(\(x) str_remove_all(x, "^cc_post|_charged_all_any$")) |>
  print(width = Inf)
cat("dismissed cases with a felony revocation proceeding filed under the case within 3 years:",
    round(mean(filter(followed, treatment == "dismissed")$case_group %in%
                 (rev_events |>
                    filter(status == "charged", type == "all") |>
                    inner_join(filter(followed, treatment == "dismissed"),
                               join_by(person_id, case == case_group, date > focal_dispdate)) |>
                    filter(date <= focal_dispdate + 3 * 365) |>
                    pull(case))), 4), "\n")
cat("outcomes within 1 and 3 years after release (cap on time served:", str_flatten(release_caps, " / "), "years;",
    "cases first disposed by", str_flatten(cc_last_year - 3 - release_caps, " / "), "-- full follow-up), share of",
    "cases by treatment: new felony case charged, revocation proceeding of the focal sentences:\n")
map(release_caps, \(cap) judge_iv |>
      filter(Y <= cc_last_year - 3 - cap) |>
      transmute(case_group, treatment = case_when(convict == 0 ~ "dismissed", incar == 0 ~ "non-carceral",
                                                  carceral_length_inbtw < 3 * 365 ~ "incarcerated < 3 years",
                                                  .default = "incarcerated >= 3 years")) |>
      left_join(select(cc_rel, case_group, matches(str_c("^cc_rel", cap, "_post[13]yr_(revfocal_)?charged_all_any$"))),
                by = "case_group", relationship = "one-to-one") |>
      summarise(across(starts_with("cc_rel"), \(x) round(mean(coalesce(x, 0L)), 4)), cases = n(), .by = treatment) |>
      rename_with(\(x) str_remove_all(x, "^cc_rel[0-9]_post|_all_any$")) |>
      mutate(cap = cap, .before = 1)) |>
  list_rbind() |>
  print(width = Inf)


# ---- Identify the case's own SC sentencing event, and its runningvariable ------------
# SC events of the person (one sc_id can belong to several CC person_ids of the
# same fastLink person) with their sentencing date, and their offense dates;
# circuit and score of each SC event
sc_events <- distinct(sc, person_id, sc_id, sc_sentdate)
sc_off <- sc |>
  select(person_id, sc_id, num_range("sc_off", 1:10, "date")) |>
  pivot_longer(-c(person_id, sc_id), values_to = "date", values_drop_na = TRUE) |>
  distinct(person_id, sc_id, date)
sc_info <- distinct(sc, sc_id, sc_circno, sc_runningvariable)
stopifnot(!anyDuplicated(sc_info$sc_id))

# same court unless both circuits are known and differ
court_ok <- \(cc_circuit, sc_circno) is.na(cc_circuit) | is.na(sc_circno) | cc_circuit == sc_circno

# offense and disposition dates of the case's charges (one row per charge); the
# disposition dates with their rank among the case's distinct ones (m = 1 for the
# earliest)
case_off <- charges |>
  filter(!is.na(offensedate)) |>
  select(case_group, date = offensedate) |>
  inner_join(select(cases, case_group, person_id, cc_circuit), by = "case_group", relationship = "many-to-one")
case_disp <- charges |>
  filter(!is.na(dispositiondate)) |>
  select(case_group, date = dispositiondate) |>
  mutate(m = dense_rank(date), .by = case_group) |>
  inner_join(select(cases, case_group, person_id, cc_circuit), by = "case_group", relationship = "many-to-one")

set.seed(match_seed)

# stage 1: exact matches of an offense date and of the sentencing date; matches
# are counted over the case's charges
off_hits <- inner_join(case_off, sc_off, by = c("person_id", "date"), relationship = "many-to-many")
disp_hits <- inner_join(case_disp, sc_events, by = c("person_id", "date" = "sc_sentdate"),
                        relationship = "many-to-many")
matched_m <- disp_hits |>                    # the case's matched disposition dates, in order
  distinct(case_group, sc_id, m) |>
  arrange(case_group, sc_id, m) |>
  mutate(i = row_number(), .by = c(case_group, sc_id)) |>
  pivot_wider(names_from = i, values_from = m, names_prefix = "m_", names_sort = TRUE)
stage1 <- inner_join(
  summarise(off_hits, n_off = n(), .by = c(case_group, sc_id, cc_circuit)),
  summarise(disp_hits, n_disp = n(), .by = c(case_group, sc_id)),
  by = c("case_group", "sc_id"), relationship = "one-to-one") |>
  left_join(sc_info, by = "sc_id", relationship = "many-to-one")
cat("exact matches rejected for a different circuit:",
    format(sum(!court_ok(stage1$cc_circuit, stage1$sc_circno)), big.mark = ","), "\n")
stage1 <- stage1 |>
  filter(court_ok(cc_circuit, sc_circno)) |>
  left_join(matched_m, by = c("case_group", "sc_id"), relationship = "one-to-one") |>
  arrange(case_group, sc_id) |>
  mutate(u = runif(n())) |>
  # most matched offense dates, then disposition dates, then matching the case's
  # earliest disposition date, then its 2nd earliest, ... (m_1 < m_2 < ...), then at
  # random
  arrange(case_group, desc(n_off), desc(n_disp), pick(starts_with("m_")), u) |>
  mutate(rule_rank = consecutive_id(n_off, n_disp, pick(starts_with("m_"))), .by = case_group)

# stage 2: an offense date and the sentencing date within the case's date ranges
case_range <- inner_join(
  summarise(case_off, off_first = min(date), off_last = max(date), .by = c(case_group, person_id, cc_circuit)),
  summarise(case_disp, disp_first = min(date), disp_last = max(date), .by = case_group),
  by = "case_group", relationship = "one-to-one")
stage2 <- case_range |>
  anti_join(stage1, by = "case_group") |>
  inner_join(sc_events, join_by(person_id, disp_first <= sc_sentdate, disp_last >= sc_sentdate)) |>
  semi_join(sc_off, join_by(person_id, sc_id, off_first <= date, off_last >= date)) |>
  left_join(sc_info, by = "sc_id", relationship = "many-to-one") |>
  filter(court_ok(cc_circuit, sc_circno))
# The norm sqrt(d_off^2 + d_disp^2) over all (SC offense date, CC offense date)
# and (SC sentencing date, CC disposition date) pairs, in days, is smallest for
# the smallest d_off and the smallest d_disp, as the two vary independently
off_dist <- stage2 |>
  distinct(case_group, sc_id, person_id) |>
  inner_join(rename(sc_off, sc_date = date), by = c("person_id", "sc_id"), relationship = "many-to-many") |>
  inner_join(distinct(case_off, case_group, cc_date = date), by = "case_group", relationship = "many-to-many") |>
  summarise(d_off = min(abs(as.numeric(sc_date - cc_date))), .by = c(case_group, sc_id))
disp_dist <- stage2 |>
  distinct(case_group, sc_id, sc_sentdate) |>
  inner_join(distinct(case_disp, case_group, cc_date = date), by = "case_group", relationship = "many-to-many") |>
  summarise(d_disp = min(abs(as.numeric(sc_sentdate - cc_date))), .by = c(case_group, sc_id))
stage2 <- stage2 |>
  distinct(case_group, sc_id, sc_runningvariable) |>
  left_join(off_dist, by = c("case_group", "sc_id"), relationship = "one-to-one") |>
  left_join(disp_dist, by = c("case_group", "sc_id"), relationship = "one-to-one") |>
  mutate(dist = sqrt(d_off^2 + d_disp^2)) |>
  arrange(case_group, sc_id) |>
  mutate(u = runif(n())) |>
  arrange(case_group, dist, u) |>                 # smallest norm, then at random
  mutate(rule_rank = consecutive_id(dist), .by = case_group)

candidates <- bind_rows(mutate(stage1, rv_match_stage = 1), mutate(stage2, rv_match_stage = 2)) |>
  select(-any_of("person_id")) |>
  left_join(select(cases, case_group, person_id), by = "case_group", relationship = "many-to-one")

# An SC event chosen by the cases of several defendants (CC person_ids) stays with the
# defendant with the most cases choosing it (ties at random); the other defendants'
# cases drop it from their candidates and take their next best one -- repeated until
# no event is chosen by cases of different defendants. An event chosen by several
# cases of one defendant (one sentencing hearing for several of the person's cases)
# stays with all of them
repeat {
  shared <- candidates |>
    distinct(case_group, .keep_all = TRUE) |>                       # each case's best candidate
    count(sc_id, person_id) |>
    filter(n() > 1, .by = sc_id)
  if (!nrow(shared)) break
  owners <- shared |>
    mutate(u = runif(n())) |>
    arrange(sc_id, desc(n), u) |>
    distinct(sc_id, .keep_all = TRUE) |>
    select(sc_id, owner = person_id)
  cat("SC events chosen by the cases of several defendants:", n_distinct(shared$sc_id), "| cases giving them up:",
      sum(shared$n) - sum(semi_join(shared, owners, by = c("sc_id", "person_id" = "owner"))$n), "\n")
  candidates <- candidates |>
    left_join(owners, by = "sc_id", relationship = "many-to-one") |>
    filter(is.na(owner) | person_id == owner) |>
    select(-owner)
}
rv <- candidates |>
  distinct(case_group, .keep_all = TRUE) |>
  select(case_group, runningvariable = sc_runningvariable, rv_sc_id = sc_id, rv_match_stage) |>
  left_join(distinct(sc, rv_sc_id = sc_id, rv_offense_group = sc_offenses), by = "rv_sc_id", relationship = "many-to-one")

cat("cases matched to an SC event:", format(nrow(rv), big.mark = ","),
    "| stage 1:", format(sum(rv$rv_match_stage == 1), big.mark = ","),
    "| stage 2:", format(sum(rv$rv_match_stage == 2), big.mark = ","), "\n")
candidates |>
  filter(rule_rank == 1) |>
  summarise(n_tied = n(), scores_differ = n_distinct(sc_runningvariable) > 1,
            stage = first(rv_match_stage), .by = case_group) |>
  summarise(cases = n(), tied_after_rule = sum(n_tied > 1),
            tied_with_different_scores = sum(n_tied > 1 & scores_differ), .by = stage) |>
  print()
cat("SC events matched to more than one case (all of one defendant):",
    format(sum(count(rv, rv_sc_id)$n > 1), big.mark = ","), "\n")


# ---- Race and sex, standardized within person --------------------------------
# A person's race and sex are the modal values over all of the person's charges in
# the full CC record (all charge types), race codings harmonized; "Other or unknown"
# race is used only if no other race is recorded; ties go to the value on the
# person's most recent charge (functions/person_demog.R). female, black and white
# are rebuilt from them.
demog <- cc_charge_demog(file.path(orig_dir, "Circuit criminal 2000-2019", "circuit_2000_2019.csv")) |>
  semi_join(cases, by = "person_id") |>
  person_demog()
demog |>
  summarise(persons = n(), with_differing_values = sum(n_values > 1), modal_tie = sum(tied), .by = var) |>
  print()
demog <- demog |>
  pivot_wider(id_cols = person_id, names_from = var, values_from = value) |>
  mutate(female = as.integer(sex %in% "Female"),
         black  = as.integer(race %in% "Black"),
         white  = as.integer(race %in% "White"))


# ---- Age at the case's first offense --------------------------------------------
# birth year: the modal one on the person's SC records (ties: the earlier year);
# birth month and day: the CC ones (their year is masked)
sc_birthyr <- read_dta(file.path(orig_dir, "final_wage_edu_analysis.dta"), col_select = c(id, birthyr)) |>
  filter(!is.na(birthyr)) |>
  inner_join(distinct(sc, person_id, sc_id), by = c("id" = "sc_id"), relationship = "one-to-many") |>
  count(person_id, birthyr) |>
  arrange(person_id, desc(n), birthyr) |>
  distinct(person_id, .keep_all = TRUE)
age <- charges |>
  filter(!is.na(dob)) |>
  distinct(case_group, .keep_all = TRUE) |>
  select(case_group, dob) |>
  right_join(select(cases, case_group, person_id, first_offdate), by = "case_group", relationship = "one-to-one") |>
  left_join(select(sc_birthyr, person_id, birthyr), by = "person_id", relationship = "many-to-one") |>
  mutate(age = year(first_offdate) - birthyr -
           coalesce(format(first_offdate, "%m%d") < format(dob, "%m%d"), FALSE),   # birthday not yet reached
         age = if_else(between(age, 10, 100), age, NA)) |>
  select(case_group, age)
cat("cases with an age:", round(mean(!is.na(age$age)), 3), "\n")


# ---- Merge info onto the judge-IV cases --------------------------------------------
stopifnot(setequal(judge_iv$case_group, flags$case_group), all(judge_iv$person_id %in% demog$person_id))
person_level <- judge_iv |>
  left_join(flags, by = "case_group", relationship = "one-to-one") |>
  left_join(rv, by = "case_group", relationship = "one-to-one") |>
  left_join(demog, by = "person_id", relationship = "many-to-one") |>
  left_join(age, by = "case_group", relationship = "one-to-one") |>
  left_join(sc_prior, by = "case_group", relationship = "one-to-one") |>
  left_join(cc_prior, by = "case_group", relationship = "one-to-one") |>
  left_join(select(prior_conv_5yr, -fel_cc, -fel_sc), by = "case_group", relationship = "one-to-one") |>
  left_join(cc_post, by = "case_group", relationship = "one-to-one") |>
  left_join(cc_post_revother, by = "case_group", relationship = "one-to-one") |>
  left_join(cc_post_revfocal, by = "case_group", relationship = "one-to-one") |>
  left_join(cc_post_rev_parts, by = "case_group", relationship = "one-to-one") |>
  left_join(cc_rel, by = "case_group", relationship = "one-to-one") |>
  mutate(across(c(starts_with(c("sc_prior_", "cc_prior_", "cc_post", "cc_rel")), flag_prior_fel_conv,
                  flag_prior_mis_only_conv),
                \(x) coalesce(x, 0L)))

write_csv(person_level, file.path(data_dir, "cc_person_level_data.csv"))
cat("cc_person_level_data:", format(nrow(person_level), big.mark = ","), "cases;",
    format(sum(!is.na(person_level$runningvariable)), big.mark = ","), "with runningvariable\n")
cat("share of cases with any prior SC conviction:", round(mean(person_level$flag_prior_conviction), 3), "\n")
cat("share of cases with a conviction 31 days-5 years before, by year: felony (CC or SC; CC; SC), misdemeanor only (CC):\n")
person_level |>
  left_join(select(prior_conv_5yr, case_group, fel_cc, fel_sc), by = "case_group", relationship = "one-to-one") |>
  summarise(fel = mean(flag_prior_fel_conv), fel_cc = mean(fel_cc %in% TRUE), fel_sc = mean(fel_sc %in% TRUE),
            mis_only = mean(flag_prior_mis_only_conv), .by = Y) |>
  arrange(Y) |>
  print(n = Inf, digits = 3)
cat("share of cases with any CC case / revocation (charged / convicted or revoked / incarcerated):\n")
person_level |>
  summarise(across(matches("^cc_(prior_(fel|mis)|post[0-9to]+yr(_revother|_revfocal)?)_(charged|conv|incar)_all_any$"),
                   \(x) round(mean(x), 3))) |>
  pivot_longer(everything(), names_to = c("when", "status"), names_pattern = "^cc_(.+)_(charged|conv|incar)_all_any$") |>
  pivot_wider(names_from = status) |>
  print()
