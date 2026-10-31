# ==============================================================================
# 02_cc_cases_full.R
#
# Case-level record of all circuit-court (CC) cases filed 2000-2019, the source
# of the prior-record and recidivism variables cc_prior_*, cc_post* (04). Cases are built from the full
# charge record with the case_group procedure of the judge-IV data
# (functions/cc_case_groups.R), but without that data's sample restrictions:
# misdemeanors, appeals from district court, cases outside the judge sample, ...
# are all kept.
#
# Charges: felonies and misdemeanors (infractions, civil and "other" -- animal
# violations, bond appeals -- are left out).
#   convicted     disposition "Guilty"; or "Appeal Withdrawn", "Resolved" or no
#                 disposition, with any penalty recorded: jail/penitentiary, life/death,
#                 program, probation, license or driving fields filled in, or
#                 sentence, fine, restitution, costs, probation or license
#                 suspension time above zero
#   incarcerated  convicted, and jail or penitentiary with sentence time minus
#                 suspended time above zero, or a life or death sentence (these
#                 mostly have no sentence time); "Time Served" is not carceral
# Dates: offense, arrest, hearing and disposition dates after 2019 are errors,
# set to missing.
# Revocation proceedings -- disposition "Sentence/Probation Revoked" (whatever the
# code section); or a code section listed under "Supervision violation" in the
# Virginia Crime Codes (data_dir/vcc_2026_active_retired.xlsx), other than
# pretrial violations: 19.2-306 (suspended sentence, good behavior, community
# diversion, probation), 19.2-303.3 (community-based probation), 19.2-303.5
# (immediate sanction program), 16.1-291 (juvenile and domestic), 19.2-315 and
# 53.1-137 (parole), 19.2-295.2(B) and 19.2-295.2:1 (post-release), 18.2-271.1
# (alcohol safety program); or a violation charge text under another code
# section: probation / suspended sentence / good behavior / parole violations,
# show cause, capias, contempt, court orders, escape (not pretrial -- bond, bail,
# failure to appear -- or driving on a revoked license) -- act on the sentence
# of an earlier conviction and are not new convictions. A revocation (revoked, or
# convicted as above as a revocation proceeding) is dropped if the person has a
# conviction disposed on or before it (its disposition date, else its hearing,
# arrest, offense or filing date) in the data (whatever the case; the case
# numbering mostly files it under that conviction's case); otherwise the conviction
# predates the data and the revocation counts as a conviction, carceral by the
# rule above. Other revocation proceedings (e.g. dismissed) are dropped.
# The record is kept separately for the case's felony and misdemeanor charges
# (priors count both, separately; recidivism counts felonies): within a class, a
# case is convicted / incarcerated if any of its charges of the class is; a case
# that is not convicted is dismissed.
#
# One row per case x class:
#   case, person_id, fips, class (fel, mis), n_charges, n_revocations (of them,
#                      revocation proceedings counted as convictions, below)
#   outcome            carceral (incarcerated), noncarceral (convicted, not
#                      incarcerated) or dismissal (not convicted)
#   first_offdate, last_offdate   earliest / latest offense date over the case's
#                      charges of the class (latest: not revocation proceedings)
#   case_date          first_offdate, else the earliest arrest, hearing,
#                      disposition or filing date: dates the case
#   first_dispositiondate   earliest disposition date: dates the conviction
#   charged_<type>     a charge of that offense type (functions/offense_types.R)
#   conv_<type>        a convicted charge of that type (the amended code section
#                      if the charge was amended); carceral and non-carceral
#   incar_<type>       an incarcerated charge of that type
#
# Also, one row per revocation proceeding (for the post-sentencing revocation
# indicators of 04): case (the case it is filed under), person_id, filed (the
# filing date, which dates the violation: the offense date often repeats the
# original offense's), offensedate and arrestdate (its own; the offense date is the
# violation's date on some records and repeats the original offense's on others),
# class (fel, mis: its own charge type), revoked, incarcerated (revoked, carceral),
# own_type (the offense type of its own code section), unlinked (no earlier conviction
# in the data, so also counted as a conviction in the case record).
#
# Inputs:  orig_dir/Circuit criminal 2000-2019/circuit_2000_2019.csv
#          data_dir/vcc_2026_active_retired.xlsx   (Virginia Crime Codes)
# Also, one row per felony charge that is not a revocation proceeding (the charges
# of the judge-IV cases, 03): case, person_id, fips, casenumber_raw (as in the
# Cases files), codesection, felony_class, dispositioncode, offensedate,
# dispositiondate, dob, convicted, incarcerated, carceral_days (sentence minus
# suspended time of an incarcerated charge), concurrent, life_death.
#
# Also, the same case record of the felony charges that are neither revocation
# proceedings nor technical hearings (failure to appear, bond and pretrial
# violations, motions, review hearings, ...; below): the new felony cases of the
# recidivism outcomes (cc_post*, 04).
#
# Outputs: data_dir/cc_cases_full.csv, data_dir/cc_cases_recid.csv, data_dir/cc_revocations_full.csv,
#          data_dir/cc_felony_charges.csv,
#          data_dir/cc_case_xwalk.csv (fips, person_id, casenumber -> case)
# ==============================================================================

# penalty fields: numeric (above zero) and text (filled in)
penalty_num <- c("sentencetime", "sentencesuspended", "fineamount", "restitutionamount", "costs",
                 "probationtime", "operatorlicensesuspensiontime")
penalty_chr <- c("jailpenitentiary", "lifedeath", "programtype", "probationtype", "probationstarts",
                 "courtdmvsurrender", "driverimprovementclinic", "drivingrestrictions", "vaalcoholsafetyaction")

# supervision-violation code sections: the statutes under the heading "SUPERVISION
# VIOLATION" in the Virginia Crime Codes (active and retired codes), other than the
# pretrial violations (subheadings "Pretrial - ..."). A charge's code section matches
# if it starts with one (subsection dropped): "19.2-306 SC", "16.1-291(A)", ...
supervision_vcc <- read_excel(file.path(data_dir, "vcc_2026_active_retired.xlsx"), col_types = "text") |>
  filter(HEADING == "SUPERVISION VIOLATION", !str_starts(SUBHEAD, "Pretrial"), !is.na(STATUTE)) |>
  distinct(SUBHEAD, STATUTE)
cat("supervision-violation statutes (VCC):\n"); print(as.data.frame(supervision_vcc))
supervision_codes <- unique(str_remove(supervision_vcc$STATUTE, "\\(.*$"))
supervision_regex <- str_c("^(", str_c(str_replace_all(supervision_codes, "\\.", "\\\\."), collapse = "|"), ")(?![0-9.:])")

# charge texts of violations filed under other code sections (or none): probation,
# suspended sentence, good behavior and parole violations; show cause, capias,
# contempt and court orders; escape -- but not pretrial violations (bond, bail,
# failure to appear) or driving on a revoked / suspended license (46.2-30x)
violation_text     <- str_c("PROB|P/V|\\bPV\\b|\\bVOP\\b|SUSP(ENDED)? SENT|GOOD BEHAV|REVOKE|REVOCATION|",
                            "COMMUNITY DIVERSION|FIRST OFFENDER|PAROLE|SHOW ?CAUSE|S/C|CAPIAS|CONTEMPT|COURT ORDER|ESCAPE")
not_violation_text <- str_c("BOND|BAIL|PRE-?TRIAL|RECOGNIZANCE|FTA|F[.]T[.]A|FAIL(URE)? TO APPEAR|",
                            "DRIV|DWR|DWS|OPERAT|LICENSE|REVO/SUSP|SUSP/REVO")

# technical hearings, not new offenses: left out of the new felony cases of the
# recidivism outcomes (04), as the Revolving Door paper does (drop_nonfel_revo_techhrng.do,
# without its matches of real offenses: driving on a revoked license, petit larceny
# with prior convictions, drug offenses near a school, ...). Charge texts: failure to
# appear; bond, bail and pretrial-release violations; motions (to reconsider, modify,
# reinstate, ...) and review hearings (costs, drug court); community service in lieu
# of costs, payment plans, failure to pay restitution / costs / fines; rules to show
# cause; failure to comply with court conditions, to report to jail or serve weekends;
# escape; revocations and probation or show-cause hearings written "REVOC-", "/VP",
# "/SC"; first-offender violations -- but not driving offenses, motor vehicles or the
# sex-offender registry (18.2-472). Code sections: failure to appear (19.2-128) and the
# pretrial supervision violations of the Virginia Crime Codes (19.2-152.4:1)
technical_text  <- str_c("\\bFTA\\b|F[.]T[.]A|FAIL(URE)? (TO )?APPEAR|\\bFT APPEAR|",
                         "\\bBOND\\b|\\bBAIL\\b|PRE-?TRIAL|RECOGNIZANCE|",
                         "MOTION|\\bMOT\\b|\\bMTR\\b|RECONSIDER|REINSTATE|\\bREVIEW\\b|",
                         "COMM(UNITY)? SERVICE|PAYMENT PLAN|MONTHLY PAYMENT|LOWER PAYMENT|",
                         "FAIL(URE)? TO PAY (RESTITUTION|COSTS?|FINES?)|",
                         "\\bRULE\\b|FAIL(URE)? TO COMPLY|FAIL COMPLY|SERVE WEEKENDS|R(E)?P(OR)?T TO JAIL|\\bESC(APE)?\\b|",
                         "\\bREVOC\\b|REVOC-|/VP\\b|\\bVP/|/SC\\b|\\bSC/|VIOL(ATION)? OF FIRST OFF")
not_technical   <- "DRIV|DWR|DWS|OPERAT|\\bOPER\\b|LICENSE|HABITUAL|\\bHAB\\b|MOT(OR)? ?VEH|MOTION PICTURE"
technical_codes <- "^(19\\.2-128|19\\.2-152\\.4:1)(?![0-9.:])"

# ---- full CC charge record, grouped into cases -----------------------------------
# empty fields stay "" (na = none); dates are "01jan2005"
cc <- read_csv(file.path(orig_dir, "Circuit criminal 2000-2019", "circuit_2000_2019.csv"),
               col_select = all_of(c("fips", "person_id", "casenumber", "charge", "codesection", "amendedcodesection",
                                     "chargetype", "class", "dispositioncode", "filed", "offensedate", "arrestdate",
                                     "hearingdate", "dispositiondate", "dob", "concurrentconsecutive",
                                     penalty_num, penalty_chr)),
               col_types = cols(.default = col_character()), na = character()) |>
  rename(felony_class = class) |>
  mutate(across(c(fips, person_id), as.numeric),
         across(all_of(penalty_num), \(x) coalesce(as.numeric(x), 0)),
         across(c(filed, offensedate, arrestdate, hearingdate, dispositiondate, dob), \(x) as.Date(x, "%d%b%Y")),
         # dates after 2019 (the end of the data) are errors
         across(c(offensedate, arrestdate, hearingdate, dispositiondate), \(x) if_else(year(x) <= 2019, x, NA)),
         chargetype     = if_else(chargetype == "F", "Felony", chargetype),
         casenumber_raw = casenumber) |>                    # as in the Cases files (03); cleaned below
  cc_case_groups()
cat("CC charges:", format(nrow(cc), big.mark = ","), "| cases:", format(n_distinct(cc$case), big.mark = ","),
    "| persons:", format(n_distinct(cc$person_id), big.mark = ","), "\n")
stopifnot(mean(is.na(cc$filed)) < 0.01)                  # English month names: LC_TIME = "C" (00_master.R)

# the case of each court x person x (cleaned) case number: links the original
# judge-IV case groups (model 17) to the cases here (99_checks.R)
write_csv(distinct(cc, fips, person_id, casenumber, case), file.path(data_dir, "cc_case_xwalk.csv"))

# ---- charge outcomes ------------------------------------------------------------------
charges <- cc |>
  filter(chargetype %in% c("Felony", "Misdemeanor")) |>
  mutate(vcc_code     = str_detect(str_remove(codesection, "^[A-Z][.](?=[0-9])"), supervision_regex),
         vcc_text     = str_detect(charge, violation_text) & !str_detect(charge, not_violation_text) &
                          !str_starts(codesection, "46.2-30"),
         supervision  = vcc_code | vcc_text,
         # disposition "Sentence/Probation Revoked": a revocation whatever the code section
         revocation   = dispositioncode == "Sentence/Probation Revoked" | supervision,
         technical    = !revocation & ((str_detect(charge, technical_text) & !str_detect(charge, not_technical) &
                                          !str_starts(codesection, "18.2-472")) |
                                         str_detect(str_remove(codesection, "^[A-Z][.](?=[0-9])"), technical_codes)),
         penalty      = if_any(all_of(penalty_num), \(x) x > 0) | if_any(all_of(penalty_chr), \(x) x != ""),
         guilty       = dispositioncode == "Guilty" | (dispositioncode %in% c("Appeal Withdrawn", "Resolved", "") & penalty),
         class        = if_else(chargetype == "Felony", "fel", "mis"),
         revoked      = dispositioncode == "Sentence/Probation Revoked" | (supervision & guilty),
         convicted    = !revocation & guilty,
         carceral     = (jailpenitentiary %in% c("Jail", "Penitentiary") & sentencetime - sentencesuspended > 0) |
                          lifedeath != "",
         type         = offense_type_of(codesection),
         conv_type    = offense_type_of(if_else(amendedcodesection != "", amendedcodesection, codesection)))
cat("felony/misdemeanor charges:", format(nrow(charges), big.mark = ","), "(non-criminal left out:",
    format(nrow(cc) - nrow(charges), big.mark = ","), ")\n")
cat("revocation proceedings by source (not exclusive): disposition revoked",
    format(sum(charges$dispositioncode == "Sentence/Probation Revoked"), big.mark = ","), "| VCC code section",
    format(sum(charges$vcc_code), big.mark = ","), "| violation charge text",
    format(sum(charges$vcc_text), big.mark = ","), "| text only",
    format(sum(charges$vcc_text & !charges$vcc_code & charges$dispositioncode != "Sentence/Probation Revoked"), big.mark = ","), "\n")
cat("technical hearings (left out of the recidivism outcomes): felony charges",
    format(sum(charges$technical & charges$class == "fel"), big.mark = ","), "of",
    format(sum(!charges$revocation & charges$class == "fel"), big.mark = ","), "that are not revocation proceedings\n")
charges |>
  filter(technical, class == "fel") |>
  count(charge, sort = TRUE) |>
  head(15) |>
  print()

# the felony charges that are not revocation proceedings: the charges of the judge-IV
# cases (03), with their outcome by the rules above and their active time
charges |>
  filter(class == "fel", !revocation) |>
  transmute(case, person_id, fips, casenumber_raw, codesection, felony_class, dispositioncode, offensedate,
            dispositiondate, dob, convicted = as.integer(convicted), incarcerated = as.integer(convicted & carceral),
            carceral_days = if_else(convicted & carceral, pmax(sentencetime - sentencesuspended, 0), 0),
            concurrent = as.integer(concurrentconsecutive == "Concurrent"),
            life_death = as.integer(convicted & lifedeath != "")) |>
  write_csv(file.path(data_dir, "cc_felony_charges.csv"))

# revocations: dropped if the person has a conviction disposed on or before them
# (their disposition date, else -- disposed after 2019 or undated -- their hearing,
# arrest, offense or filing date), else counted as convictions (of an earlier
# conviction outside the data)
first_conv <- charges |>
  filter(convicted, !is.na(dispositiondate)) |>
  arrange(dispositiondate) |>
  distinct(person_id, .keep_all = TRUE) |>
  select(person_id, first_conv_date = dispositiondate)
charges <- charges |>
  left_join(first_conv, by = "person_id", relationship = "many-to-one") |>
  mutate(unlinked = revoked & !coalesce(first_conv_date <= coalesce(dispositiondate, hearingdate, arrestdate,
                                                                    offensedate, filed), FALSE))
cat("revocation proceedings:", format(sum(charges$revocation), big.mark = ","), "| revocations:",
    format(sum(charges$revoked), big.mark = ","), "| of which with no earlier conviction (counted as convictions):",
    format(sum(charges$unlinked), big.mark = ","), "\n")
charges |>
  filter(revocation) |>
  transmute(case, person_id, fips, casenumber_raw, filed, offensedate, arrestdate, class, revoked = as.integer(revoked),
            incarcerated = as.integer(revoked & carceral), own_type = type, unlinked = as.integer(unlinked)) |>
  write_csv(file.path(data_dir, "cc_revocations_full.csv"))
charges <- charges |>
  filter(!revocation | unlinked) |>
  mutate(convicted = convicted | unlinked, incarcerated = convicted & carceral)

# ---- case record: one row per case x charge class (felony, misdemeanor) ------------
case_record <- \(charges) {
  cases <- charges |>
    summarise(n_charges = n(), n_revocations = sum(revocation), convicted = any(convicted),
              incarcerated = any(incarcerated), .by = c(case, person_id, fips, class)) |>
    mutate(outcome = case_when(incarcerated ~ "carceral", convicted ~ "noncarceral", .default = "dismissal")) |>
    select(-convicted, -incarcerated)

  # earliest dates of the case's charges of the class: each case x class's first row
  # after sorting (missing dates sort last; much faster than min() by case). The
  # case date is the first offense date, else the first arrest, hearing, disposition
  # or filing date
  first_per_case <- \(date) charges |>
    arrange(.data[[date]]) |>
    distinct(case, class, .keep_all = TRUE) |>
    select(case, class, all_of(date)) |>
    rename_with(\(x) str_c("first_", x), all_of(date))
  case_dates <- map(c("offensedate", "arrestdate", "hearingdate", "dispositiondate", "filed"), first_per_case) |>
    reduce(\(x, y) full_join(x, y, by = c("case", "class"), relationship = "one-to-one")) |>
    transmute(case, class, first_offdate = first_offensedate,
              case_date = coalesce(first_offensedate, first_arrestdate, first_hearingdate, first_dispositiondate, first_filed),
              first_dispositiondate) |>
    # latest offense date of the charges that are not revocation proceedings (theirs
    # often repeat the original offense): each case x class's first row after
    # sorting in descending order
    left_join(charges |>
                filter(!is.na(offensedate), !revocation) |>
                arrange(desc(offensedate)) |>
                distinct(case, class, .keep_all = TRUE) |>
                select(case, class, last_offdate = offensedate),
              by = c("case", "class"), relationship = "one-to-one") |>
    relocate(last_offdate, .after = first_offdate)

  # offense types of all charges, of the convicted ones and of the incarcerated ones
  case_types <- bind_rows(charged = charges |> distinct(case, class, type),
                          conv    = charges |> filter(convicted) |> distinct(case, class, type = conv_type),
                          incar   = charges |> filter(incarcerated) |> distinct(case, class, type = conv_type),
                          .id = "status") |>
    mutate(type = factor(type, levels = offense_type_names), value = 1L) |>
    pivot_wider(id_cols = c(case, class), names_from = c(status, type), values_from = value, values_fill = 0L,
                names_expand = TRUE)

  cases <- cases |>
    left_join(case_dates, by = c("case", "class"), relationship = "one-to-one") |>
    left_join(case_types, by = c("case", "class"), relationship = "one-to-one") |>
    relocate(first_offdate, case_date, first_dispositiondate, .after = outcome)
  stopifnot(!anyDuplicated(select(cases, case, class)), !anyNA(cases$charged_other))
  cases
}

# all cases (priors, 04); and the new felony cases of the recidivism outcomes (04):
# the felony charges that are neither revocation proceedings (also not those counted
# as convictions above) nor technical hearings
cases <- case_record(charges)
write_csv(cases, file.path(data_dir, "cc_cases_full.csv"))
recid_cases <- case_record(filter(charges, class == "fel", !revocation, !technical))
write_csv(recid_cases, file.path(data_dir, "cc_cases_recid.csv"))
cat("cc_cases_recid (new felony cases of the recidivism outcomes):", format(nrow(recid_cases), big.mark = ","),
    "cases, against", format(sum(cases$class == "fel"), big.mark = ","), "cases with felony charges\n")
cat("cc_cases_full:", format(n_distinct(cases$case), big.mark = ","), "cases of",
    format(n_distinct(cases$person_id), big.mark = ","), "persons; case x class rows by outcome:\n")
print(count(cases, class, outcome) |> mutate(pct = round(100 * n / sum(n), 1), .by = class))
cat("case dates from: offense", sum(!is.na(cases$first_offdate)), "| fallback", sum(is.na(cases$first_offdate) & !is.na(cases$case_date)),
    "| none", sum(is.na(cases$case_date)), "\n")
