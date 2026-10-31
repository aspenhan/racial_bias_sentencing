# ==============================================================================
# cc_case_groups.R
#
# Groups circuit-court (CC) charges into cases as in Parts 2-3 of
# Cecile code/replication/01_cleaning/judge_iv/va_criminal_court_data_2020_judge_iv_casenumber.do
# (the "part1only" case_group). A case number looks like CRyy######-pp: part1 is
# the number before the dash and part2 the two digits after it. Letters inserted
# into the number (e.g. CR07F05795-00) stand for zeros, so part1 is recovered
# format by format. Within court x person, with charges sorted by part1 and
# part2, a charge starts a new case unless its part1 equals the previous charge's
# part1 or exceeds it by 1 or 2, or its part2 exceeds the previous part2 by 3
# (part2, not part1, in the original).
#
# cc_case_groups(charges) takes charge-level data with fips (numeric), person_id
# and casenumber; returns it with casenumber cleaned and a case id (case).
# Charges whose case number fits none of the 14 formats are dropped.
# ==============================================================================

cc_case_formats <- c(
  CR8d_2d         = "CR\\d{8}-\\d{2}",
  CR7d1w_2d       = "CR\\d{7}[A-Z]-\\d{2}",
  CR2d1w5d_2d     = "CR\\d{2}[A-Z]\\d{5}-\\d{2}",
  CR4d1w3d_2d     = "CR\\d{4}[A-Z]\\d{3}-\\d{2}",
  CR5d1w2d_2d     = "CR\\d{5}[A-Z]\\d{2}-\\d{2}",
  CR3d1w4d_2d     = "CR\\d{3}[A-Z]\\d{4}-\\d{2}",
  CR6d1w1d_2d     = "CR\\d{6}[A-Z]\\d{1}-\\d{2}",
  CR2d2w4d_2d     = "CR\\d{2}[A-Z]{2}\\d{4}-\\d{2}",
  CR2d3w3d_2d     = "CR\\d{2}[A-Z]{3}\\d{3}-\\d{2}",
  CR2d4w2d_2d     = "CR\\d{2}[A-Z]{4}\\d{2}-\\d{2}",
  CR2d1w4d1w_2d   = "CR\\d{2}[A-Z]\\d{4}[A-Z]-\\d{2}",
  CR2d1w3d1w1d_2d = "CR\\d{2}[A-Z]\\d{3}[A-Z]\\d{1}-\\d{2}",
  CR2d1w2d1w2d_2d = "CR\\d{2}[A-Z]\\d{2}[A-Z]\\d{2}-\\d{2}",
  CR2d1w1d1w3d_2d = "CR\\d{2}[A-Z]\\d[A-Z]\\d{3}-\\d{2}")

cc_case_groups <- function(charges) {
  # capture groups of a regex; NA where it does not match
  grp <- \(x, pattern, i) str_match(x, pattern)[, i + 1]
  zeros <- \(x) as.numeric(str_replace_all(x, "[A-Z]", "0"))   # letters stand for zeros

  out <- charges |>
    mutate(
      # typos: extra dashes inside the number (twice, for numbers with several), three one-offs
      casenumber = str_replace_all(casenumber, "-(?=\\d+-\\d{2})", "0"),
      casenumber = str_replace_all(casenumber, "-(?=\\d+-\\d{2})", "0"),
      casenumber = case_match(casenumber,
                              "CR00      -00"  ~ "CR00000000-00",
                              "CR0200575 -00"  ~ "CR02005750-00",
                              "CR120M1`04-80"  ~ "CR120M1048-00",
                              .default = casenumber),
      # format: the last matching pattern, as in the original sequence of replaces
      format = reduce(names(cc_case_formats),
                      \(acc, f) if_else(str_detect(casenumber, cc_case_formats[[f]]), f, acc),
                      .init = rep(NA_character_, n())))

  cat("cc_case_groups: charges with an unclassified case number (dropped):", sum(is.na(out$format)), "\n")

  out <- out |>
    filter(!is.na(format)) |>
    mutate(
      # part1: the digits before the dash (a letter there: the digits before it)
      part1 = grp(casenumber, "(\\d+)-\\d{2}$", 1),
      part1 = coalesce(part1, grp(casenumber, "CR(\\d{7})[A-Z]-\\d{2}", 1),
                       grp(casenumber, "CR\\d{2}[A-Z](\\d{4})[A-Z]-\\d{2}", 1)),
      part1 = as.numeric(part1),
      part2 = as.numeric(grp(casenumber, "-(\\d{2})$", 1)),
      # part1 corrections, in the original's order
      corr = case_when(
        format == "CR2d3w3d_2d" ~ zeros(grp(casenumber, "CR(\\d{2}[A-Z]{3}\\d{3})-\\d{2}", 1)),
        format == "CR2d4w2d_2d" ~ zeros(grp(casenumber, "CR(\\d{2}[A-Z]{4}\\d{2})-\\d{2}", 1))),
      part1 = coalesce(corr, part1),
      corr = if_else(format %in% c("CR2d1w5d_2d", "CR2d2w4d_2d"),
                     zeros(grp(casenumber, "CR(\\d{2}[A-Z]{1,2}\\d{4,5})-\\d{2}", 1)), NA),
      part1 = coalesce(corr, part1),
      corr = if_else(format == "CR2d1w4d1w_2d" & fips == 760,
                     as.numeric(str_c(grp(casenumber, "CR(\\d{2})[A-Z](\\d{4})[A-Z]-\\d{2}", 1), "00",
                                      grp(casenumber, "CR(\\d{2})[A-Z](\\d{4})[A-Z]-\\d{2}", 2))), NA),
      part1 = coalesce(corr, part1),
      # a letter took the place of a zero: put the zero back
      corr = if_else(format == "CR7d1w_2d",
                     as.numeric(str_c(grp(casenumber, "CR(\\d{2})(0+)(\\d+)[A-Z]-\\d{2}", 1),
                                      grp(casenumber, "CR(\\d{2})(0+)(\\d+)[A-Z]-\\d{2}", 2), "0",
                                      grp(casenumber, "CR(\\d{2})(0+)(\\d+)[A-Z]-\\d{2}", 3))), NA),
      part1 = coalesce(corr, part1),
      corr = if_else(format %in% c("CR4d1w3d_2d", "CR5d1w2d_2d", "CR3d1w4d_2d", "CR6d1w1d_2d") &
                       fips %in% c(23, 63, 85, 87, 131, 175, 187, 520, 71, 75, 105, 185),
                     as.numeric(str_c(grp(casenumber, "CR(\\d{2})(0+)[A-Z](\\d+)-\\d{2}", 1),
                                      grp(casenumber, "CR(\\d{2})(0+)[A-Z](\\d+)-\\d{2}", 2), "0",
                                      grp(casenumber, "CR(\\d{2})(0+)[A-Z](\\d+)-\\d{2}", 3))), NA),
      part1 = coalesce(corr, part1),
      corr = if_else(format %in% c("CR2d1w3d1w1d_2d", "CR2d1w2d1w2d_2d", "CR2d1w1d1w3d_2d"),
                     as.numeric(str_c(grp(casenumber, "CR(\\d{2})[A-Z](\\d{1,3})[A-Z](\\d{1,3})-\\d{2}", 1), "0",
                                      grp(casenumber, "CR(\\d{2})[A-Z](\\d{1,3})[A-Z](\\d{1,3})-\\d{2}", 2), "0",
                                      grp(casenumber, "CR(\\d{2})[A-Z](\\d{1,3})[A-Z](\\d{1,3})-\\d{2}", 3))), NA),
      part1 = coalesce(corr, part1),
      # fips 770: the number before the letter identifies the case
      corr = if_else(format == "CR6d1w1d_2d" & fips == 770,
                     as.numeric(str_c(grp(casenumber, "CR(\\d{2})(\\d{4})[A-Z]\\d-\\d{2}", 1), "00",
                                      grp(casenumber, "CR(\\d{2})(\\d{4})[A-Z]\\d-\\d{2}", 2))), NA),
      part1 = coalesce(corr, part1))
  stopifnot(!anyNA(out$part1), !anyNA(out$part2))

  # cases: runs of charges with close case numbers within court x person
  out |>
    arrange(fips, person_id, part1, part2) |>
    mutate(close = coalesce(part1 == lag(part1) | part1 == lag(part1) + 1 | part1 == lag(part1) + 2 |
                              part2 == lag(part2) + 3, FALSE),
           case  = cumsum(!close | fips != lag(fips, default = -1) | person_id != lag(person_id, default = -1))) |>
    select(-format, -part1, -part2, -corr, -close)
}
