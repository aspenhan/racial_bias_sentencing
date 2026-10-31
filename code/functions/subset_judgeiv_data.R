# ==============================================================================
# subset_judgeiv_data.R
#
# Replicates Cecile code/replication/01_cleaning/subset_judgeiv_data.do. Keeps
#   (1) courts in court_fipscode_list
#   (2) judges with at least case_count_cutoff cases in the court, counted in the
#       same year ("1yr") or a rolling 3-/5-year window (filled in for years
#       2000-2020; other years are not dropped)
#   (3) court x min_judges_window cells with at least min_judges_per_court judges
#   (4) courts observed in at least court_yr_min years
# drop_if_dummy first drops rows where that variable is nonzero (or missing).
# ==============================================================================

subset_judgeiv_data <- function(d, court_fipscode_list, case_count_window = c("1yr", "3yr", "5yr"),
                                case_count_cutoff, min_judges_per_court, min_judges_window = "Y",
                                court_yr_min, drop_if_dummy = NULL) {
  case_count_window <- match.arg(case_count_window)
  report <- function(msg, n_before) {
    cat(msg, "\n(", format(n_before - nrow(d), big.mark = ","), " observations deleted)\n", sep = "")
  }

  # step 0: drop rows flagged by drop_if_dummy
  if (!is.null(drop_if_dummy)) d <- filter(d, .data[[drop_if_dummy]] %in% 0)

  # step 1: keep only courts in court_fipscode_list
  n0 <- nrow(d)
  d <- filter(d, fipscode %in% court_fipscode_list)
  report("Dropped due to subsetting to specific courts:", n0)

  # step 2: number of cases of the judge in the court, by year or rolling window
  n0 <- nrow(d)
  if (case_count_window == "1yr") {
    d <- mutate(d, n_cases = n(), .by = c(judgeid, Y, fipscode))
  } else {
    h <- if (case_count_window == "3yr") 1 else 2
    cells <- count(d, judgeid, fipscode, Y)
    n_window <- map(-h:h, \(offset) mutate(cells, Y = Y + offset)) |>
      list_rbind() |>
      summarise(n_cases = sum(n), .by = c(judgeid, fipscode, Y)) |>
      filter(between(Y, 2000, 2020))
    d <- left_join(d, n_window, by = c("judgeid", "fipscode", "Y"), relationship = "many-to-one")
  }
  d <- d |>
    filter(is.na(n_cases) | n_cases >= case_count_cutoff) |>
    select(-n_cases)
  report(str_glue("Dropped due to the {case_count_window} case count cutoff:"), n0)

  # step 3: drop court x window cells with too few judges
  n0 <- nrow(d)
  d <- filter(d, n_distinct(judgeid) >= min_judges_per_court,
              .by = all_of(c(min_judges_window, "fipscode")))
  report("Dropped due to minimum number of judges per court*year:", n0)

  # step 4: drop courts observed in too few years
  n0 <- nrow(d)
  d <- filter(d, n_distinct(Y) >= court_yr_min, .by = fipscode)
  report("Dropped due to minimum number of years we must see court (after other drops):", n0)

  d
}
