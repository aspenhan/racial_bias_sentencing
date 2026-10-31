# ==============================================================================
# gen_loo_instruments.R
#
# Replicates Cecile code/replication/01_cleaning/gen_loo_instruments.do, with
# residualization on controls and fixed effects: leave-one-out mean
# judge instruments.
#
# residualize(d, treat, controls, fe)
#   residual of feols(treat ~ 1 | <fe> + <controls>), the controls entering as fixed
#   effects (one indicator per value; user decision 2026-10-26); NA where treat is NA.
#   Both controls and fe NULL = the treatment itself.
#
# loo_mean(d, v, window, grvar, loo_excl_person)
#   for each case in d, the mean of v over the judge's other cases in the court
#   and year window: the judge x court x year cell ("1yr") or a rolling window
#   ("3yr"/"5yr", filled in for years 2000-2020), leaving out the case, or all of
#   the person's cases. Cases with v = NA do not enter any mean, but still get
#   one -- so v = NA outside a subsample gives every case the mean over the
#   judge's cases in that subsample.
#
# gen_loo_instruments(d, window, grvar, loo_excl_person, resid_controls, resid_fe,
#                     resid_treat, ujive)
#   the instruments for each treatment -- d0 (dismissed), d1 (convicted, not
#   incarcerated), d2 (incarcerated): loo_d0_jdg_yr, loo_d1_jdg_yr, loo_d2_jdg_yr.
#   resid_controls and resid_fe (all absorbed as fixed effects) are partialled out of the
#   treatments in resid_treat (default: all three); with ujive = TRUE (leave-out-person
#   means only), the defendant is also left out of that residualization
#   (functions/ujive.R). Returns d without the cases
#   where an unadjusted leave-out mean of d0, d1, d2, cond_incar (incarcerated |
#   convicted) or conviction (1 - d0) equals 1, as the original.
# ==============================================================================

residualize <- function(d, treat, controls = NULL, fe = NULL) {
  fe <- c(fe, controls)
  if (!length(fe)) return(d[[treat]])
  fit <- feols(as.formula(str_glue("{treat} ~ 1 | {str_flatten(fe, ' + ')}")), data = d, notes = FALSE,
               fixef.rm = "none", fixef.tol = 1e-10, fixef.iter = 1e5)
  resid(fit, na.rm = FALSE)
}

loo_mean <- function(d, v, window = c("1yr", "3yr", "5yr"), grvar = NULL, loo_excl_person = FALSE) {
  window <- match.arg(window)
  stopifnot(!anyNA(d$judgeid), !anyNA(d$fipscode), !anyNA(d$Y))
  h <- c("1yr" = 0, "3yr" = 1, "5yr" = 2)[[window]]
  keys <- c("judgeid", "fipscode", grvar)

  # for each row: total (S) and count (N) of v over the judge x court x window
  # cell, or over the row's person within that cell (by = keys + person_id)
  cell_totals <- function(by) {
    cells <- d |>
      select(all_of(by), Y) |>
      mutate(v = v) |>
      summarise(s = sum(v, na.rm = TRUE), n = sum(!is.na(v)), .by = all_of(c(by, "Y")))
    windows <- map(-h:h, \(offset) mutate(cells, Y = Y + offset)) |>
      list_rbind() |>
      summarise(S = sum(s), N = sum(n), .by = all_of(c(by, "Y")))
    if (window != "1yr") windows <- filter(windows, between(Y, 2000, 2020))
    d |>
      select(all_of(by), Y) |>
      left_join(windows, by = c(by, "Y"), relationship = "many-to-one")
  }

  # leave-out mean: (S - own) / (N - own), own = the case's or person's contribution
  cell <- cell_totals(keys)
  own <- if (loo_excl_person) {
    cell_totals(c(keys, "person_id"))
  } else {
    tibble(S = coalesce(v, 0), N = as.integer(!is.na(v)))
  }
  out <- (cell$S - own$S) / (cell$N - own$N)
  if_else(is.finite(out), out, NA_real_)                  # no other cases in the cell
}

gen_loo_instruments <- function(d, window = c("1yr", "3yr", "5yr"), grvar = NULL,
                                loo_excl_person = FALSE, resid_controls = NULL, resid_fe = NULL,
                                resid_treat = c("d0", "d1", "d2"), ujive = FALSE) {
  window <- match.arg(window)
  treats <- c(d0 = "d0", d1 = "d1", d2 = "d2")
  stopifnot(all(resid_treat %in% treats), !ujive || (loo_excl_person && is.null(grvar)))

  loo_of <- \(v) loo_mean(d, v, window, grvar, loo_excl_person)
  residualized <- set_names(treats %in% resid_treat & length(c(resid_controls, resid_fe)) > 0, names(treats))
  loo <- imap(treats, \(treat, ab) {
    if (!residualized[[ab]]) loo_of(d[[treat]])
    else if (ujive) ujive_loo(d, treat, resid_controls, resid_fe, window)          # functions/ujive.R
    else loo_of(residualize(d, treat, resid_controls, resid_fe))
  })

  # drop cases where an unadjusted leave-out mean equals 1: d0, d1, d2, cond_incar
  # (incarcerated | convicted), or conviction (1 - d0, so d0 = 0)
  unadj <- map(c(treats, cinc = "cond_incar"), \(treat) loo_of(d[[treat]]))
  keep <- with(unadj, !(d0 %in% c(0, 1) | d1 %in% 1 | d2 %in% 1 | cinc %in% 1))
  cat("(", format(sum(!keep), big.mark = ","), " observations deleted: leave-out mean equals 1)\n", sep = "")

  d |>
    mutate(loo_d0_jdg_yr = loo$d0, loo_d1_jdg_yr = loo$d1, loo_d2_jdg_yr = loo$d2) |>
    filter(keep)
}
