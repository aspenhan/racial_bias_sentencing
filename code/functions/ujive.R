# ==============================================================================
# ujive.R
#
# ujive_loo(d, treat, controls, fe, window)
#   the judge instrument of loo_mean(d, residualize(d, treat, controls, fe), window,
#   loo_excl_person = TRUE) -- the mean, over the judge's cases of other defendants in
#   the judge x court x year window, of the treatment residualized on the controls and
#   fixed effects -- with each defendant also left out of the residualization (UJIVE,
#   Kolesar 2013): the instrument of a case of defendant P averages the residuals of the
#   regression estimated without P's cases. With the residualization on every case
#   (loo_mean), a case's own treatment shifts the fixed effects and control
#   coefficients, hence the residuals of the judge's other cases, and so enters its own
#   instrument with a small negative weight; here no defendant's treatment enters their
#   own instrument. Computed exactly, without re-estimation, by the block-deletion
#   formula of least squares: removing P's rows (residuals e_P, leverage block H_PP)
#   shifts the coefficients by -(X'X)^-1 X_P' (I - H_PP)^-1 e_P, so a case i of P gets
#     z_i + xbar_i' (X'X)^-1 X_P' (I - H_PP)^-1 e_P,
#   where z_i is the loo_mean instrument and xbar_i the same leave-out mean of the
#   regressors X (one indicator per level of each fixed effect and control: the controls
#   enter as fixed effects, as in residualize). As in
#   loo_mean, cases with treat NA enter neither the regression nor any mean but get an
#   instrument; NA if the judge has no other defendant's case in the window.
# ==============================================================================

ujive_loo <- function(d, treat, controls, fe, window = c("1yr", "3yr", "5yr")) {
  window <- match.arg(window)
  stopifnot(!anyNA(d$judgeid), !anyNA(d$fipscode), !anyNA(d$Y), !anyNA(d$person_id))
  h <- c("1yr" = 0, "3yr" = 1, "5yr" = 2)[[window]]
  y <- as.numeric(d[[treat]])
  est <- which(!is.na(y) & complete.cases(d[c(controls, fe)]))   # the regression's cases

  # regressors of the estimation cases: one indicator per level of each fixed effect and
  # control, without the collinear columns (each court's fixed effects overlap)
  X <- do.call(cbind, map(c(fe, controls), \(f) t(fac2sparse(factor(d[[f]][est])))))
  q <- qr(as.matrix(crossprod(X)), tol = 1e-9)
  X <- X[, sort(q$pivot[seq_len(q$rank)]), drop = FALSE]
  C <- chol2inv(chol(as.matrix(crossprod(X))))              # (X'X)^-1
  e <- y[est] - as.vector(X %*% (C %*% as.vector(crossprod(X, y[est]))))   # residuals

  # judge x court x year cells; each case's window: the cells of its judge and court in
  # years Y - h to Y + h; sums over the estimation cases of a window (residuals,
  # regressors, count), minus those of the case's own defendant
  cells <- distinct(d, judgeid, fipscode, Y) |> mutate(cell = row_number())
  cell_of <- left_join(select(d, judgeid, fipscode, Y), cells, by = c("judgeid", "fipscode", "Y"),
                       relationship = "many-to-one")$cell
  win <- inner_join(cells, cells, by = c("judgeid", "fipscode"), suffix = c("", "_k"),
                    relationship = "many-to-many") |>
    filter(abs(Y - Y_k) <= h)
  to_window <- sparseMatrix(i = win$cell, j = win$cell_k, x = 1, dims = c(nrow(cells), nrow(cells)))
  in_cell <- sparseMatrix(i = seq_along(est), j = cell_of[est], x = 1, dims = c(length(est), nrow(cells)))
  window_sum <- \(M) (to_window %*% crossprod(in_cell, M))[cell_of, , drop = FALSE]
  rows <- tibble(i = seq_len(nrow(d)), person_id = d$person_id, judgeid = d$judgeid, fipscode = d$fipscode, Y = d$Y)
  est_rows <- rows[est, ] |> transmute(k = seq_along(est), person_id, judgeid, fipscode, Y_k = Y)
  own <- inner_join(rows, est_rows, by = c("person_id", "judgeid", "fipscode"), relationship = "many-to-many") |>
    filter(abs(Y - Y_k) <= h)
  own_cases <- sparseMatrix(i = own$i, j = own$k, x = 1, dims = c(nrow(d), length(est)))
  n_other <- as.vector(window_sum(Matrix(1, length(est), 1))) - rowSums(own_cases)
  ok <- n_other > 0 & (window == "1yr" | between(d$Y, 2000, 2020))   # rolling windows: 2000-2020, as loo_mean
  scale <- Diagonal(x = if_else(ok, 1 / n_other, 0))
  z <- as.vector(scale %*% (window_sum(Matrix(e, ncol = 1)) - own_cases %*% e))
  xbar <- scale %*% (window_sum(X) - own_cases %*% X)

  # each case i with each estimation case k of its own defendant: q_ik = xbar_i' C x_k;
  # within a defendant, the leverage block h_kl = x_k' C x_l
  chunked <- \(a, rows_a, b, rows_b) {          # row-wise (a[rows_a, ] C) . b[rows_b, ]
    out <- numeric(length(rows_a))
    for (ch in split(seq_along(rows_a), ceiling(seq_along(rows_a) / 5e4)))
      out[ch] <- rowSums(as.matrix(a[rows_a[ch], , drop = FALSE] %*% C) * as.matrix(b[rows_b[ch], , drop = FALSE]))
    out
  }
  pairs <- inner_join(select(rows, i, person_id), select(est_rows, k, person_id), by = "person_id",
                      relationship = "many-to-many") |>
    filter(ok[i])
  pairs$q <- chunked(xbar, pairs$i, X, pairs$k)
  blocks <- inner_join(select(est_rows, k, person_id), select(est_rows, l = k, person_id), by = "person_id",
                       relationship = "many-to-many")
  blocks$h <- chunked(X, blocks$k, X, blocks$l)

  # w_P = (I - H_PP)^-1 e_P for each defendant (a generalized inverse where a defendant's
  # cases alone determine a fixed-effect level: their residuals are then zero)
  w <- numeric(length(est))
  single <- blocks |> filter(n() == 1, .by = person_id)
  w[single$k] <- if_else(1 - single$h > 1e-8, e[single$k] / (1 - single$h), 0)
  multi <- as.data.frame(filter(blocks, n() > 1, .by = person_id))
  for (b in split(multi, multi$person_id)) {
    ks <- sort(unique(b$k))
    H <- matrix(0, length(ks), length(ks))
    H[cbind(match(b$k, ks), match(b$l, ks))] <- b$h
    A <- diag(length(ks)) - H
    w[ks] <- if (rcond(A) > 1e-10) solve(A, e[ks]) else as.vector(MASS::ginv(A) %*% e[ks])
  }

  # the instrument with the defendant left out of the residualization
  correction <- pairs |> summarise(c = sum(q * w[k]), .by = i)
  out <- if_else(ok, z, NA_real_)
  out[correction$i] <- out[correction$i] + correction$c
  out
}
