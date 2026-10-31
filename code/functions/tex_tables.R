# ==============================================================================
# tex_tables.R
#
# Helpers for the LaTeX tables of 06-07: booktabs tabulars (no float, for
# \input{}), with significance stars * p < 0.1, ** p < 0.05, *** p < 0.01.
#   stars(p)                 star superscript for p-values
#   fmt(x, digits)           fixed-decimal numbers with thousands separators
#   coef_cells(est, se, p)   two lines of cells: estimate with stars, (se)
#   tex_row(label, cells)    one row: label & cells \\
#   write_tex(file, cols, header, body, notes)   write the tabular; notes go
#                            into comment lines at the top of the file
# ==============================================================================

stars <- \(p) case_when(p < 0.01 ~ "^{***}", p < 0.05 ~ "^{**}", p < 0.1 ~ "^{*}", .default = "")
num   <- \(x, digits) formatC(x, format = "f", digits = digits, big.mark = ",")
# text cells: a minus sign, not a hyphen
fmt   <- \(x, digits = 3) {
  s <- num(x, digits)
  if_else(is.na(x), "", if_else(str_starts(s, "-"), str_c("$-$", str_sub(s, 2)), s))
}

# blank cells where the estimate is missing (e.g. a regressor dropped as collinear)
coef_cells <- \(est, se, p, digits = 3) list(est = if_else(is.na(est), "", str_c("$", num(est, digits), stars(p), "$")),
                                               se  = if_else(is.na(se), "", str_c("(", fmt(se, digits), ")")))

tex_row <- \(label, cells) str_c(label, " & ", str_flatten(cells, " & "), " \\\\")

# star_note = FALSE: no significance footnote (tables without tests)
write_tex <- function(file, cols, header, body, notes = NULL, star_note = TRUE) {
  n <- str_length(str_remove_all(cols, "[^lcr]"))
  lines <- c(if (!is.null(notes)) str_c("% ", str_wrap(notes, 100) |> str_split_1("\n")),
             str_c("\\begin{tabular}{", cols, "}"), "\\toprule", header, "\\midrule", body, "\\bottomrule",
             if (star_note) str_c("\\multicolumn{", n, "}{l}{\\footnotesize $^{*}$ p $<$ 0.1, $^{**}$ p $<$ 0.05, ",
                                  "$^{***}$ p $<$ 0.01} \\\\"),
             "\\end{tabular}")
  writeLines(lines, file)
  cat("  wrote", file, "\n")
}

# column header: groups spanning `span` columns each (after the label column), with
# their \cmidrule's
group_header <- \(groups, span) c(
  str_c(" & ", str_flatten(str_c("\\multicolumn{", span, "}{c}{", groups, "}"), " & "), " \\\\"),
  str_flatten(str_c("\\cmidrule(lr){", 2 + (seq_along(groups) - 1) * span, "-", 1 + seq_along(groups) * span, "}")))
yes_no <- \(x) if_else(x, "Yes", "No")
