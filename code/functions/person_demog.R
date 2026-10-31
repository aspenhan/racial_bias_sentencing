# ==============================================================================
# person_demog.R
#
# Race and sex in the full CC record (04, 11):
#   cc_charge_demog(path)   one row per charge: person_id, date (offense, else
#                           disposition), race and sex. The two race codings used over
#                           time ("Black (Non-Hispanic)" / "Black", ...) are harmonized;
#                           blanks are NA
#   person_demog(charges)   one row per person and variable (var = "race", "sex"): the
#                           modal value over the person's charges; "Other or unknown"
#                           race only if no other race is recorded; ties go to the value
#                           on the person's most recent charge. n_values: distinct values
#                           recorded; tied: the mode was a tie. Persons without any
#                           recorded value of a variable have no row for it
# ==============================================================================

cc_charge_demog <- \(path) {
  read_csv(path, col_select = c(person_id, race, sex, offensedate, dispositiondate),
           col_types = cols(.default = col_character()), na = character()) |>
    transmute(person_id = as.numeric(person_id),
              date = coalesce(as.Date(offensedate, "%d%b%Y"), as.Date(dispositiondate, "%d%b%Y")),
              race = case_match(race,
                                c("Black (Non-Hispanic)", "Black") ~ "Black",
                                c("White Caucasian (Non-Hispanic)", "White") ~ "White",
                                "Hispanic" ~ "Hispanic",
                                "Asian Or Pacific Islander" ~ "Asian or Pacific Islander",
                                c("American Indian", "American Indian Or Alaskan Native") ~ "American Indian or Alaskan Native",
                                c("Other (Includes Not Applicable, Unknown)", "Unknown") ~ "Other or unknown"),
              sex = na_if(sex, ""))
}

person_demog <- \(charges) {
  charges |>
    arrange(person_id, desc(date)) |>
    mutate(recency = row_number(), .by = person_id) |>
    pivot_longer(c(race, sex), names_to = "var", values_drop_na = TRUE) |>
    summarise(n_charges = n(), recency = min(recency), .by = c(person_id, var, value)) |>
    arrange(person_id, var, value == "Other or unknown", desc(n_charges), recency) |>
    mutate(n_values = n(),
           tied     = n_values > 1 & n_charges[1] == n_charges[2] & value[2] != "Other or unknown",
           .by = c(person_id, var)) |>
    distinct(person_id, var, .keep_all = TRUE)
}
