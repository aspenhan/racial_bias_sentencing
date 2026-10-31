# ==============================================================================
# 01_merging_ids_cc_sc.R
#
# Replicates Cecile code/replication/01_cleaning/courts/merging_ids_sg_cc.do,
# which links circuit-court (CC) defendants to Sentencing Commission (SC)
# defendants through the fastLink person match. Added to the SC
# variables merged from final_wage_edu_analysis:
#   sc_id                        SC record id (one record = one sentencing event)
#   sc_off1date - sc_off10date   dates of the primary and additional offenses on
#                                the guidelines worksheet (sc_offensedate is the
#                                earliest of them)
#   sc_runningvariable           guidelines score minus (cutoff + 1)
#   sc_offenses                  guidelines offense group of the primary offense
#   sc_circno                    judicial circuit of the SC event
#
# Inputs:  orig_dir/pii/unique_identifiers/unique_identifiers_sc_cc_mi_<1-12>_9521.csv
#          orig_dir/pii/sc_cc_ids_cw.dta
#          orig_dir/final_wage_edu_analysis.dta
# Outputs: data_dir/cc_sc_id_xwalk.csv   CC person_id and SC id/Ted_ID -> new_ID
#          data_dir/cc_sc_for_merge.csv  one row per CC person_id x SC sentencing event
# ==============================================================================

# ---- FastLink ids, by birth month: new_ID = "<month>_<dedupe id>" --------------
# Each grp corresponds to a unique standardised (first name, last name, middle name or initial, birth month)
# for each person (person_id for CC and Ted_ID for SC)

# grps are then matched with each other using FastLink:
# exact match on birth month, then fuzzy match on names (Jaro-Winkler score),
# and matches are assigned the same new_ID
# id assigned within a birth month and then concatenated to get new_ID

# Note matching is not transitive:
# if A matches B, and B matches C, but A does not match C, all three will not be assigned the same id

fastlink_ids <- map(1:12, \(month) {
  read_csv(file.path(orig_dir, "pii", "unique_identifiers",
                     str_glue("unique_identifiers_sc_cc_mi_{month}_9521.csv")),
           col_types = cols_only(grp = col_double(), dedupe.ids = col_double())) |>
    transmute(grp, new_ID = str_c(month, "_", as.integer(dedupe.ids)))
}) |>
  list_rbind()
stopifnot(!anyDuplicated(fastlink_ids$grp))

# ---- Crosswalk: CC person_id and SC id/Ted_ID -> new_ID ------------------------
# sc_cc_ids_cw.dta holds CC person_ids and SC ids + Ted_IDs with their respective grps,
# so we want to match in the FastLink new_IDs to get the matched CC and SC ids

cw <- read_dta(file.path(orig_dir, "pii", "sc_cc_ids_cw.dta"),
               col_select = c(person_id, id, Ted_ID, grp)) |>
  mutate(Ted_ID = na_if(Ted_ID, ""))
stopifnot(all(fastlink_ids$grp %in% cw$grp))

xwalk <- cw |>
  left_join(fastlink_ids, by = "grp", relationship = "many-to-one") |>
  distinct(person_id, id, Ted_ID, new_ID)

write_csv(xwalk, file.path(data_dir, "cc_sc_id_xwalk.csv"))
cat("cc_sc_id_xwalk:", format(nrow(xwalk), big.mark = ","), "rows\n")

# ---- SC side: SC sentencing events and info by new_ID ------------
sc_ids <- xwalk |>
  filter(!is.na(id)) |>
  distinct(id, new_ID, Ted_ID)
cat("SC ids with missing new_ID:", sum(is.na(sc_ids$new_ID)),
    "| unique Ted_ID:", n_distinct(sc_ids$Ted_ID), "| unique new_ID:", n_distinct(sc_ids$new_ID), "\n")

sc_vars <- c("sentdate", "offensedate", str_c("off", 1:10, "date"),
             "priorcon_a", "prior_incar", "runningvariable", "offenses", "circno")

sc_full <- read_dta(file.path(orig_dir, "final_wage_edu_analysis.dta"),
               col_select = all_of(c("id", "Ted_ID", sc_vars))) |>
  mutate(Ted_ID = na_if(Ted_ID, ""))
stopifnot(nrow(anti_join(sc_full, sc_ids, by = c("id", "Ted_ID"))) == 0,   # every SC record has an id
          !anyDuplicated(sc_full$id))

sc <- sc_ids |>
  inner_join(sc_full, by = c("id", "Ted_ID"), relationship = "one-to-one") |>
  mutate(across(c(prior_incar, priorcon_a), \(x) if_else(x > 0, 1, x))) |>
  rename_with(\(x) str_c("sc_", x), all_of(sc_vars)) |>
  filter(!is.na(new_ID)) |>
  rename(sc_id = id)

# ---- CC side joined to the SC events of the same new_ID -------------------------
cc <- xwalk |>
  filter(!is.na(person_id)) |>
  distinct(person_id, new_ID)
cat("CC persons with missing new_ID:", sum(is.na(cc$new_ID)),
    "| unique person_id:", n_distinct(cc$person_id), "| unique new_ID:", n_distinct(cc$new_ID), "\n")

sc_for_merge <- cc |>
  filter(!is.na(new_ID)) |>
  inner_join(sc, by = "new_ID", relationship = "many-to-many")

write_csv(sc_for_merge, file.path(data_dir, "cc_sc_for_merge.csv"))

cat("cc_sc_for_merge:", format(nrow(sc_for_merge), big.mark = ","), "rows,",
    format(n_distinct(sc_for_merge$person_id), big.mark = ","), "CC persons\n",
    format(n_distinct(sc_for_merge$Ted_ID), big.mark = ","), "SC persons\n")
