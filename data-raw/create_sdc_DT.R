library(data.table)

# dt for descriptives ----
set.seed(1)
n <- 20L
sdc_descriptives_DT <- data.table::data.table(
  id = as.factor(rep_len(LETTERS[1L:10L], n)),
  year = sort(rep_len(2019L:2020L, n)),
  val_1 = runif(n, min = 1, max = 10),
  val_2 = c(runif(13, min = 2, max = 10), rep(0, 4), runif(3, min = 2, max = 10)),
  key = "id"
)
sdc_descriptives_DT[, sector := as.factor(sort(rep_len(paste0("S", 1L:2L), n)))]
sdc_descriptives_DT[id == "A" & year == 2019L, val_1 := NA_real_]
sdc_descriptives_DT[id %in% c("A", "F") & year == 2020L, val_1 := val_1 * 33]
sdc_descriptives_DT[!(id %in% c("A", "F")), id_na := id]
data.table::setcolorder(sdc_descriptives_DT, c("id", "id_na", "sector", "year"))

usethis::use_data(sdc_descriptives_DT, overwrite = TRUE)

# dt for extreme ----
set.seed(1)
n <- 20L
sdc_min_max_DT <- data.table::data.table(
  id = as.factor(rep_len(LETTERS[1L:10L], n)),
  sector = as.factor(sort(rep_len(paste0("S", 1L:2L), n))),
  year = rep_len(2019L:2020L, n),
  val_1 = 20L:1L,
  val_2 = c(200L, 190L, 18L:1L),
  val_3 = c(NA_integer_, 19L:1L),
  key = "id"
)
data.table::setorder(sdc_min_max_DT, -val_1)
usethis::use_data(sdc_min_max_DT, overwrite = TRUE)

# dt for model ----
set.seed(1)
n <- 80
y <- rnorm(n, mean = 120, sd = 8)
sdc_model_DT <- data.table::data.table(
  id = as.factor(rep_len(LETTERS[1L:10L], n)),
  y = y,
  x_1 = jitter(y, factor = 10000),
  x_2 = jitter(y, factor = 15000),
  x_3 = jitter(y, factor = 50000),
  x_4 = jitter(y, factor = 60000),
  dummy_1 = as.factor(sort(rep_len(paste0("S", 1L:2L), n))),
  dummy_2 = as.factor(rep_len(paste0("Y", 1L:8L), n)),
  dummy_3 = as.factor(c(rep(rawToChar(as.raw(c(66, 69))), n / 4), rep(rawToChar(as.raw(68:69)), n / 4), rep(rawToChar(as.raw(c(69, 83))), 36), rep(rawToChar(as.raw(c(70, 82))), 4))),
  key = "id"
)

# create problems id's for x_3
# sdc_model_DT[id %chin% c("A", "B", "C", "D", "E", "F"), x_3 := NA_real_]
sdc_model_DT[id %in% c("A", "B", "C", "D", "E", "F"), x_3 := NA_real_]

# create problems dominance for x_4
# sdc_model_DT[id %chin% c("A", "B"), x_4 := x_4 * 100]
sdc_model_DT[id %in% c("A", "B"), x_4 := x_4 * 100]

usethis::use_data(sdc_model_DT, overwrite = TRUE)


# dt for dup obs ----
sdc_dups_credits_DT <- data.table(
    lender = paste0("L", 1:6),
    borrower = paste0("B", 1:6),
    volume = c(400L:401L, 10L:12L, 99L)
)
sdc_dups_lender_groups_DT <- data.table(
    lender_group = paste0("LG", 1:7),
    lender = paste0("L", c(1:6, 6))
)
sdc_dups_borrower_groups_DT <- data.table(
    borrower_group = paste0("BG", 1:7),
    borrower = paste0("B", c(1:6, 6))
)

sdc_dups_DT <- merge(
    sdc_dups_lender_groups_DT,
    sdc_dups_credits_DT,
    by = "lender"
)
sdc_dups_DT <- merge(
    sdc_dups_DT,
    sdc_dups_borrower_groups_DT,
    by = "borrower"
)
setcolorder(sdc_dups_DT, c("lender_group", "lender", "volume", "borrower", "borrower_group"))

usethis::use_data(sdc_dups_DT, overwrite = TRUE)

