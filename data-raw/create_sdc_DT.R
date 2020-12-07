library(data.table)
set.seed(1)

# generate same test_dt data as in nobsdes
sdc_DT <- as.data.table(replicate(n = 3, runif(n = 100)))
sdc_DT[, `:=`(
  id = .I %/% 10L + 1L,
  time = .I %% 10L + 1L,
  D1 = .I %in% c(5L, 57L),
  D2 = .I %in% c(5L, 57L, 78L)
)]

sdc_DT[id == 11L, V2 := NA]
sdc_DT[id != 1L, V3 := 0L]
sdc_DT[, D3 := fifelse(id == 1L, TRUE, FALSE)]

setcolorder(sdc_DT, c("id", "time"))

# save for further usage in test_dts

usethis::use_data(sdc_DT, overwrite = TRUE)

sdc_DT <- data.table(
  id = c(sort(rep_len(1L:10L, 300L)), 11L),
  year = rep_len(2001L:2010L, 301L),
  country = rep_len(sort(rep.int(LETTERS[1:3], 10)), 301L),
  x = c(runif(n = 300L), NA_real_)
)
sdc_DT[id > 2 & year == 2002L, x := NA_real_]
sdc_DT[id == 1L, x_dom := 5L] # use fcase when available
sdc_DT[id == 2L, x_dom := 4L]
sdc_DT[id == 2L & country == "C", x_dom := -4L]
sdc_DT[id == 3L, x_dom := 1L]
sdc_DT[id == 3L & country == "B" & year >= 2009L, x_dom := 10L]

usethis::use_data(sdc_DT, overwrite = TRUE)

# dt for descriptives
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
data.table::setcolorder(sdc_descriptives_DT, c("id", "sector", "year"))

usethis::use_data(sdc_descriptives_DT, overwrite = TRUE)

# dt for extreme
set.seed(1)
n <- 20L
sdc_extreme_DT <- data.table::data.table(
  id = as.factor(rep_len(LETTERS[1L:10L], n)),
  sector = as.factor(sort(rep_len(paste0("S", 1L:2L), n))),
  year = rep_len(2019L:2020L, n),
  val_1 = 20L:1L,
  val_2 = c(200L, 190L, 18L:1L),
  val_3 = c(NA_integer_, 19L:1L),
  key = "id"
)
data.table::setorder(sdc_extreme_DT, -val_1)
usethis::use_data(sdc_extreme_DT, overwrite = TRUE)

# dt for model
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
  dummy_4 = as.factor("W"),
  key = "id"
)

# create problems id's for x_3
# sdc_model_DT[id %chin% c("A", "B", "C", "D", "E", "F"), x_3 := NA_real_]
sdc_model_DT[id %in% c("A", "B", "C", "D", "E", "F"), x_3 := NA_real_]

# create problems dominance for x_4
# sdc_model_DT[id %chin% c("A", "B"), x_4 := x_4 * 100]
sdc_model_DT[id %in% c("A", "B"), x_4 := x_4 * 100]

# create problem with interaction for dummmy_4 (dummy_2:dummy_4 interaction)
sdc_model_DT[id %in% c("A", "C", "E", "G", "J"), dummy_4 := "M"]

usethis::use_data(sdc_model_DT, overwrite = TRUE)
