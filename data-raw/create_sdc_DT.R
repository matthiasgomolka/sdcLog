library(data.table)
set.seed(1)

# generate same test_dt data as in nobsdes
sdc_DT <- as.data.table(replicate(n = 3, runif(n = 100)))
sdc_DT[, `:=`(
    id   = .I %/% 10L + 1L,
    time = .I %% 10L + 1L,
    D1   = .I %in% c(5L, 57L),
    D2   = .I %in% c(5L, 57L, 78L)
)]

sdc_DT[id == 11L, V2 := NA]
sdc_DT[id != 1L , V3 := 0L]
sdc_DT[, D3 := fifelse(id == 1L, TRUE, FALSE)]

setcolorder(sdc_DT, c("id", "time"))

# save for further usage in test_dts

usethis::use_data(sdc_DT, overwrite = TRUE)

sdc_DT <- data.table(
    id      = c(sort(rep_len(1L:10L, 300L)), 11L),
    year    = rep_len(2001L:2010L, 301L),
    country = rep_len(sort(rep.int(LETTERS[1:3], 10)), 301L),
    x       = c(runif(n = 300L), NA_real_)
)
sdc_DT[id > 2 & year == 2002L, x := NA_real_]
sdc_DT[id == 1L                                 , x_dom :=  5L] # use fcase when available
sdc_DT[id == 2L                                 , x_dom :=  4L]
sdc_DT[id == 2L & country == "C"                , x_dom := -4L]
sdc_DT[id == 3L                                 , x_dom :=  1L]
sdc_DT[id == 3L & country == "B" & year >= 2009L, x_dom := 10L]

usethis::use_data(sdc_DT, overwrite = TRUE)
