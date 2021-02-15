library(haven)
library(data.table)
library(sdcLog)

path_dta <- list.files(
    pattern = "nobsreg5xmpl.dta", full.names = TRUE, recursive = TRUE
)
dt <- setDT(read_dta(path_dta))

# with and without global idlist
options(sdc.id_var = "id")
options(sdc.info_level = 2L)

dt[, year := as.factor(year)]
reg_1a <- lm(y ~ x + z + dum_2ids + dum_5ids + year, data = dt)
summary(reg_1a)

(res <- sdc_model(dt, reg_1a))

dt[, dum_2ids_lgl := as.logical(dum_2ids)]
reg_1b <- lm(y ~ x + z + dum_2ids_lgl + dum_5ids + year, data = dt)
summary(reg_1b)

(res <- sdc_model(dt, reg_1b))

lapply(c(id = "id", idtest = "idtest"), function(x) sdc_model(dt, reg_1, id_var = x))


## How to get results
## HANDLING ZEROS in sdc_model
setDT(broom::tidy(reg_1))[][!(term %in% c("z", "dum_2ids", "year2002"))]


## Variablelists
reg_2 <- lm(y ~ x + z + dum_2ids + dum_5ids + year*land, data = dt)
summary(reg_2)
(res <- sdc_model(dt, reg_2))
as.data.table(res$variables$land)
nobs(reg_2)
lapply(list(reg1 = reg_1, reg2 = reg_2), broom::tidy, .id = "reg")

dt[, CH := fifelse(land == "CH", TRUE, FALSE)]
reg_3 <- lm(y ~ x + year * CH * z, data = dt)
summary(reg_3)
(res <- sdc_model(dt, reg_3))


reg_4 <- lm(y ~ x + year:z + CH + year:CH:z, data = dt)
summary(reg_4)
sdc_model(dt, reg_4)

reg_5 <- lm(y ~ x + year:CH:z, data = dt)
summary(reg_5)
sdc_model(dt, reg_5)


dt[, lag_z := shift(z), by = c("id", "land")]
# model specification not identical to Stata
reg_6 <- lm(
    y ~ x + z + dum_2ids + dum_5ids + id1 + year + lag_z + 0,
    data = dt
)

summary(reg_6)
sdc_model(dt, reg_6)

dt[, (paste0("lag_z", 1:3)) := shift(z, n = 1:3), by = c("id", "land")]

reg_7 <- lm(
    y ~ x + z + lag_z1 + lag_z2 + lag_z3 + id1 + year,
    data = dt
)
summary(reg_7)
sdc_model(dt, reg_7)


reg_8 <- lm(
    y ~ z + dum_2ids + dum_5ids + year * x,
    data = dt
)
summary(reg_8)
options(sdc.info_level = 1L)
sdc_model(dt, reg_8)

