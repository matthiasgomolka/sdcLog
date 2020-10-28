## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE-----------------------------------------------------------
#library(sdcLog)
devtools::load_all()
library(data.table)

## -----------------------------------------------------------------------------
data("sdc_descriptives_DT")
sdc_descriptives_DT

## -----------------------------------------------------------------------------
#library(sdcLog)
devtools::load_all()
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val")


## -----------------------------------------------------------------------------
#library(sdcLog)
devtools::load_all()
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val", by = sector)


## -----------------------------------------------------------------------------
#library(sdcLog)
devtools::load_all()
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val", by = c("sector", "year"))


## -----------------------------------------------------------------------------
#library(sdcLog)
devtools::load_all()
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1", by = c("sector", "year"))


## -----------------------------------------------------------------------------
#library(sdcLog)
devtools::load_all()
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1", by = c("sector", "year"), zero_as_NA = FALSE)


## -----------------------------------------------------------------------------
data("sdc_extreme_DT")
sdc_extreme_DT

## -----------------------------------------------------------------------------
devtools::load_all()
sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_1")


## -----------------------------------------------------------------------------
devtools::load_all()
sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_2")


## -----------------------------------------------------------------------------
devtools::load_all()
sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_2", n_min = 7)


## -----------------------------------------------------------------------------
devtools::load_all()
sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_3", n_min = 10, n_max = 10)


## -----------------------------------------------------------------------------
devtools::load_all()
sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_3", n_min = 8, n_max = 8)


## -----------------------------------------------------------------------------
devtools::load_all()
sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1", by = year)
sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1", by = sector)


## -----------------------------------------------------------------------------
devtools::load_all()
res <- sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1", by = .(sector, year))

res

## -----------------------------------------------------------------------------
data("sdc_model_DT")
summary(sdc_model_DT)

## -----------------------------------------------------------------------------

model_1 <- lm(y ~ x_1 + x_2, data = sdc_model_DT)
model_2 <- lm(y ~ x_1 + x_2 + x_3, data = sdc_model_DT)
model_3 <- lm(y ~ x_1 + x_2 + x_4, data = sdc_model_DT)
model_4 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = sdc_model_DT)
model_5 <- lm(y ~ x_1 + x_2 + dummy_3, data = sdc_model_DT)


## -----------------------------------------------------------------------------
devtools::load_all()
sdc_model(data = sdc_model_DT, model = model_1, id_var = "id")

sdc_model(data = sdc_model_DT, model = model_4, id_var = "id")


## -----------------------------------------------------------------------------
devtools::load_all()
sdc_model(data = sdc_model_DT, model = model_2, id_var = "id")

sdc_model(data = sdc_model_DT, model = model_3, id_var = "id")


## -----------------------------------------------------------------------------
devtools::load_all()

options(sdc.info_level = 2)
sdc_model(data = sdc_model_DT, model = model_5, id_var = "id")


