## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(knitr.kable.NA = "")
options(sdc.info_level = 1L)
options(datatable.print.keys = FALSE)
options(datatable.print.class = FALSE)

library(sdcLog)
library(knitr)
library(skimr)

## ----test_data_descriptives---------------------------------------------------
data("sdc_descriptives_DT")
sdc_descriptives_DT

## ----descriptives_simple_case-------------------------------------------------
sdc_descriptives_DT[, .(mean = mean(val_1, na.rm = TRUE))]

## ----descriptives_simple------------------------------------------------------
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1")

## ----descriptives_by_case-----------------------------------------------------
sdc_descriptives_DT[, .(mean = mean(val_1, na.rm = TRUE)), by = "sector"]

## ---- descriptives_by---------------------------------------------------------
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1", by = "sector")

## ----descriptives_byby_case---------------------------------------------------
sdc_descriptives_DT[, .(mean = mean(val_1, na.rm = TRUE)), by = c("sector", "year")]

## ----descriptives_byby--------------------------------------------------------
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1",
                 by = c("sector", "year"))

## ----descriptives_zero_case---------------------------------------------------
sdc_descriptives_DT[, .(mean = mean(val_2, na.rm = TRUE)), by = c("sector", "year")]

## ----descriptives_zero--------------------------------------------------------
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_2", 
                 by = c("sector", "year"))

## ----descriptives_zerozero----------------------------------------------------
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_2", 
                 by = c("sector", "year"), zero_as_NA = FALSE)

## ----model_data---------------------------------------------------------------
data("sdc_model_DT")
print(skim(sdc_model_DT))

## ----model_models-------------------------------------------------------------
model_1 <- lm(y ~ x_1 + x_2, data = sdc_model_DT)
model_2 <- lm(y ~ x_1 + x_2 + x_3, data = sdc_model_DT)
model_3 <- lm(y ~ x_1 + x_2 + x_4, data = sdc_model_DT)
model_4 <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = sdc_model_DT)
model_5 <- lm(y ~ x_1 + x_2 + dummy_3, data = sdc_model_DT)

## ----model_simple-------------------------------------------------------------
sdc_model(data = sdc_model_DT, model = model_1, id_var = "id")

sdc_model(data = sdc_model_DT, model = model_4, id_var = "id")

## ----model_prob1--------------------------------------------------------------
sdc_model(data = sdc_model_DT, model = model_2, id_var = "id")

sdc_model(data = sdc_model_DT, model = model_3, id_var = "id")

sdc_model(data = sdc_model_DT, model = model_5, id_var = "id")

## ----test_data_extreme--------------------------------------------------------
data("sdc_extreme_DT")
sdc_extreme_DT

## ----extreme_simple-----------------------------------------------------------
sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1")

## ----extreme_n1---------------------------------------------------------------
sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_2")

## ----extreme_n2---------------------------------------------------------------
sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_2", n_min = 7)

## ----extreme_n3---------------------------------------------------------------
sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_3", n_min = 10, n_max = 10)

## -----------------------------------------------------------------------------
sdc_extreme(data = sdc_extreme_DT, id = "id", val_var = "val_3", n_min = 8, n_max = 8)

## ----exterme_by1--------------------------------------------------------------
sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1", by = "year")

sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1", by = "sector")

## ----extreme_by2--------------------------------------------------------------
extreme_vals <- sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1", 
                            by = c("sector", "year"))

## ----extreme_by3--------------------------------------------------------------
extreme_vals

## ----eval = FALSE-------------------------------------------------------------
#  sdc_log(r_scripts = "C:/Beispielprojekt/r_scripts/sdc_log_example.R",
#          log_files = "C:/Beispielprojekt/sdc_log/log_file.txt")

## ----return_descriptives1-----------------------------------------------------
typeof(sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1"))

## ----return_descriptives2-----------------------------------------------------
result_sdc_descriptives <- sdc_descriptives(
  data = sdc_descriptives_DT, id_var = "id", val_var = "val_1"
)

result_sdc_descriptives <- sdc_descriptives(
  data = sdc_descriptives_DT, id_var = "id", val_var = "val_1", by = c("sector", "year")
)

## ----return_descriptives3-----------------------------------------------------
result_sdc_descriptives <- sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1")

result_sdc_descriptives

sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1")

## ----return_descriptives4-----------------------------------------------------
result_sdc_descriptives <- sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1")

class(result_sdc_descriptives)

result_sdc_descriptives

str(result_sdc_descriptives)

## ----return_model-------------------------------------------------------------
model <- lm(y ~ x_1 + x_2 + dummy_1 + dummy_2, data = sdc_model_DT)

result_sdc_model <- sdc_model(data = sdc_model_DT, model = model, id_var = "id")

class(result_sdc_model)

result_sdc_model

str(result_sdc_model)

## ----return_extreme1----------------------------------------------------------
result_sdc_extreme <- sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1")

class(result_sdc_extreme)

result_sdc_extreme

str(result_sdc_extreme)

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


