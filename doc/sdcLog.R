## ----setup, include = FALSE------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>" # ,
  # tidy = TRUE,
  # tidy.opts = list(
  #   indent = 2L,
  #   width.cutoff = 95L,
  #   wrap = TRUE
  # )
)

user_options <- options()
options(width = 95)
options(knitr.kable.NA = "")
options(sdc.info_level = 1L)
options(datatable.print.keys = FALSE)
options(datatable.print.class = FALSE)

library(sdcLog)
library(knitr)
library(skimr)

## ----test_data_descriptives------------------------------------------------------------------
data("sdc_descriptives_DT")
sdc_descriptives_DT

## ----descriptives_simple_case----------------------------------------------------------------
sdc_descriptives_DT[, .(mean = mean(val_1, na.rm = TRUE))]

## ----descriptives_simple---------------------------------------------------------------------
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1")

## ----descriptives_by_case--------------------------------------------------------------------
sdc_descriptives_DT[, .(mean = mean(val_1, na.rm = TRUE)), by = "sector"]

## ---- descriptives_by------------------------------------------------------------------------
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1", by = "sector")

## ----descriptives_byby_case------------------------------------------------------------------
sdc_descriptives_DT[, .(mean = mean(val_1, na.rm = TRUE)), by = c("sector", "year")]

## ----descriptives_byby-----------------------------------------------------------------------
sdc_descriptives(
  data = sdc_descriptives_DT,
  id_var = "id",
  val_var = "val_1",
  by = c("sector", "year")
)

## ----descriptives_zero_case------------------------------------------------------------------
sdc_descriptives_DT[, .(mean = mean(val_2, na.rm = TRUE)), by = c("sector", "year")]

## ----descriptives_zero-----------------------------------------------------------------------
sdc_descriptives(
  data = sdc_descriptives_DT,
  id_var = "id",
  val_var = "val_2",
  by = c("sector", "year")
)

## ----descriptives_zerozero-------------------------------------------------------------------
sdc_descriptives(
  data = sdc_descriptives_DT,
  id_var = "id",
  val_var = "val_2",
  by = c("sector", "year"),
  zero_as_NA = FALSE
)

## ----test_data_extreme-----------------------------------------------------------------------
data("sdc_min_max_DT")
sdc_min_max_DT

## ----extreme_simple--------------------------------------------------------------------------
sdc_min_max(data = sdc_min_max_DT, id_var = "id", val_var = "val_1")

## ----extreme_n1------------------------------------------------------------------------------
sdc_min_max(data = sdc_min_max_DT, id_var = "id", val_var = "val_2")

## ----extreme_n2------------------------------------------------------------------------------
sdc_min_max(data = sdc_min_max_DT, id_var = "id", val_var = "val_2", max_obs = 5)

## ----exterme_by1-----------------------------------------------------------------------------
sdc_min_max(data = sdc_min_max_DT, id_var = "id", val_var = "val_1", by = "year")

sdc_min_max(data = sdc_min_max_DT, id_var = "id", val_var = "val_1", by = "sector")

## --------------------------------------------------------------------------------------------
res <- sdc_min_max(
  data = sdc_min_max_DT,
  id_var = "id",
  val_var = "val_1",
  by = c("sector", "year")
)

## ----extreme_by3-----------------------------------------------------------------------------
# extreme_vals
res

## ----model_data------------------------------------------------------------------------------
data("sdc_model_DT")
print(skim(sdc_model_DT))

## ----model_models----------------------------------------------------------------------------
model_1 <- lm(y ~ x_1 + x_2, data = sdc_model_DT)
model_2 <- lm(y ~ x_1 + x_2 + x_3, data = sdc_model_DT)
model_3 <- lm(y ~ x_1 + x_2 + dummy_1 * dummy_2, data = sdc_model_DT)
model_4 <- lm(y ~ x_1 + x_2 + dummy_1 * dummy_3, data = sdc_model_DT)

## ----model_simple----------------------------------------------------------------------------
sdc_model(data = sdc_model_DT, model = model_1, id_var = "id")

sdc_model(data = sdc_model_DT, model = model_3, id_var = "id")

## ----model_prob1-----------------------------------------------------------------------------
sdc_model(data = sdc_model_DT, model = model_2, id_var = "id")

## ----model_prob2-----------------------------------------------------------------------------
sdc_model(data = sdc_model_DT, model = model_4, id_var = "id")

## ----eval = FALSE----------------------------------------------------------------------------
#  sdc_log(
#    r_scripts = "/home/my_project/R/my_script.R",
#    log_files = "/home/my_project/log/my_script.txt"
#  )

## ----reset options, include=FALSE---------------------------------------------
options(user_options)

