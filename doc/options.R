## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)

user_options <- options()

options(datatable.print.class = FALSE)
options(datatable.print.keys = FALSE)
options(datatable.print.trunc.cols = FALSE)

options(sdc.n_ids = 5L)
options(sdc.n_dominance = 2L)
options(sdc.share_dominance = 0.85)

## ----label, options-----------------------------------------------------------
library(sdcLog)
df <- data.frame(id = LETTERS[1:3], v1 = 1L:3L, v2 = c(1L, 2L, 4L))
df

## ----example1_sdc.n_ids-------------------------------------------------------
sdc_descriptives(data = df, id_var = "id", val_var = "v1")

## ----set_sdc.n_ids------------------------------------------------------------
options(sdc.n_ids = 3)

## ----example2_sdc.n_ids-------------------------------------------------------
sdc_descriptives(data = df, id_var = "id", val_var = "v1")

## ----example1_sdc.n_ids_dominance---------------------------------------------
sdc_descriptives(data = df, id_var = "id", val_var = "v2")

## ----set_sdc.n_ids_dominance--------------------------------------------------
options(sdc.n_ids_dominance = 1)

## ----example2_sdc.n_ids_dominance---------------------------------------------
sdc_descriptives(data = df, id_var = "id", val_var = "v2")

## ----reset_options1, include=FALSE--------------------------------------------
options(sdc.n_ids_dominance = 2L)

## ----set_sdc.share_dominance--------------------------------------------------
options(sdc.share_dominance = 0.8)

## ----example1_sdc.share_dominance---------------------------------------------
sdc_descriptives(data = df, id_var = "id", val_var = "v1")

## ----reset_options2, include=FALSE--------------------------------------------
options(sdc.share_dominance = 0.85)

## ----example_sdc.info_level---------------------------------------------------
for (i in 0:2) {
  options(sdc.info_level = i)
  cat("\nsdc.info_level: ", getOption("sdc.info_level"), "\n")
  print(sdc_descriptives(data = df, id_var = "id", val_var = "v1"))
}

## ----sdc.id_var---------------------------------------------------------------
options(sdc.id_var = "id")

## ----reset options,echo=-1----------------------------------------------------
options(user_options)
sdc_descriptives(data = df, val_var = "v1")

