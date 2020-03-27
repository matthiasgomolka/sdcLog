## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)
options(datatable.print.class      = FALSE)
options(datatable.print.keys       = FALSE)
options(datatable.print.trunc.cols = FALSE)

options(sdc.n_ids = 5L)
options(sdc.n_dominance = 2L)
options(sdc.share_dominance = 0.85)

## ----label, options-----------------------------------------------------------
library(sdcLog)
df <- data.frame(id = LETTERS[1:3], v1 = 1L:3L, v2 = 0L:2L)
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

## ----reset_options------------------------------------------------------------
options(sdc.n_ids_dominance = 2L)

## ----set_sdc.share_dominance--------------------------------------------------
options(sdc.share_dominance = 0.8)

## ----example1_sdc.share_dominance---------------------------------------------
sdc_descriptives(data = df, id_var = "id", val_var = "v1")

