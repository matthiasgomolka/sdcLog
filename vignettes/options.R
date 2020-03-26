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

