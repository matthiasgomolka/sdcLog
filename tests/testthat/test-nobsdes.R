# library(haven)
# library(data.table)
# library(sdcLog)
#
# path_dta <- list.files(
#     pattern = "nobsdes5xmpl.dta", full.names = TRUE, recursive = TRUE
# )
# dt <- setDT(read_dta(path_dta))
#
# # 2. Applying nobsdes2
# ## NOBSDES2 without options
# options(sdc.info_level = 2L)
# sdc_descriptives(dt, "id")
# sdc_descriptives(dt, "id", "x")
#
#
# ## How to use the BY - option
# res <- sdc_descriptives(dt, "id", "x", by = "year")
# sdc_descriptives(dt, "id", "x", by = "year")
#
# cube(dt[year != 2002], j = .(`sum(x)` = sum(x, na.rm = TRUE)), by = "year")
#
#
# dt[, year_bucket := fifelse(year %% 2L == 1L, year + 1L, year)
# ][, .(`sum(x)` = sum(x, na.rm = TRUE)), keyby = year_bucket]
#
# sdc_descriptives(dt, "id", "x", by = year_bucket)
#
#
# ## GEN - option
# res <- sdc_descriptives(dt, "id", "x", by = year)
# merge(
#     as.data.table(res$distinct_ids[distinct_ids >= 5]),
#     dt[,.(`sum(x)` = sum(x, na.rm = TRUE)), keyby = year],
#     by = "year"
# )
#
#
# ## BYEST - option
# res <- sdc_descriptives(
#     dt,
#     "id", "xpc",
#     by = cut(xpc, breaks = quantile(xpc, seq(0, 1, 0.1), na.rm = TRUE, type = 6L))
# )
# na.omit(as.data.table(merge(res$distinct_ids, res$dominance, by = "cut"))[order(cut)])
#
#
# ## ZEROS
# sdc_descriptives(dt, "id", "xwithzeros", by = "year")
# sdc_descriptives(dt, "id", "xwithzeros", by = "year", zero_as_NA = FALSE)
# as.data.table(res$distinct_ids)
#
#
# ## DOMINANCE
# DT <- cube(dt[!is.na(xfordom)], j = sum(xfordom), by = c("land", "id"), id = TRUE)
# dcast(DT[grouping <= 1], id ~ land, value.var = "V1")[order(id)]
#
# sdc_descriptives(dt, "id", "xfordom", by = "land")
#
#
# ## MAXIMUM and MINIMUM
# sdc_extreme(dt, "id", "x")
#
# ## PERCENTILES
# # throw useful error if expression not encapsulated in {}
# sdc_descriptives(
#     dt[year == 2010],
#     id_var = "id",
#     val_var = "xpc",
#     by = .(cut(xpc, quantile(xpc, probs = c(.05, .50, .90), type = 6, na.rm = TRUE)))
# )
#
# sdc_descriptives(
#     dt,
#     id_var = "id",
#     val_var = "xpc",
#     by = .(year, {cut(xpc, quantile(xpc, probs = c(.05, .50, .90), type = 6, na.rm = TRUE))})
# )
