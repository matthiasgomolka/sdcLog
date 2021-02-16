#
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
# dt[year != 2002, .(`sum(x)` = sum(x, na.rm = TRUE)), by = "year"][order(year)]
#
# dt[, year_bucket := fifelse(year %% 2L == 1L, year + 1L, year)
# ][, .(`sum(x)` = sum(x, na.rm = TRUE)), keyby = year_bucket]
#
# sdc_descriptives(dt, "id", "x", by = "year_bucket")
#
#
# ## GEN - option
# res <- sdc_descriptives(dt, "id", "x", by = "year")
# merge(
#     dt[,.(`sum(x)` = sum(x, na.rm = TRUE)), keyby = year],
#     merge(
#         as.data.table(res$distinct_ids[distinct_ids >= 5]),
#         as.data.table(res$dominance),
#         by = "year"
#     )
# )
#
#
# ## BYEST - option
# dt[, quantiles := cut(xpc, breaks = quantile(xpc, seq(0, 1, 0.1), na.rm = TRUE, type = 6L))]
# res <- sdc_descriptives(dt, "id", "xpc", by = "quantiles")
# na.omit(as.data.table(merge(res$distinct_ids, res$dominance))[order(quantiles)])
#
#
# ## ZEROS
# res <- sdc_descriptives(dt, "id", "xwithzeros", by = "year")
# res <- sdc_descriptives(dt, "id", "xwithzeros", by = "year", zero_as_NA = FALSE)
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
# sdc_min_max(dt, "id", "x")
#
# ## PERCENTILES
# # sdc_percentiles() would be helpful suppress percentile labels
# dt[year == 2010L, percentiles := cut(xpc, unique(quantile(xpc, probs = c(0, .05, .50, .90, 1), type = 6, na.rm = TRUE)))]
# res <- sdc_descriptives(dt[year == 2010], "id", val_var = "xpc", by = "percentiles")
# as.data.table(merge(res$distinct_ids, res$dominance))[order(percentiles)]
#
# dt[, percentiles := cut(xpc, unique(quantile(xpc, probs = c(0, .05, .50, .90, 1), type = 6, na.rm = TRUE)))]
# res <- sdc_descriptives(dt, "id", val_var = "xpc", by = "percentiles")
# as.data.table(merge(res$distinct_ids, res$dominance))[order(percentiles)]
#
# dt[, percentiles := cut(xpc, quantile(xpc, probs = seq(0, 1, .1), type = 6, na.rm = TRUE), right = FALSE,l)]
# res <- sdc_descriptives(dt, "id", "xpc", by = "percentiles")
#
#
# ## Creating GRAPHS
# options(sdc.id_var = "id")
# res <- sdc_descriptives(dt, val_var = "x", by = c("year", "land"))
# plot_dt <- as.data.table(merge(res$distinct_ids, res$dominance))[distinct_ids >= 5 & value_share <= 0.85]
# plot_dt <- merge(plot_dt, dt)
# plot(plot_dt[land == "BE", .(`mean(x)` = mean(x, na.rm = TRUE)), by = "year"])
#
#
# ## Creating HISTOGRAMS
# dt[, bins := cut(xpc, breaks = 10L)]
# sdc_descriptives(dt, val_var = "xpc", by = "bins")
# ggplot(na.omit(dt[, .N, by = bins])) +
#     aes(x = bins, y = N) +
#     geom_col()
#
#
# ## Aggregating Data
# ## not relevant for R
# }
