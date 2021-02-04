# skip({
#     library(haven)
#     library(data.table)
#
#     path_dta <- list.files(
#         pattern = "nobsdes2xmpl.dta", full.names = TRUE, recursive = TRUE
#     )
#     dt <- setDT(read_dta(path_dta))
#
#     # 2. Applying nobsdes2
#     ## NOBSDES2 without options
#     options(sdc.info_level = 2L)
#     sdc_descriptives(dt, "id")
#     sdc_descriptives(dt, "id", "x")
#
#     # How to use the BY - option
#     sdc_descriptives(dt, "id", "x", by = "year")
#
#     dt[year != 2002, .(`sum(x)` = sum(x, na.rm = TRUE)), keyby = year]
#
#     dt[
#         , year_bucket := fifelse(year %% 2L == 1L, year + 1L, year)
#     ][
#         , .(`sum(x)` = sum(x, na.rm = TRUE)), keyby = year_bucket
#     ]
#     sdc_descriptives(dt, "id", "x", by = year_bucket)
#
#     res <- sdc_descriptives(dt, "id", "x", by = year)
#     as.data.table(merge(
#         res$distinct_ids,
#         dt[year != 2002, .(`sum(x)` = sum(x, na.rm = TRUE)), keyby = year],
#         by = "year"
#     ))
#
#     # BYEST - option
#     res <- sdc_descriptives(
#         dt[, xpc_buckets := cut(xpc, breaks = quantile(xpc, seq(0, 1, 0.1), na.rm = TRUE, type = 6L))],
#         "id", "xpc", by = "xpc_buckets"
#     )
#     na.omit(as.data.table(merge(res$distinct_ids, res$dominance, by = "xpc_buckets"))[order(xpc_buckets)])
#
#     res <- sdc_descriptives(
#         dt,
#         "id", "xpc",
#         by = cut(xpc, breaks = quantile(xpc, seq(0, 1, 0.1), na.rm = TRUE, type = 6L))
#     )
#     na.omit(as.data.table(merge(res$distinct_ids, res$dominance, by = "cut"))[order(cut)])
#
#     sdc_descriptives(
#         dt,
#         "id", "x",
#         by = year
#     )
#     sdc_descriptives(
#         dt,
#         "id", "x",
#         by = .(year)
#     )
#     sdc_descriptives(
#         dt,
#         "id", "x",
#         by = "year"
#     )
#
#     sdc_descriptives(
#         dt,
#         "id", "x",
#         by = .(year, land)
#     )
#     sdc_descriptives(
#         dt,
#         "id", "x",
#         by = c("year", "land")
#     )
#     sdc_descriptives(
#         dt,
#         "id", "x",
#         by = "year,land"
#     )
#
#     sdc_descriptives(
#         dt,
#         "id", "xpc",
#         by = cut(xpc, breaks = quantile(xpc, seq(0, 1, 0.1), na.rm = TRUE, type = 6L))
#     )
#
# })
