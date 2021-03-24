library(data.table)


# test print.sdc_distinct_ids ----
distinct_ids_1 <- structure(
    data.table(distinct_ids = 10L),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
)

test_that("print.sdc_distinct_ids works for most simple case", {
    options(sdc.info_level = 0L)
    expect_silent(print(distinct_ids_1))

    options(sdc.info_level = 1L)
    expect_silent(print(distinct_ids_1))

    options(sdc.info_level = 2L)
    expect_message(
        print(distinct_ids_1),
        "No problem with number of distinct entities (10).",
        fixed = TRUE
    )
})

distinct_ids_2 <- structure(
    data.table(distinct_ids = 3L),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
)

test_that("print.sdc_distinct_ids works for problematic case", {
    options(sdc.info_level = 0L)
    expect_output(
        print(distinct_ids_2),
        "Not enough distinct entities:",
        fixed = TRUE
    )

    options(sdc.info_level = 1L)
    expect_output(
        print(distinct_ids_2),
        "Not enough distinct entities:",
        fixed = TRUE
    )

    options(sdc.info_level = 2L)
    expect_output(
        print(distinct_ids_2),
        "Not enough distinct entities:",
        fixed = TRUE
    )
})

# needed for testing print.sdc_descriptives
distinct_ids_3 <- structure(
    data.table(sector = paste0("S", 1:2), distinct_ids = 3L:4L),
    class = c("sdc_distinct_ids", "data.table", "data.frame")
)

# test print.sdc_dominance ----
dominance_1 <- structure(
    data.table(value_share = numeric(0)),
    class = c("sdc_dominance", "data.table", "data.frame")
)

test_that("print.sdc_dominance works for most simple case", {
    options(sdc.info_level = 0L)
    expect_silent(print(dominance_1))

    options(sdc.info_level = 1L)
    expect_silent(print(dominance_1))

    options(sdc.info_level = 2L)
    expect_message(
        print(dominance_1),
        "No problem with dominance.",
        fixed = TRUE
    )
})

dominance_2 <- structure(
    data.table(value_share = 0.9),
    class = c("sdc_dominance", "data.table", "data.frame")
)

test_that("print.sdc_dominance works for problematic case", {
    options(sdc.info_level = 0L)
    expect_output(print(dominance_2), "Dominant entities:", fixed = TRUE)

    options(sdc.info_level = 1L)
    expect_output(print(dominance_2), "Dominant entities:", fixed = TRUE)

    options(sdc.info_level = 2L)
    expect_output(print(dominance_2), "Dominant entities:", fixed = TRUE)
})

# needed for testing print.sdc_descriptives
dominance_3 <- structure(
    data.table(sector = paste0("S", 1:2), value_share = c(0.88, 0.9)),
    class = c("sdc_dominance", "data.table", "data.frame")
)

dominance_4 <- structure(
    data.table(value_share = NA_real_),
    class = c("sdc_dominance", "data.table", "data.frame")
)

test_that("print.sdc_dominance works for val_var = NULL", {
    options(sdc.info_level = 0L)
    expect_silent(print(dominance_4))

    options(sdc.info_level = 1L)
    expect_silent(print(dominance_4))

    options(sdc.info_level = 2L)
    expect_message(
        print(dominance_4),
        "No dominance check conducted, because 'val_var = NULL'.",
        fixed = TRUE
    )
})


# test print.sdc_descriptives ----
descriptives_1 <- structure(
    list(
        options = list_options(),
        settings = list_arguments(id_var = "id", val_var = "val"),
        distinct_ids = distinct_ids_1,
        dominance = dominance_1
    ),
    class = c("sdc_descriptives", "list")
)

test_that("print.sdc_descriptives works for most simple case", {
    options(sdc.info_level = 0L)
    messages <- capture_messages(print(descriptives_1))
    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n")
    )

    options(sdc.info_level = 1L)
    messages <- capture_messages(print(descriptives_1))
    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "Output complies to RDC rules.\n")
    )

    options(sdc.info_level = 2L)
    messages <- capture_messages(print(descriptives_1))
    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "No problem with number of distinct entities (10).\n",
          "No problem with dominance.\n",
          "Output complies to RDC rules.\n")
    )
})

descriptives_2 <- structure(
    list(
        options = list_options(),
        settings = list_arguments(id_var = "id", val_var = "val"),
        distinct_ids = distinct_ids_2,
        dominance = dominance_2
    ),
    class = c("sdc_descriptives", "list")
)

expect_print.sdc_descriptives_2 <- function(x) {
    output <- capture_output_lines({
        message <- capture_messages(print(x))
    })

    expect_identical(
        crayon::strip_style(message),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n")
    )

    expect_match(
        output[1],
        "Not enough distinct entities:",
        fixed = TRUE
    )

    expect_match(
        output[4],
        "Dominant entities:",
        fixed = TRUE
    )
}


test_that("print.sdc_descriptives works for problematic case", {
    options(sdc.info_level = 0L)
    expect_print.sdc_descriptives_2(print(descriptives_2))

    options(sdc.info_level = 1L)
    expect_print.sdc_descriptives_2(print(descriptives_2))

    options(sdc.info_level = 2L)
    expect_print.sdc_descriptives_2(print(descriptives_2))
})


descriptives_3 <- structure(
    list(
        options = list_options(),
        settings = list_arguments(id_var = "id", val_var = "val", by = "sector"),
        distinct_ids = distinct_ids_3,
        dominance = dominance_3
    ),
    class = c("sdc_descriptives", "list")
)

expect_print.sdc_descriptives_3 <- function(x) {
    output <- capture_output_lines({
        message <- capture_messages(x)
    })

    expect_identical(
        crayon::strip_style(message),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val | by: sector\n")
    )

    expect_match(
        output[1],
        "Not enough distinct entities:",
        fixed = TRUE
    )

    expect_match(
        output[5],
        "Dominant entities:",
        fixed = TRUE
    )
}


test_that("print.sdc_descriptives works for problematic by case", {
    options(sdc.info_level = 0L)
    expect_print.sdc_descriptives_3(print.sdc_descriptives(descriptives_3))

    options(sdc.info_level = 1L)
    expect_print.sdc_descriptives_3(print.sdc_descriptives(descriptives_3))

    options(sdc.info_level = 2L)
    expect_print.sdc_descriptives_3(print.sdc_descriptives(descriptives_3))
})


descriptives_4 <- structure(
    list(
        options = list_options(),
        settings = list_arguments(id_var = "id"),
        distinct_ids = distinct_ids_1,
        dominance = structure(
            data.table::data.table(value_share = NA_real_),
            class = c("sdc_dominance", "data.table", "data.frame")
        )
    ),
    class = c("sdc_descriptives", "list")
)

test_that("print.sdc_descriptives works for most simple case", {
    options(sdc.info_level = 0L)
    messages <- capture_messages(print.sdc_descriptives(descriptives_1))

    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n")
    )

    options(sdc.info_level = 1L)
    messages <- capture_messages(print.sdc_descriptives(descriptives_1))

    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "Output complies to RDC rules.\n")
    )

    options(sdc.info_level = 2L)
    messages <- capture_messages(print.sdc_descriptives(descriptives_1))

    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "No problem with number of distinct entities (10).\n",
          "No problem with dominance.\n",
          "Output complies to RDC rules.\n")
    )
})

### create model ref
model_ref_1 <- structure(
    list(
        options = list_options(),
        settings = list_arguments(id_var = "id"),
        distinct_ids = distinct_ids_1,
        terms = list(
            y = structure(
                data.table(distinct_ids = 10L),
                class = c("sdc_distinct_ids", "data.table", "data.frame")
            ),
            x_1 = structure(
                data.table(distinct_ids = 10L),
                class = c("sdc_distinct_ids", "data.table", "data.frame")
            ),
            x_2 = structure(
                data.table(distinct_ids = 10L),
                class = c("sdc_distinct_ids", "data.table", "data.frame")
            )
        )
    ),
    class = c("sdc_model", "list")
)

test_that("print.sdc_model works for most simple case", {
    options(sdc.info_level = 0L)
    messages <- capture_messages(print(model_ref_1))

    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id\n")
    )

    options(sdc.info_level = 1L)
    messages <- capture_messages(print(model_ref_1))
    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id\n",
          "Output complies to RDC rules.\n")
    )

    options(sdc.info_level = 2L)
    output <- capture_output({
        messages <- capture_messages(print(model_ref_1))
    })

    expect_identical(
        crayon::strip_style(messages),
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id\n",
          rep("No problem with number of distinct entities (10).\n", 4),
          "Output complies to RDC rules.\n")
    )
})


model_ref_2 <- structure(
    list(
        options = list_options(),
        settings = list_arguments(id_var = "id"),
        distinct_ids = structure(
            data.table(distinct_ids = 4L),
            class = c("sdc_distinct_ids", "data.table", "data.frame")
        ),
        terms = list(
            y = structure(
                data.table(distinct_ids = 4L),
                class = c("sdc_distinct_ids", "data.table", "data.frame")
            ),
            x_1 = structure(
                data.table(distinct_ids = 4L),
                class = c("sdc_distinct_ids", "data.table", "data.frame")
            ),
            x_2 = structure(
                data.table(distinct_ids = 4L),
                class = c("sdc_distinct_ids", "data.table", "data.frame")
            )
        )
    ),
    class = c("sdc_model", "list")
)

test_that("print.sdc_model works for errors", {
    for (level in 1:3) {

        options(sdc.info_level = level)
        output <- capture_output_lines({
            messages <- capture_messages(print(model_ref_2))
        })

        expect_identical(
            crayon::strip_style(messages),
            c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
              "SETTINGS: id_var: id\n")
        )

        lapply(c(1, 5, 10, 15), function(x) {
            expect_match(output[x], "Not enough distinct entities:")
        })
    }
})
