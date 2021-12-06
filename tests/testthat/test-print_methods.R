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
    expect_message(
        expect_output(
            print(distinct_ids_2)
        ),
        "Not enough distinct entities",
        fixed = TRUE
    )

    options(sdc.info_level = 1L)
    expect_message(
        expect_output(
            print(distinct_ids_2)
        ),
        "Not enough distinct entities:",
        fixed = TRUE
    )

    options(sdc.info_level = 2L)
    expect_message(
        expect_output(
            print(distinct_ids_2)
        ),
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
    expect_message(
        expect_output(
            print(dominance_2)
        ),
        "Dominant entities:",
        fixed = TRUE
    )

    options(sdc.info_level = 1L)
    expect_message(
        expect_output(
            print(dominance_2)
        ),
        "Dominant entities:",
        fixed = TRUE
    )

    options(sdc.info_level = 2L)
    expect_message(
        expect_output(
            print(dominance_2)
        ),
        "Dominant entities:",
        fixed = TRUE
    )
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

dominance_5 <- structure(
    data.table(value_share = 0.1),
    class = c("sdc_dominance", "data.table", "data.frame")
)

test_that("print.sdc_dominance prints info on cases without dominance problem", {
    options(sdc.info_level = 0L)
    expect_silent(print(dominance_5))

    options(sdc.info_level = 1L)
    expect_silent(print(dominance_5))

    options(sdc.info_level = 2L)
    expect_message(
        print(dominance_5),
        "No problem with dominance (0.1).",
        fixed = TRUE
    )
})

# test print.sdc_options ----
test_that("options are printed correctly", {
    options(sdc.n_ids = 3L)
    options(sdc.n_ids_dominance = 1L)
    options(sdc.share_dominance = 0.5)
    expect_identical(
        cli::ansi_strip(capture_message(print(list_options()))$args$text$str),
        "OPTIONS: sdc.n_ids: 3 | sdc.n_ids_dominance: 1 | sdc.share_dominance: 0.5"
    )

    options(sdc.n_ids = 5L)
    options(sdc.n_ids_dominance = 2L)
    options(sdc.share_dominance = 0.85)
    expect_identical(
        cli::ansi_strip(capture_message(print(list_options()))$args$text$str),
        "OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85"
    )
})

# test print.sdc_settings ----
test_that("settings are printed correctly", {

    settings <- list_arguments(id_var = "ID")

    expect_identical(
        cli::ansi_strip(capture_message(print(settings))$args$text$str),
        "SETTINGS: id_var: ID"
    )

    settings <- list_arguments(id_var = "ID", val_var = "VARIABLE")
    expect_identical(
        cli::ansi_strip(capture_message(print(settings))$args$text$str),
        "SETTINGS: id_var: ID | val_var: VARIABLE"
    )

    settings <- list_arguments(id_var = "ID", val_var = "VARIABLE", by = "BY")
    expect_identical(
        cli::ansi_strip(capture_message(print(settings))$args$text$str),
        "SETTINGS: id_var: ID | val_var: VARIABLE | by: BY"
    )


    settings <- list_arguments(
        id_var = "ID", val_var = "VARIABLE", by = "BY", zero_as_NA = FALSE
    )
    expect_identical(
        cli::ansi_strip(capture_message(print(settings))$args$text$str),
        "SETTINGS: id_var: ID | val_var: VARIABLE | by: BY | zero_as_NA: FALSE"
    )

    settings <- list_arguments(
        id_var = "ID", val_var = "VARIABLE", by = "BY", zero_as_NA = FALSE,
        fill_id_var = TRUE
    )
    expect_identical(
        cli::ansi_strip(capture_message(print(settings))$args$text$str),
        "SETTINGS: id_var: ID (filled) | val_var: VARIABLE | by: BY | zero_as_NA: FALSE"
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

    output <- capture_output({
        messages <- clean_cli_output(descriptives_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n")
    )

    options(sdc.info_level = 1L)
    output <- capture_output({
        messages <- clean_cli_output(descriptives_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "v Output complies to RDC rules.\n")
    )

    options(sdc.info_level = 2L)
    output <- capture_output({
        messages <- clean_cli_output(descriptives_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "v No problem with number of distinct entities (10).\n",
          "v No problem with dominance.\n",
          "v Output complies to RDC rules.\n")
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
    expect_output({
        message <- clean_cli_output(x)
    })

    expect_identical(
        message,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "x Not enough distinct entities:\n",
          "x Dominant entities:\n")
    )
}


test_that("print.sdc_descriptives works for problematic case", {
    options(sdc.info_level = 0L)
    expect_print.sdc_descriptives_2(descriptives_2)

    options(sdc.info_level = 1L)
    expect_print.sdc_descriptives_2(descriptives_2)

    options(sdc.info_level = 2L)
    expect_print.sdc_descriptives_2(descriptives_2)
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
    output <- capture_output({
        message <- clean_cli_output(x)
    })

    expect_identical(
        message,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val | by: sector\n",
          "x Not enough distinct entities:\n",
          "x Dominant entities:\n"
        )
    )
}


test_that("print.sdc_descriptives works for problematic by case", {
    options(sdc.info_level = 0L)
    expect_print.sdc_descriptives_3(descriptives_3)

    options(sdc.info_level = 1L)
    expect_print.sdc_descriptives_3(descriptives_3)

    options(sdc.info_level = 2L)
    expect_print.sdc_descriptives_3(descriptives_3)
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
    output <- capture_output({
        messages <- clean_cli_output(descriptives_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n")
    )

    options(sdc.info_level = 1L)
    output <- capture_output({
        messages <- clean_cli_output(descriptives_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "v Output complies to RDC rules.\n")
    )

    options(sdc.info_level = 2L)
    output <- capture_output({
        messages <- clean_cli_output(descriptives_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id | val_var: val\n",
          "v No problem with number of distinct entities (10).\n",
          "v No problem with dominance.\n",
          "v Output complies to RDC rules.\n")
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
    output <- capture_output({
        messages <- clean_cli_output(model_ref_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id\n")
    )

    options(sdc.info_level = 1L)
    output <- capture_output({
        messages <- clean_cli_output(model_ref_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id\n",
          "v Output complies to RDC rules.\n")
    )

    options(sdc.info_level = 2L)
    output <- capture_output({
        messages <- clean_cli_output(model_ref_1)
    })
    expect_identical(
        messages,
        c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
          "SETTINGS: id_var: id\n",
          rep("v No problem with number of distinct entities (10).\n", 4),
          "v Output complies to RDC rules.\n")
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
            messages <- clean_cli_output(model_ref_2)
        })

        expect_identical(
            messages,
            c("OPTIONS: sdc.n_ids: 5 | sdc.n_ids_dominance: 2 | sdc.share_dominance: 0.85\n",
              "SETTINGS: id_var: id\n",
              rep("x Not enough distinct entities:\n", 4))
        )
    }
})

# test print.sdc_min_max
test_that("print.sdc_min_max throws information", {
    ref <- structure(
        list(
            options = sdcLog:::list_options(),
            settings = sdcLog:::list_arguments("id_na", "val_1"),
            min_max = data.table(
                val_var = "val_1",
                min = NA_real_,
                distinct_ids_min = NA_integer_,
                max = NA_real_,
                distinct_ids_max = NA_integer_
            )
        ),
        class = c("sdc_min_max", "list")
    )
    capture_output({
        messages <- capture_messages(print(ref))
    })
    expect_match(
        messages[3],
        "It is impossible to compute extreme values for variable 'val_1' that comply to RDC rules."
    )
})
