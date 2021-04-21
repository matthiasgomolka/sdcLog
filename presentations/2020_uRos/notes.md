What do I do?

- you know my name and where I work
- RDC -> reason why I spent time on an R package for disclosure control in the context of research data centers
- joined work with Tim Becker (also in the audience)
- you need to understand a little bit of what do we at the RDC
- bullets ...
- secure environment

A tiny bit of theory

An example

- ID column, grouping variables, value variables
- calculate grouped mean (using data.table)
- show that result complies to rules
- check

Another example

- same example, now grouping by sector AND year
- problems with both number of distinct entities and dominance

Other functionality

- `sdc_model()` for all models supported by `broom`
- `sdc_extreme()` for the calculation of non-confidential min/max values

Why is it called sdcLog?

- Workflow at our RDC:

    - researchers write their analysees in scripts
    - within these scripts they use the functions mentioned before (name them)
    - researchers source their scripts using `sdc_log()`
    - `sdc_log()` creates a log file of everything which happened in the script
    - log file is inspected by RDC staff to evaluate if results comply to RDC rules
    - results can be released
