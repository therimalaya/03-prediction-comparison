dta <- design %>%
    get_design(1) %>%
    simulate() %>%
    get_data()
mdl <- dta %>%
    fit_model("Ridge")

