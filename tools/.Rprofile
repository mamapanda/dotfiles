local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cloud.r-project.org"
    options(repos = r)
})

setHook(
    packageEvent("languageserver", "onLoad"),
    function(...) {
        options(languageserver.default_linters = lintr::with_defaults(
            camel_case_linter = NULL,
            commented_code_linter = NULL,
            object_usage_linter = NULL,
            snake_case_linter = NULL
        ))
    }
)
