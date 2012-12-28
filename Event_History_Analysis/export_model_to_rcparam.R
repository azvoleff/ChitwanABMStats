export_to_model_param <- function(model, txtfile, param_prefix, type="rcparams.default") {
    model_coefs <- as.character(round(coef(model), 6))
    coef_names <- names(coef(model))
    #coef_names <- gsub('[()]|(^I[(])', '', coef_names)
    #coef_names <- gsub('\\^2', '_squared', coef_names)
    if (type == 'rcparams.default') {
        params <- paste("'", param_prefix, ".", coef_names, sep="")
        params <- paste(params, "' : [", sep="")
        params <- paste(params, model_coefs, " | validate_float]", sep="")
    }
    else if (type == "rcfile") {
        params <- paste(param_prefix, coef_names, sep=".")
        params <- paste(params, " : ", sep="")
        params <- paste(params, model_coefs, sep="")
    }
    param_file <- file(txtfile, "w")
    writeLines(params, param_file, sep="\n")
    close(param_file)
}
