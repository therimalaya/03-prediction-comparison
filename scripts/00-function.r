## ---- catvec: Print a vector ----
## Print a vector adding `and` before the last element
## INPUT: A vector
## OUTPUT: A character string
## ---- ---- ---- ---- ---- ----
catvec <- function(vec) {
    v1 <- paste(vec[-length(vec)], collapse = ", ")
    return(paste(v1, vec[length(vec)], sep = " and "))
}

## ---- list2chr: Convert list of parameters to a character ----
## Converts a list into a character vector. The nested list
## is separated by semicolon and the elements by comma
## INPUT: A list
## OUTPUT: A character string
## ---- ---- ---- ---- ---- ----
list2chr <- function(lst){
    paste(map_chr(lst, function(x) {
        paste(x, collapse = ", ")
    }), collapse = "; ")
}

## ---- name_it: give the name to a list or vector ----
## Give the nave to a list or vector
## INPUT: list or vector (usually a character vector)
## OUTPUT: A list or vector based on input
name_it <- function(x, name = NULL) {
    if (!is.character(x) & is.null(name))
        stop("Argument 'name' is required if x is not character vector")
    if (is.null(name) & is.character(x))
        names(x) <- x
    else
        names(x) <- name
    return(x)
}
## ---- ---- ---- ---- ---- ----
## ---- get_integer: Get integer from a text removing all the strings ----
## Get integer from a text removing all the strings
## INPUT: A vector, value for empty strings
## OUTPUT: A numeric vector by removing all the strings
## ---- ---- ---- ---- ---- ----
get_integer <- function(vec, empty_val = NULL) {
    ret <- gsub("\\D+", "", vec)
    if (!is.null(empty_val))
        ret[ret == ""] <- gsub("\\D+", "", empty_val)
    as.numeric(ret)
}

## ---- get_design: Extract single design from a design table ready for simulation ----
## Extract a design from a design table and prepare it for simrel function
## INPUT: A design table (class: "simrel-param-table") and a row index
## OUTPUT: A list of simrel parameters which can directly be passed to simrel function
## ---- ---- ---- ---- ---- ----
get_design <- function(design_tbl, which = 1L){
    design_tbl %>% slice(which) %>% transpose %>% .[[1]]
}

## ---- simulate: Simulate data from suppled design paramters ----
## From the simrel parameters, run the simrel function
## INPUT: A design (list of simrel parameters)
## OUTPUT: A simrel object
## ---- ---- ---- ---- ---- ----
simulate <- function(design) {
    do.call(simrel, design)
}

## Extract true values from the simulated object ----
get_true_value <- function(sim_obj, what = c("coef", "minerror", "sigma_x", "sigma_y")) {
    what <- match.arg(what, what)
    switch(
        what,
        coef = {
            out <- as.matrix(rbind(sim_obj$beta0, sim_obj$beta))
            dimnames(out) <- list(
                c("(Intercept)", paste0("X", 1:(nrow(out) - 1))),
                paste0("Y", 1:ncol(out))
            )
            return(out)
        },
        minerror = sim_obj$minerror,
        sigma_x = sim_obj$Sigma[-c(1:sim_obj$m), -c(1:sim_obj$m)],
        sigma_x = sim_obj$Sigma[c(1:sim_obj$m), c(1:sim_obj$m)]
    )
}

## Get data from simulated object, if need_pc than x will the principal components ----
get_data <- function(sim_obj, need_pc = FALSE, prop = ifelse(need_pc, 0.95, NULL), ncomp = NULL, ...) {
    Y <- sim_obj$Y
    if (need_pc) {
        pc.a <- prcomp(sim_obj$X)
        nc <- max(which(cumsum(pc.a$sdev/sum(pc.a$sdev)) < prop))
        if (!is.null(ncomp)) nc <- max(ncomp, nc)
        X <- pc.a$x[, 1:nc]
        out <- data.frame(y = I(Y), x = I(X))
        attr(out, "rotation") <- pc.a$rotation[, 1:nc]
    } else {
        X <- sim_obj$X
        out <- data.frame(y = I(Y), x = I(X))
        if (!is.null(sim_obj$ntest)) {
            testY <- sim_obj$testY
            testX <- sim_obj$testX
            test_out <- data.frame(y = I(testY), x = I(testX))
            out <- rbind(train = out, test = test_out)
        }
    }
    class(out) <- append(class(out), "simrel_data")
    return(out)
}

## Fit a model with data and method supplied ----
fit_model <- function(data, method = c("PCR", "PLS1", "PLS2", "Xenv", "Yenv", "Ridge", "Lasso", "Senv", "CPPLS"), ...) {
    method <- match.arg(method, method)
    if (method %in% c("Ridge", "Lasso")) {
        require(parallel)
        cl <- makeCluster(6, type = "PSOCK")
        registerDoParallel(cl)
        on.exit(stopCluster(cl))
    }
    list2env(list(...), environment())
    if (method == "Senv" & !exists('u')) u <- 2
    fit <- switch(
        method,
        PCR   = pcr(y ~ x, data = data, ncomp = 10),
        PLS2  = plsr(y ~ x, data = data, ncomp = 10),
        CPPLS = cppls(y ~ x, data = data, ncomp = 10, trunc.pow = 0.5),
        Xenv  = map(1:min(10, ncol(data$x)), function(nc) with(data, try(xenv(x, y, u = nc)))),
        Yenv  = map(1:ncol(data$y), function(nc) with(data, try(env(x, y, u = nc)))),
        Senv = map(1:10, function(nc) with(data, try(stenv(x, y, q = nc, u = u)))),
        Ridge = cv.glmnet(data$x, data$y, family = "mgaussian", alpha = 0,
                          parallel = TRUE, nlambda = 100),
        Lasso = cv.glmnet(data$x, data$y, family = "mgaussian", alpha = 1,
                          parallel = TRUE, nlambda = 100),
        PLS1  = map(1:ncol(data$y), function(i){
            dta <- data.frame(y = data$y[, i], x = data$x)
            plsr(y ~ x, data = dta)
        })
    )
    class(fit) <- append(class(fit), "fitted_model")
    return(fit)
}

## get Estimated coefficients ----
get_beta <- function(method = c('pcr', 'pls', 'cppls', 'mvr', 'xenv',
                                'cpls', "ridge", "lasso", "senv",
                                'try-error', "glmnet", "cv.glmnet",
                                "yenv", "senv", "pls1", 'pls2')){
    mthd <- match.arg(method)
    if (mthd %in% c('pcr', 'pls', 'cppls', 'cpls', 'pls2')) method <- 'mvr'
    if (mthd %in% c("senv", "stenv")) method <- "senv"
    if (mthd %in% c("glmnet", "cv.glmnet", "ridge", "lasso")) method <- "glmnet"
    if (mthd %in% c("try-error")) return(method)
    switch(method,
           mvr    = {
               coefs <- function(mdl, ncomp = 10, intercept = TRUE)
               {
                   coef <- drop(coef(mdl, intercept = intercept, ncomp = 1:ncomp))
                   dimnames(coef)[[3]] <- paste0("Comp", 1:dim(coef)[3])
                   return(coef)
               }
           },
           pls1   = {
               coefs <- function(mdl, ncomp = 10, intercept = TRUE)
               {
                   out <- sapply(mdl, coef, intercept = intercept,
                                 ncomp = 1:ncomp, simplify = 'array')
                   out <- unname(aperm(drop(out), c(1, 3, 2)))
                   dimnames(out) <- list(
                       paste0("X", 1:dim(out)[1]),
                       paste0("Y", 1:dim(out)[2]),
                       paste0("Comp", 1:dim(out)[3])
                   )
                   return(out)
               }
           },
           xenv   = {
               coefs <- function(mdl, intercept = TRUE)
               {
                   out <- sapply(mdl, function(obj) {
                       if (intercept) rbind(c(unname(obj$mu)), unname(obj$beta))
                       else unname(obj$beta)
                   }, simplify = 'array')
                   dx <- dim(out)[1]
                   dy <- dim(out)[2]
                   dz <- dim(out)[3]
                   dnx <- if (intercept) c("Intercept", paste0("X", 1:(dx - 1))) else paste0("X", 1:dx)
                   dny <- paste0("Y", 1:dy)
                   dnz <- paste0("Comp", 1:dz)
                   dimnames(out) <- list(dnx, dny, dnz)
                   return(out)
               }
           },
           yenv   = {
               coefs <- function(mdl, intercept = TRUE)
               {
                   out <- sapply(mdl, function(obj) {
                       unname(t(obj$beta))
                   }, simplify = 'array')
                   if (intercept) {
                       out <- abind::abind(sapply(mdl, "[[", "alpha"), out, along = 1)
                   }
                   dimnames(out) <- list(
                       paste0("X", 1:dim(out)[1]),
                       paste0("Y", 1:dim(out)[2]),
                       paste0("Comp", 1:dim(out)[3])
                   )
                   return(out)
               }
           },
           senv   = {
               coefs <- function(mdl, intercept = TRUE)
               {
                   out <- sapply(mdl, function(obj) {
                       unname(obj$beta)
                   }, simplify = 'array')
                   if (intercept) {
                       out <- abind::abind(sapply(mdl, "[[", "alpha"), out, along = 1)
                   }
                   dimnames(out) <- list(
                       paste0("X", 1:dim(out)[1]),
                       paste0("Y", 1:dim(out)[2]),
                       paste0("Comp", 1:dim(out)[3])
                   )
                   return(out)
               }
           },
           glmnet = {
               coefs <- function(mdl, intercept = TRUE)
               {
                   o <- options(scipen = 999)
                   coef <- sapply(mdl$glmnet.fit$beta, as.matrix, simplify = 'array')
                   if (intercept) {
                       beta0 <- mdl$glmnet.fit$a0
                       coef <- abind::abind(t(beta0), coef, along = 1)
                       dimnames(coef)[[1]][1] <- "Intercept"
                   }
                   out <- aperm(coef, c(1, 3, 2))
                   dimnames(out)[[3]] <- round(mdl$glmnet.fit$lambda, 5)
                   attr(out, "lambda") <- mdl$glmnet.fit$lambda
                   attr(out, "lambda.min") <- mdl$lambda.min
                   attr(out, "lambda.1se") <- mdl$lambda.1se
                   options(o)
                   return(out)
               }
           })
    return(coefs)
}
get_est_beta <- function(fit, model_name, beta_fun = get_beta(tolower(model_name)),
                         rotation_mat = NULL, intercept = TRUE) {
    if (!is.null(rotation_mat)) {
        out <- beta_fun(fit, intercept = intercept)
        out <- apply(out, 2:3, function(x) rotation_mat %*% x)
        dimnames(out)[[1]] <- paste0("X", 1:dim(out)[1])
    } else {
        out <- beta_fun(fit, intercept = intercept)
    }
    class(out) <- append(class(out), "estimated_coefficient")
    return(out)
}

## Compute prediction and estimation error ----
getPredErr <- function(coef, minerr, trueBeta, sigma, scale = FALSE){
    out <- map(0:dim(coef)[3], function(cmp){
        if (cmp == 0) {
            bmat <- matrix(0, nrow = nrow(coef[, , cmp + 1]),
                           ncol = ncol(coef[, , cmp + 1]))
        } else {
            bmat <- coef[, , cmp]
        }
        if (dim(sigma)[1] + 1 == nrow(bmat))
            sigma <- rbind(0, cbind(0, sigma))
        err_out <- t(bmat - trueBeta) %*% sigma %*% (bmat - trueBeta)
        out <- err_out + minerr
        if (scale) out <- out/minerr
        return(diag(out))
    })
    names(out) <- c("0", dimnames(coef)[[3]])
    ret <- map_df(out, ~map_df(.x, ~data_frame(Pred_Error = .x),
                               .id = "Response"),
    .id = "Tuning_Param")
    ret <- ret %>%
        mutate_at(c("Tuning_Param", "Response"),
                  get_integer, empty_val = 0) %>%
        mutate_at("Response", as.integer)
    class(ret) <- append(class(ret), "prediction_error")
    return(ret)
}
getEstErr <- function(coef, trueBeta){
    out <- map(0:dim(coef)[3], function(cmp){
        if (cmp == 0) {
            bmat <- matrix(0, nrow = nrow(coef[, , cmp + 1]),
                           ncol = ncol(coef[, , cmp + 1]))
        } else {
            bmat <- coef[, , cmp]
        }
        err_out <- t(bmat - trueBeta) %*% (bmat - trueBeta)
        diag(err_out)
    })
    names(out) <- c("0", dimnames(coef)[[3]])
    ret <- map_df(out, ~map_df(.x, ~data_frame(
        Est_Error = .x
    ), .id = "Response"),
    .id = "Tuning_Param")
    ret <- ret %>%
        mutate_at(c("Tuning_Param", "Response"), get_integer, empty_val = 0) %>%
        mutate_at("Response", as.integer)
    class(ret) <- append(class(ret), "estimation_error")
    return(ret)
}
compute_intercept <- function(B, Xmeans, Ymeans) {
    dB <- dim(B)
    dB[1] <- dB[1] + 1
    dnB <- dimnames(B)
    dnB[[1]] <- c("(Intercept)", dnB[[1]])
    BInt <- array(dim = dB, dimnames = dnB)
    BInt[-1, , ] <- B
    for (i in 1:dim(B)[3])
        BInt[1, , i] <- Ymeans - Xmeans %*% B[, , i]
    B <- BInt
    return(B)
}
coef_errors <- function(sim_obj, est_method, scale = FALSE, ...) {
    dta <- sim_obj %>%
        get_data(...)
    rot <- attr(dta, "rotation")
    Ymeans <- colMeans(sim_obj$Y)
    Xmeans <- colMeans(sim_obj$X)
    coef <- dta %>%
        fit_model(est_method, ...) %>%
        get_est_beta(tolower(est_method), rotation_mat = rot, intercept = FALSE)
    coef <- compute_intercept(coef, Xmeans, Ymeans)

    ## True and Estimated Coefficient Data Frame ----
    coef_df <- reshape2::melt(coef, varnames = c("Predictor", "Response", "Components"),
                              value.name = "Estimated")
    true_coef <- get_true_value(sim_obj, "coef")
    true_coef_df <- reshape2::melt(true_coef, varnames = c("Predictor", "Response"),
                                   value.name = "True")
    if (!(est_method %in% c('Lasso', 'Ridge'))) {
        coef_df <- coef_df %>% spread(Components, Estimated)
    }
    coef_df <- coef_df %>%
        left_join(true_coef_df, by = c("Predictor", "Response")) %>%
        as_tibble()
    class(coef_df) <- append(class(coef_df), "coefficient_df")

    ## Prediction Error ----
    pred_err <- coef %>%
        getPredErr(
            minerr = get_true_value(sim_obj, "minerror"),
            trueBeta = get_true_value(sim_obj, "coef"),
            sigma = get_true_value(sim_obj, "sigma_x"),
            scale = scale
        )
    class(pred_err) <- append(class(pred_err), "prediction_error_df")

    ## Estimation Error ----
    est_err <- coef %>%
        getEstErr(trueBeta = get_true_value(sim_obj, "coef"))
    class(est_err) <- append(class(est_err), "estimation_error_df")

    out <- list(
        coefficients = coef_df,
        prediction_error = pred_err,
        estimation_error = est_err
    )
    attr(out, "Method") <- est_method
    attr(out, "Sim_Properties") <- list(
        n = sim_obj$n,
        p = sim_obj$p,
        q = sim_obj$q,
        m = sim_obj$m,
        relpos = sim_obj$relpos,
        ypos = sim_obj$ypos,
        gamma = sim_obj$gamma,
        eta = sim_obj$eta,
        R2 = sim_obj$R2,
        ntest = sim_obj$ntes,
        muX = sim_obj$muX,
        muY = sim_obj$muY,
        type = sim_obj$type
    )
    class(out) <- append(class(out), "coefficient_error")
    return(out)
}

## Complete the following function making it pure but compatible with impure functions -----------
## Coefficient Plot ----
coef_plot <- function(coef_error, ncomp = NULL, err_type = "prediction") {
    ## Check if it is nested ----
    not_nested <- "coefficient_error" %in% class(coef_error)
    if (not_nested) coef_error <- list(coef_error)

    ## Is a shrinkage method ----
    method <- map_chr(coef_error, attr, "Method") %>% unique()
    is_shrinkage <- method %in% c("Ridge", "Lasso")

    ## Coefficients ----
    coef_mat <- map(coef_error, 'coefficients')
    coef_mat <- map(coef_mat, ~mutate_if(.x, is.factor, as.character))
    n_rep <- length(coef_error)

    ## Grouping Variables ----
    group_vars <- c('Predictor', 'Response')

    ## What if not shrinkage ----
    if (!is_shrinkage) {
        if (is.null(ncomp)) ncomp <- seq.int(0, 5) else if (length(ncomp) == 1) ncomp <- seq.int(0, ncomp)
        coef_mat <- map(coef_mat, function(x){
            as_tibble(x) %>%
                gather(Components, Estimated, -c(1:2, ncol(.))) %>%
                mutate_at('Components', ~as.integer(get_integer(.x))) %>%
                filter(Components %in% ncomp)
        })
        group_vars <- c('Predictor', 'Response', 'Components')
    } else {
        err_df <- map(coef_error, paste0(err_type, '_error'))
        err_df <- map(err_df, function(x) {
            x %>% mutate(Response = paste0("Y", Response)) %>%
                rename_at(1, function(x) "Error") %>%
                group_by(Response) %>%
                filter(Error == min(Error))
        })
        coef_mat <- map2(coef_mat, err_df, function(x, y){
            x %>% anti_join(y, by = c('Components' = 'Tuning_Param',
                                      'Response' = 'Response'))
        })
    }

    dta <- bind_rows(coef_mat, .id = "Replication") %>%
        mutate_at(vars(Predictor, Response), get_integer) %>%
        mutate(Predictor = ifelse(is.na(Predictor), 0, Predictor)) %>%
        mutate_at(vars(Replication, Predictor, Response), as.integer) %>%
        gather(Est_Type, Coef, True, Estimated) %>%
        group_by_at(group_vars) %>%
        group_by(Est_Type, add = TRUE) %>%
        summarize_at("Coef", mean)


    facet_form <- if (!is_shrinkage) {
        as.formula(Response ~ Components)
    } else {
        as.formula(Response ~ .)
    }

    prm <- attr(coef_error[[1]], "Sim_Properties")
    sub_title <- paste0("p:", prm$p, " gamma:", prm$gamma,
                        " eta:", prm$eta, " R2:", prm$R2)
    title <- paste("Method:", method)
    if (!not_nested) title <- paste(title, "Replicates:", n_rep)

    plt <- dta %>%
        ggplot(aes(Predictor, Coef, color = Est_Type, group = Est_Type)) +
        geom_line() +
        geom_point(shape = 4, size = 0.7) +
        theme(legend.position = "bottom",
              plot.subtitle = element_text(family = "mono")) +
        labs(y = "Coefficient", color = NULL, linetype = NULL) +
        facet_grid(facet_form, labeller = label_both) +
        ggtitle(title, subtitle = sub_title) +
        scale_color_brewer(palette = "Set1")
    return(plt)
}

## Error Plot -----
err_plot <- function(coef_error, error_type = "Prediction", ncomp = NULL,
                     params = NULL, title = NULL, subtitle = NULL) {
    not_nested <- "coefficient_error" %in% class(coef_error)
    if (not_nested) coef_error <- list(coef_error)
    METHOD <- map_chr(coef_error, attr, "Method") %>% unique()
    is_shrinkage <- METHOD %in% c("Ridge", "Lasso")
    error_df <- map_df(coef_error, .id = "Replication",
                       paste(tolower(error_type), "error", sep = "_"))
    if (!is.null(ncomp) & !is_shrinkage) {
        if (length(ncomp) == 1) ncomp <- 1:ncomp
        error_df <- error_df %>% filter(Tuning_Param %in% ncomp)
    }
    dta <- error_df %>%
        mutate(Response = paste0("Y", Response))
    names(dta)[grep("Error", names(dta))] <- "Error"

    params <- if (is.null(params)) c('p', 'eta', 'gamma', 'relpos') else params
    prms <- attr(coef_error[[1]], "Sim_Properties")
    lbls <- sapply(prms[params], list2chr)

    dgn_lbl <- paste0(paste(names(lbls), lbls, sep = ": "), collapse=", ")

    lbl <- dta %>%
        group_by(Response, Tuning_Param) %>%
        summarize_at('Error', mean) %>%
        group_by(Response) %>%
        summarize(Tuning_Param = Tuning_Param[which.min(Error)],
                  Error = min(Error)) %>%
        mutate(label = paste0(Response, " = ", round(Error, 3), " (", Tuning_Param, ")")) %>%
        arrange(Error)

    x_lab <- if (is_shrinkage) "Lambda" else "Number of Components"

    if (!is_shrinkage) {
        dta <- dta %>%
            mutate(Tuning_Param = as.factor(as.integer(Tuning_Param)))
    }

    plt <- dta %>%
        ggplot(aes(Tuning_Param, Error, fill = Response)) +
        stat_summary(fun.y = mean, geom = "line", size = 0.8,
                     aes(group = Response, color = Response))
    if (not_nested) {
        plt <- plt + stat_summary(fun.y = mean, geom = "point",  size = 1, shape = 21,
                                  aes(group = Response, fill = Response),
                                  stroke = 0.2)
    }
    if (!is_shrinkage) {
        plt <- plt +
            stat_summary(fun.y = mean, geom = "point",  size = 2, shape = 21,
                         aes(group = Response, fill = Response))
    }
    if (!not_nested & !is_shrinkage) {
        plt <- plt + geom_point(aes(color = Response),
                                position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0),
                                alpha = 0.1) +
            geom_boxplot(alpha = 0.2, color = "grey70", size = 0.3)
    }
    plt_title <- paste0(paste(error_type, "Error Plot:: "), "Method: ", METHOD)
    if (!is.null(title)) plt_title <- paste(plt_title, title)
    plt_subtitle <- if (is.null(subtitle)) dgn_lbl else paste(dgn_lbl, subtitle)
    plt <- plt + labs(x = x_lab, y = paste(error_type, "Error"),
                      color = "Response", fill = "Response") +
        geom_text(data = lbl, aes(label = label, color = Response),
                  x = Inf, y = Inf, hjust = 1,
                  vjust = seq(2, nrow(lbl) * 2, 2),
                  family = "mono") +
        ggtitle(plt_title, subtitle = plt_subtitle) +
        theme_light() +
        theme(plot.subtitle = element_text(family = "mono"),
              legend.position = "bottom")

    return(plt)
}

## Effect Plot ----
eff_mlm <- function(focal.predictors, mod, response, ...){
    if (missing(response)) {
        mod.frame <- model.frame(mod)
        response <- colnames(model.response(mod.frame))
    } else if (is.numeric(response)) {
        mod.frame <- model.frame(mod)
        response.names <- colnames(model.response(mod.frame))
        response <- response.names[response]
    }
    data <- cbind(model.response(mod.frame), mod.frame[-1])
    if (length(response) == 1) {
        mod.1 <- update(mod, as.formula(paste(response, " ~ .")))
        result <- Effect(focal.predictors, mod.1, ...)
    } else {
        result <- as.list(NULL)
        for (resp in response) {
            preds <- paste(attr(mod$terms, 'term.labels'), collapse = " + ")
            fn <- eval(mod$call[[1]])
            mod.1 <- fn(as.formula(paste(resp, " ~ ", preds)), data = data)
            lab <- resp
            result[[lab]] <- effects::effect(focal.predictors, mod.1)
        }
        class(result) <- "efflist"
    }
    result
}
deparse_term_label <- function(term, model) {
    trms <- strsplit(term, ":")[[1]]
    possible_terms <- unlist(combinat::permn(trms, fun = paste0, collapse = ":"))
    term_labels <- attr(model$terms, "term.labels")
    term <- possible_terms[possible_terms %in% term_labels]
    return(term)
}
eff_df <- function(term, model) {
    term_arg <- term
    term <- deparse_term_label(term, model)
    trms <- strsplit(term, ":")[[1]]
    eff_df <- map_df(suppressMessages(eff_mlm(term, model)),
                     as.data.frame, .id = "Response") %>%
        as.tibble()
    attr(eff_df, "model") <- deparse(substitute(model))
    attr(eff_df, "terms") <- deparse(model$terms)
    attr(eff_df, "term_arg") <- term_arg
    class(eff_df) <- append(class(eff_df), "effects.df")
    return(eff_df)
}
get_eff_plot <- function(effect_df, term, reorder = FALSE, show_errorbar = FALSE, ...) {
  trms <- rev(unlist(strsplit(term, ":")))
  effect_df <- effect_df %>% 
    select_at(vars(!!!trms, everything()))
  if (length(trms) <= 2) {
    facet_formula <- NULL
  } else {
    trms_sub <- trms[-c(1:2)]
    facet_formula <- paste(trms_sub[1], paste(trms_sub[-1], collapse = " + "), sep = " ~ ")
    if (length(trms_sub) == 1) facet_formula <- paste0(facet_formula, ".")
  }
  if (reorder) {
    plt <- effect_df %>%
      ggplot(aes(reorder(get(trms[1]), fit), fit))
  } else {
    plt <- effect_df %>% ggplot(aes(get(trms[1]), fit))
  }
  if (length(trms) == 1) {
    plt <- plt +
      stat_summary(fun.y = mean, geom = "line", group = 1) +
      stat_summary(fun.y = mean, geom = "point")
  } else {
    plt <- plt +
      stat_summary(fun.y = mean, geom = "line",
                   aes(color = get(trms[2]), group = get(trms[2]))) +
      stat_summary(fun.y = mean, geom = "point", size = 0.8,
                   aes(color = get(trms[2]), group = get(trms[2]))) +
      labs(color = trms[2])
  }
  plt <- plt + theme(legend.position = "bottom",
                     plot.subtitle = element_text(family = "mono")) +
    labs(x = trms[1], y = "Effect")
  if (!is.null(facet_formula))
    plt <- plt + facet_grid(as.formula(facet_formula), ...)
  if (show_errorbar & length(trms) > 1)
    plt <- plt + stat_summary(fun.data = mean_se,
                              geom = "errorbar", width = 0.1,
                              aes(color = get(trms[2])))
  if (show_errorbar & length(trms) <= 1)
    plt <- plt + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1)
  return(plt)
}
eff_plot <- function(effect_df, reorder = FALSE,
                     show_errorbar = FALSE, ...) {
    dfn <- names(effect_df)
    trms <- unlist(strsplit(attr(effect_df, "term_arg"), ":"))
    order <- length(trms) - 2 ## length except methods and response
    model_txt <- attr(effect_df, "model")
    title <- ifelse(grepl("pred", model_txt), "Prediction",
                    ifelse(grepl("est", model_txt), "Estimation", "Model"))
    if ("Method" %in% trms) {
        mthd_idx <- which(trms %in% "Method")
        trms <- c(trms[mthd_idx], trms[-mthd_idx])
    }
    if (length(trms) == 3) {
        facet_formula <- paste(c("Response", trms[-c(1:2)]), collapse = " ~ ")
    } else if (length(trms) == 2) {
        facet_formula <- paste(c(".", c("Response", trms[-c(1:2)])), collapse = " ~ ")
    } else if (length(trms) == 1) {
        facet_formula <- "Response ~ ."
    } else {
        facet_formula <- paste(c("Response", paste(trms[-c(1:2)], collapse = " + ")), collapse = " ~ ")
    }
    sub_title <- attr(effect_df, "terms")
    if (reorder) {
        plt <- effect_df %>%
            ggplot(aes(reorder(get(trms[1]), fit), fit))
    } else {
        plt <- effect_df %>% ggplot(aes(get(trms[1]), fit))
    }
    if (length(trms) == 1) {
        plt <- plt +
            stat_summary(fun.y = mean, geom = "point") +
            stat_summary(fun.y = mean, geom = "line", group = 1)
    } else {
        plt <- plt +
            stat_summary(fun.y = mean, geom = "line",
                         aes(color = get(trms[2]), group = get(trms[2]))) +
            stat_summary(fun.y = mean, size = 0.8, geom = "point",
                         aes(group = get(trms[2]), color = get(trms[2]))) +
            labs(color = trms[2])
    }
    plt <- plt + theme(legend.position = "bottom",
                       plot.subtitle = element_text(family = "mono")) +
        labs(x = trms[1], y = "Effect") +
        ggtitle(paste("Effect Plot:", title), subtitle = sub_title)
    if (show_errorbar & length(trms) == 1) {
        plt <- plt + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1)
    }
    if (show_errorbar & length(trms) > 1) {
        plt <- plt + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1,
                                  aes(color = get(trms[2])))
    }
    plt <- plt + facet_grid(as.formula(facet_formula), ...)
    if ("Method" %in% trms)
        plt <- plt + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    return(plt)
}
eff_plot3 <- function(effect_df, reorder = FALSE,
                      show_errorbar = FALSE, ...) {
    dfn <- names(effect_df)
    trms <- unlist(strsplit(attr(effect_df, "term_arg"), ":"))
    # trms <- dfn[-c(1, grep("fit", dfn):length(dfn))]
    order <- length(trms) - 2 ## length except methods and response
    model_txt <- attr(effect_df, "model")
    title <- ifelse(grepl("pred", model_txt), "Prediction",
                    ifelse(grepl("est", model_txt), "Estimation", "Model"))
    if ("Method" %in% trms) {
        mthd_idx <- which(trms %in% "Method")
        trms <- c(trms[mthd_idx], trms[-mthd_idx])
    }
    if (length(trms) <= 2) {
        facet_formula <- NULL
    } else {
        trms_sub <- trms[-c(1:2)]
        facet_formula <- paste(trms_sub[1], paste(trms_sub[-1], collapse = " + "), sep = " ~ ")
        if (length(trms_sub) == 1) facet_formula <- paste0(facet_formula, ".")
    }
    sub_title <- attr(effect_df, "terms")
    if (reorder) {
        plt <- effect_df %>%
            ggplot(aes(reorder(get(trms[1]), fit), fit))
    } else {
        plt <- effect_df %>% ggplot(aes(get(trms[1]), fit))
    }
    if (length(trms) == 1) {
        plt <- plt +
            stat_summary(fun.y = mean, geom = "line", group = 1) +
            stat_summary(fun.y = mean, geom = "point")
    } else {
        plt <- plt +
            stat_summary(fun.y = mean, geom = "line",
                         aes(color = get(trms[2]), group = get(trms[2]))) +
            stat_summary(fun.y = mean, geom = "point", size = 0.8,
                         aes(color = get(trms[2]), group = get(trms[2]))) +
            labs(color = trms[2])
    }
    plt <- plt + theme(legend.position = "bottom",
                       plot.subtitle = element_text(family = "mono")) +
        labs(x = trms[1], y = "Effect") +
        ggtitle(paste("Effect Plot:", title), subtitle = sub_title)
    if (!is.null(facet_formula))
        plt <- plt + facet_grid(as.formula(facet_formula), ...)
    if (show_errorbar & length(trms) > 1)
        plt <- plt + stat_summary(fun.data = mean_se,
                                  geom = "errorbar", width = 0.1,
                                  aes(color = get(trms[2])))
    if (show_errorbar & length(trms) <= 1)
        plt <- plt + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1)
    if ("Method" %in% trms)
        plt <- plt + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

    return(plt)
}
eff_plot2 <- function(term, model, show_errorbar = FALSE,
                      summary_fn = mean) {
    ## Identify correct term ----
    trms <- strsplit(term, ":")[[1]]
    sub_title <- deparse(model$terms)
    model_txt <- deparse(substitute(model))
    title <- switch(model_txt, pred_mdl = "Prediction Error", est_mdl = "Estimation Error")
    order <- length(trms)
    possible_terms <- unlist(combinat::permn(trms, fun = paste0, collapse = ":"))
    term_labels <- attr(model$terms, "term.labels")
    term <- possible_terms[possible_terms %in% term_labels]
    eff_df <- map_df(suppressMessages(eff_mlm(term, model)),
                     as.data.frame, .id = "Response") %>%
        as.tibble()
    if ("Method" %in% trms) {
        mthd_idx <- which(trms %in% "Method")
        trms <- c(trms[mthd_idx], trms[-mthd_idx])
    }
    if (length(trms) == 3) {
        facet_formula <- paste(c(".", trms[-c(1:2)]), collapse = " ~ ")
    } else if (length(trms) == 4) {
        facet_formula <- paste(trms[-c(1:2)], collapse = " ~ ")
    } else {
        sub_trms <- trms[-c(1:2)]
        facet_formula <- paste(c(sub_trms[1], paste(sub_trms[-1], collapse = " + ")), collapse = " ~ ")
    }
    eff_df <- eff_df %>%
        group_by_at(trms) %>%
        summarize_at(vars(fit, lower, upper), summary_fn)
    plt <- eff_df %>%
        ggplot(aes(get(trms[1]), fit))
    if (length(trms) == 1) {
        plt <- plt + geom_point() + geom_line(group = 1)
    } else {
        plt <- plt +
            geom_line(aes(color = get(trms[2]), group = get(trms[2]))) +
            geom_point(size = 0.8, aes(group = get(trms[2]), color = get(trms[2]))) +
            labs(color = trms[2])
    }
    if (length(trms) > 2) {
        plt <- plt + facet_grid(as.formula(facet_formula), labeller = label_both)
    }
    plt <- plt + theme(legend.position = "bottom",
                       plot.subtitle = element_text(family = "mono")) +
        labs(x = trms[1], y = "Effect") +
        ggtitle(paste("Effect Plot:", title), subtitle = sub_title)
    if (show_errorbar)
        plt <- plt + geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.1))
    if ("Method" %in% trms)
        plt <- plt + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    return(plt)
}

## ---- Relevant Space Diagram ----
plot_relspace <- function(rect_height = c(0.6, 0.6), rect_width = c(0.2, 0.6), rect_gap = 0.1,
                          space_fill_color = c('blue4', 'green3'), rect_fill_color = NULL) {
    require(grid)

    ## Starting coordinate for y
    y_x0 <- 0.05
    y_y0 <- 0.1

    ## Specify Widths
    y_width <- rect_width[1]
    x_width <- rect_width[2]

    ## Starting coordinate for x
    x_x0 <- y_x0 + y_width + rect_gap
    x_y0 <- 0.1

    ## Specify Heights
    y_height <- rect_height[1]
    x_height <- rect_height[2]

    grid.newpage()

    ## Rectangles for X and Y variables ----
    grid.rect(x = y_x0,
              y = y_y0,
              height = y_height,
              width = y_width,
              hjust = 0, vjust = 0)
    grid.rect(x = x_x0,
              y = x_y0,
              height = x_height,
              width = x_width,
              hjust = 0, vjust = 0)

    ## Relevant space in Y space ----
    df_y <- data.frame(
        x = c(0.05, 0.05, 0.05, y_width) + y_x0,
        y = c(0.05, 0.05, 0.05, y_height) + y_y0
    )
    grid.xspline(
        x = rep(df_y$x, 2),
        y = rep(rev(df_y$y), 2),
        gp = gpar(fill = "green4", alpha = 0.5, lwd = 2),
        shape = 1, open = FALSE
    )

    ## Relevant space in X space ----
    df_x <- data.frame(
        x = c(0.05, 0.05, 0.05, x_width) + x_x0,
        y = c(0.05, 0.05, 0.05, x_height) + y_y0
    )
    grid.xspline(
        x = rep(df_x$x, 2),
        y = rep(rev(df_x$y), 2),
        gp = gpar(fill = "blue4", alpha = 0.5, lwd = 2),
        shape = 1, open = FALSE
    )

    ## Title of the diagram ----
    grid.text(x = y_x0, y = 0.95, label = "Relevant space within a model",
              hjust = 0, gp = gpar(cex = 1.6))
    ## Subtitle of the diagram ----
    grid.text(x = y_x0, y = 0.88, hjust = 0, gp = gpar(cex = 0.9),
              label = "A concept for reduction of regression models")

    ## Labels on top of the X and Y boxes ----
    grid.text(x = y_x0 + y_width/2,
              y = y_y0 + y_height,
              label = "Response (Y)",
              draw = T, vjust = -1,
              gp = gpar(cex = 1))
    grid.text(x = x_x0 + x_width/2 ,
              y = x_y0 + x_height,
              label = "Predictor (X)",
              draw = T, vjust = -1,
              gp = gpar(cex = 1))

    ## Labels inside the X and Y boxes ----
    grid.text(x = y_x0 + y_width/2,
              y = y_y0 + y_height,
              vjust = 1.5,
              label = "Redundant Y\ninformation",
              gp = gpar(cex = 0.8))
    grid.text(x = x_x0 + x_width/2 ,
              y = x_y0 + x_height,
              vjust = 1.5,
              gp = gpar(cex = 0.8),
              label = "Irrelevant X-Space\n redundant information and noise")

    ## Big arror from X to Y to represent their connection ----
    big_arrow <- function(lw = 22, color = "grey40"){
        segmentsGrob(x0 = x_x0 - 0.01,
                     x1 = y_x0 + y_width + 0.05,
                     y0 = x_height/2,
                     y1 = x_height/2,
                     arrow = arrow(length = unit(0.15, "inches")),
                     gp = gpar(lwd = lw,
                               linejoin = "mitre",
                               lineend = "butt",
                               col = color))
    }
    grid.draw(big_arrow(25, "grey50"))
    grid.draw(big_arrow(20, "grey90"))

    ## Arrows to annotate relevant X and Y spaces ----
    grid.segments(x0 = c(y_x0 + y_width/2, x_x0 + x_width/2),
                  x1 = c(y_x0 + y_width/2, x_x0 + x_width/2),
                  y0 = c(y_y0 - 0.05, y_y0 - 0.05),
                  y1 = c(y_x0 + 0.1, y_x0 + 0.1),
                  arrow = arrow(length = unit(0.09, "inches")),
                  gp = gpar(lty = 1, lwd = 2, col = "grey40"))
    grid.segments(x0 = y_x0 + y_width/2,
                  y0 = y_y0 - 0.05,
                  x1 = x_x0 + x_width/2,
                  y1 = y_y0 - 0.05)

    ## Text annotation for relevant X and Y space ----
    grid.text(x = x_x0, y = x_y0 - 0.1, gp = gpar(cex = 0.9),
              label = "X and Y envelope/\n Relevant Spaces",
              vjust = -0.25)
}

## ---- Data Exploration Plot ------
plot_data_transform <- function() {
  library(grid)
  grid.newpage()

  fctr_rect <- rectGrob(x = 0.1, y = 0.5, height = 0.8, width = 0.15)
  fctr_label <- textGrob(x = 0.1, y = 0.5, label = "Factors")
  fctr_grob <- gList(fctr_rect, fctr_label)

  n_rect <- function(start = 0.2, step = 0.025, n = 4, label = paste0("Y", 1:n)) {
    seq_ <- seq(start, by = step, length.out = n)
    out <- lapply(seq_along(seq_), function(x){
      rect <- rectGrob(x = seq_[x], y = 0.5, height = 0.8, width = step)
      txt <- textGrob(x = seq_[x], y = 0.5 + 0.25,
                      label = label[x], hjust = 1.15,
                      rot = 90,
                      gp = gpar(fontsize = 10))
      gList(rect, txt)
    })
    do.call(gList, out)
  }
  pred_grob <- n_rect(0.2, 0.035, 4, label = paste0("PE", 1:4))
  pc_grob <- n_rect(0.4, 0.035, 4, label = paste0("PC", 1:4))
  dta_grob <- gList(fctr_grob, pred_grob, pc_grob)

  plt_dta <- data.frame(x = rnorm(1000))
  plt <- ggplot(plt_dta, aes(x)) +
    geom_histogram(bins = 30, color = "grey",
                   aes(y = ..density..), fill = NA) +
    stat_density(geom = "line") +
    labs(x = "PC1", y = "Density")
  plt_grob <- ggplotGrob(plt)

  arr1_grob <- linesGrob(c(0.33, 0.375), y = c(0.5, 0.5), gp = gpar(lwd = 2),
                         arrow = arrow(length = unit(2, "mm")))
  arr2 <- xsplineGrob(
      x = c(0.4, 0.475, 0.6, 0.7),
      y = c(0.85, 0.8, 1, 0.85),
      shape = 0.75, gp = gpar(lwd = 2),
      arrow = arrow(length = unit(2, 'mm')))
  arr2_dot <- circleGrob(0.4, y = 0.85, r = 0.01, gp = gpar(fill = "black"))
  arr2_grob <- gList(arr2, arr2_dot)
  
  vp1 <- viewport(x = 0, y = 0.5, height = 0.8, width = 1, just = "left")
  pushViewport(vp1)
  grid.draw(dta_grob)

  vp2 <- viewport(x = 0.6, y = 0.5, height = 0.8, width = 0.4, just = "left")
  pushViewport(vp2)
  grid.draw(plt_grob)
  upViewport()

  grid.draw(arr1_grob)
  grid.draw(arr2_grob)
}

## ---- Very IMPURE Functions ----
# ## Coefficient Plot ----
# get_coef <- function(design, method) {
#     out <- map_df(design, function(dgn){
#         fname <- paste0("scripts/robj/coef-error/dgn-", dgn, "-", tolower(method), ".Rdata")
#         load(fname)
#         is_shrinkage <- method %in% c("Ridge", "Lasso")
#         out <- map_df(out, function(x) {
#             ret <- x[["coefficients"]]
#             ret <- gather(ret, Tuning_Param, Coef, -c(1:2, ncol(ret)))
#             ret <- ret %>%
#                 mutate_at("Tuning_Param", if (is_shrinkage) as.numeric else get_integer) %>%
#                 mutate_if(is.factor, as.character)
#         }, .id = "Replication")
#         return(out)
#     }, .id = "Design")
#     return(as_tibble(out))
# }
# coef_plot <- function(design, method, ncomp = ifelse(method == "Yenv", 3, 7), error_type = "prediction") {
#     ncomp <- if (length(ncomp) == 1) 1:ncomp
#     dgn <- as.character(design)
#     coef <- get_coef(dgn, method) %>%
#         mutate_at(vars(Predictor, Response), ~as.integer(get_integer(.x)))
#
#     is_shrinkage <- method %in% c("Ridge", "Lasso")
#     group_vars <- c("Design", "Predictor", "Response", "Est_type")
#
#     if (is_shrinkage) {
#         err_dta_chr <- switch(error_type, prediction = "pred_error", estimation = "est_error")
#         if (!exists(err_dta_chr)) stop("Load prediction and estimation error data frame first!")
#         err_dta <- get(err_dta_chr)
#         tp <- err_dta %>%
#             filter(Method == method, Design == dgn) %>%
#             group_by(Replication, Response) %>%
#             top_n(-1, wt = get(names(err_dta)[4])) %>%
#             ungroup() %>%
#             select(Tuning_Param) %>%
#             distinct() %>%
#             .[[1]]
#         dta <- coef %>%
#             filter(Tuning_Param %in% tp)
#     } else {
#         group_vars <- append(group_vars, "Tuning_Param")
#         dta <- coef %>%
#             filter(Tuning_Param %in% ncomp)
#     }
#
#     dta <- dta %>%
#         gather(Est_type, Est_value, True, Coef) %>%
#         group_by_at(group_vars) %>%
#         rename(Component = Tuning_Param)
#
#     dta_avg <- dta %>%
#         summarize_at("Est_value", mean) %>%
#         ungroup() %>%
#         mutate(Est_type = case_when(
#             Est_type == "True" ~ "True",
#             Est_type == "Coef" ~ "Estimated")
#         )
#
#     facet_form <- if (!is_shrinkage) {
#         as.formula(Response ~ Component)
#     } else {
#         as.formula(Response ~ .)
#     }
#     sub_title <- with(design_chr %>% slice(as.numeric(design)), {
#         paste0("p: ", p, ", relpos: ", relpos,
#                ", gamma: ", gamma, ", eta: ", eta, ", R2: ", R2)
#     })
#
#     plt <- dta_avg %>%
#         ggplot(aes(Predictor, Est_value, color = Est_type, group = Est_type)) +
#         geom_line() + geom_point(shape = 21, size = 0.7) +
#         theme(legend.position = "bottom",
#               plot.subtitle = element_text(family = "mono")) +
#         labs(y = "Coefficient", color = NULL) +
#         facet_grid(facet_form, labeller = label_both) +
#         ggtitle(paste("Coefficient plot:: ", paste("Design:", design, "Method:", method)),
#                 sub_title)
#     return(plt)
# }
# ## Error Plot ----
# get_err_plot <- function(design, method, flip_facet = FALSE) {
#     dgn <- as.character(design)
#     is_shrinkage <- tolower(method) %in% c("ridge", "lasso")
#     x_lab <- ifelse(!(is_shrinkage), "Number of Component", "Lambda")
#     dta <- est_error %>%
#         filter(Design == dgn, Method == method) %>%
#         left_join(pred_error %>% filter(Design == dgn, Method == method),
#                   by = c("Design", "Method", "Replication", "Tuning_Param", "Response")) %>%
#         rename(Prediction = Pred_Error, Estimation = Est_Error) %>%
#         gather(Error_type, Error, Estimation, Prediction) %>%
#         mutate(Response = factor(Response))
#     group_vars <- c("Design", "Method", "Response", "Error_type")
#     if (!is_shrinkage) group_vars <- append(group_vars, "Tuning_Param")
#     dta_avg <- dta %>%
#         group_by(Design, Method, Response, Tuning_Param, Error_type) %>%
#         summarize(Error = mean(Error))
#     dta_min <- dta_avg %>%
#         group_by(Design, Method, Response, Error_type) %>%
#         filter(Error == min(Error)) %>%
#         arrange(Error) %>%
#         mutate(label = paste0("Y", Response, ": ", round(Error, 3)))
#     dgn_lbl <- design_chr %>% slice(design) %>%
#         select(p, eta, gamma, R2) %>% as.data.frame()
#     dgn_lbl <- paste(paste(names(dgn_lbl), dgn_lbl, sep = ": "), collapse = " ")
#     facet_formula <- if (flip_facet) as.formula(. ~ Error_type) else as.formula(Error_type ~ .)
#     v_just <- rep(seq(1, design_chr$m[design] * 2, 2), 2)
#     plt <- dta_avg %>%
#         ggplot(aes(Tuning_Param, Error, color = Response, group = Response)) +
#         facet_grid(facet_formula, scales = 'free_y') +
#         geom_line() +
#         labs(x = x_lab, color = "Response") +
#         theme(legend.position = "bottom",
#               plot.subtitle = element_text(family = "mono")) +
#         ggtitle(paste0("Estimation and Perediction Error: ", method),
#                 subtitle = dgn_lbl)
#     if (!is_shrinkage) {
#         plt <- plt + geom_point()  +
#             scale_x_continuous(breaks = 0:10)
#     }
#     plt <- plt +
#         geom_text(data = dta_min, x = Inf, y = Inf, show.legend = FALSE,
#                   aes(label = label, color = Response),
#                   inherit.aes = FALSE, hjust = 1, vjust = v_just) +
#         geom_point(data = dta_min, fill = "white", shape = 21)
#     return(plt)
#     ## Relevant Space Plot
#     ## Estimation Error Plot ----------------------
# }
