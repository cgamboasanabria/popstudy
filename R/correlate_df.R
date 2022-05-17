#' correlate_df
#'
#' Compute correlations in a data frames.
#'
#' @param data data.frame. A dataset with the variables to correlate.
#'
#' @param keep_class list. A list that contains desire classes for specyfic variables.
#'
#' @return \code{correlate_df} function returns a list with three objects: A data-frame with the correlation matrix and two correlation plots.
#'
#' @examples
#' \donttest{
#' df <- data.frame(cont1=rnorm(100),
#' cont2=rnorm(100),
#' ordi1=factor(sample(1:5, 100, replace = TRUE), ordered = TRUE),
#' ordi2=factor(sample(1:7, 100, replace = TRUE), ordered = TRUE),
#' bin1=rbinom(100, 1, .4),
#' bin2=rbinom(100, 1, .6),
#' nomi1=factor(sample(letters[1:8], 100, replace = TRUE)),
#' nomi2=factor(sample(LETTERS[1:8], 100, replace = TRUE)))
#'
#' correlate_df(df)
#' }
#' @details \code{correlate_df} takes data.frame class objects and works only with numeric, factor, and ordered class variables, so a previous data cleaning is needed for optimal results. A variable is considered nominal when it is a factor variable with more than two levels, and it is no ordered. When a numeric variable has only two different values, it is considered a binary variable. Also, when a factor variable has only two levels, it is regarded as a binary variable. The computed correlation will depend on the paired-variables class: Pearson method when both variables are numeric, Kendall correlation with a numeric and an ordinal variable, point-biserial with a numeric and a binary variable,  Polychoric correlation with two ordinal variables, Tetrachoric correlation when both are binary, Rank-Biserial when one is ordinal, and the other is binary; and Kruskal's Lambda with one binary and one nominal, or both nominal variables. A Gaussian linear model is fitted to estimate the multiple correlation coefficient in the specific cases of one nominal variable and another numerical or ordered, so the user should take it carefully.
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @references
#'
#' \insertRef{correlate_df}{popstudy}
#'
#' @export
correlate_df <- function(data, keep_class = NULL) {
    class1 <- lapply(data, class)

    #Continuous variables
    cont_vars <- sapply(class1, function(x) {
        sum(x %in% c("numeric", "double", "integer"))
    })
    cont_vars <- which(cont_vars >= 1)

    if (length(cont_vars) > 0) {
        cont_vars <- cont_vars[sapply(data[, cont_vars], function(x) {
            (sum((x %% 1) < 1 & (x %% 1) > 0) > nrow(data) * .20) |
                (length(table(x)) > 2)
        })]
    }
    #Binary variables
    bin_vars <- sapply(class1, function(x) {
        sum(x %in% c("numeric", "double", "integer", "factor", "ordered"))
    })

    bin_vars <- which(bin_vars >= 1)

    if (length(bin_vars) > 0) {
        bin_vars <- bin_vars[sapply(data[, bin_vars], function(x) {
            (length(table(x)) == "2")
        })]
    }
    #Ordinal variables
    ord_vars <- sapply(class1, function(x) {
        sum(x %in% c("ordered"))
    })

    ord_vars <- which(ord_vars >= 1)

    if (length(ord_vars) > 0 & length(levels(data[, ord_vars])) > 2) {
        ord_vars <- ord_vars[sapply(data[, ord_vars], function(x) {
            ("ordered" %in% class(x)) | (if (sum(grepl("\\D", levels(x))) < 1) {
                if (length(which(diff(as.numeric(
                    levels(x)
                )) != 1)) < 1) {
                    TRUE
                } else
                    (FALSE)
            } else
                (FALSE))
        })]
    }
    #Nominal variables
    nom_vars <- sapply(class1, function(x) {
        sum((x %in% c("factor") & length(x) == "1"))
    })

    nom_vars <- which(nom_vars >= 1)

    if (length(nom_vars) > 0 & length(levels(data[, nom_vars])) > 2) {
        nom_vars <- nom_vars[sapply(data[, nom_vars], function(x) {
            ("ordered" %nin% class(x)) & (sum(grepl("\\D", levels(x))) > 0)
            ("ordered" %nin% class(data[, nom_vars])) & (sum(grepl("\\D", levels(data[, nom_vars]))) > 0)
        })]
    }

    types <- list(
        continuous = names(cont_vars),
        binary = names(bin_vars),
        ordinal = names(ord_vars)[names(ord_vars) %nin% names(bin_vars)],
        nominal = names(nom_vars)[names(nom_vars) %nin% names(bin_vars)]
    )
    types <- types[which(sapply(types, length) > 0)] %>%
        lapply(function(x){
            x[complete.cases(x)]
        })

    # keep_class2 <-
    #     data.frame(variable = names(keep_class),
    #                class = unlist(keep_class))

    types <- unlist(types) %>%
        c %>%
        as.data.frame()

    types <- types %>%
        mutate(class = gsub("[0-9]", "",row.names(types))) %>%
        select(class, variable=".") %>%
        data.frame(row.names = NULL)

    types <-
        # do.call(rbind, list(keep_class2,
        #                          {
        #                              data.frame(class = names(types), variable = unlist(types)) %>%
        #                                  filter(variable %nin% keep_class2$variable)
        #                          })) %>%
        types %>%
        split(., .$class) %>%
        lapply(function(x) {
            pull(x, variable)
        })

    #Recoding binary variables
    if ("binary" %in% names(types)) {
        data <- mutate_at(data, vars(types$binary), function(x) {
            x <- as.numeric(x)
            x <- ifelse(x == min(x), 0, 1)
        })
    }

    types <- lapply(types, function(x) {
        as.data.frame(x)
    }) %>%
        do.call(rbind, .) %>%
        mutate(class = unlist(mapply(
            rep, names(types), sapply(types, length), SIMPLIFY = FALSE
        ))) %>%
        rename("variable" = "x")


    if (!is.null(keep_class)) {
        keep_class <-
            data.frame(variable = names(keep_class),
                       class = unlist(keep_class))

        types <- do.call(rbind, list(keep_class,
                                     {
                                         types %>%
                                             filter(variable %nin% keep_class$variable)
                                     }))
    }

    errors <- types %>%
        group_by(variable) %>%
        summarise(cases = n()) %>%
        filter(cases > 1)

    if (nrow(errors) > 0) {
        print(table(types$variable, types$class))
        #return(types)
        stop("More than 1 possible class for the same variable.")

    }



    stages <- t(combn(types$variable, 2))
    stages <- data.frame(V1 = stages[, 1], V2 = stages[, 2])
    stages <- stages %>%
        left_join(., types, by = c("V1" = "variable")) %>%
        rename(V1_class = class) %>%
        left_join(., types, by = c("V2" = "variable")) %>%
        rename(V2_class = class)

    cor_types <-
        data.frame(
            V1_class = c(
                "continuous",
                "continuous",
                "continuous",
                "continuous",
                "ordinal",
                "ordinal",
                "ordinal",
                "binary",
                "binary",
                "nominal"
            ),
            V2_class = c(
                "continuous",
                "ordinal",
                "binary",
                "nominal",
                "ordinal",
                "binary",
                "nominal",
                "binary",
                "nominal",
                "nominal"
            ),
            cor_type = c(
                "pearson",
                "kendall",
                "point-biserial",
                "biserial",
                "polychoric",
                "rank-biserial",
                "kruskal",
                "tetrachoric",
                "kruskal",
                "kruskal"
            )
        )
    cor_types <- do.call(rbind, list(cor_types, {
        cor_types %>%
            rename(V1_class = V2_class,
                   V2_class = V1_class) %>%
            select(V1_class, V2_class, cor_type)
    })) %>%
        unique %>%
        left_join(stages, ., by = c("V1_class", "V2_class"))

    cor_types <- cor_types %>%
        mutate(
            r = ifelse(
                cor_type %in% c(
                    "pearson",
                    "kendall",
                    "spearman",
                    "point-biserial",
                    "biserial",
                    "polychoric",
                    "tetrachoric",
                    "biweight",
                    "distance",
                    "percentage",
                    "shepherd"
                ),
                paste(
                    "as.data.frame(correlation(select(data,",
                    V1,
                    ",",
                    V2,
                    "), method='",
                    cor_type,
                    "'))",
                    sep = ""
                ),
                NA
            ),
            r = ifelse(
                V1_class == "continuous" & V2_class == "ordinal",
                paste(
                    "as.data.frame(correlation(mutate(select(data,",
                    V1,
                    ",",
                    V2,
                    "),",
                    V2,
                    "=as.numeric(",
                    V2,
                    ")), method='",
                    cor_type,
                    "',include_factors=TRUE))$tau",
                    sep = ""
                ),
                ifelse(
                    V1_class == "ordinal" & V2_class == "continuous",
                    paste(
                        "as.data.frame(correlation(mutate(select(data,",
                        V2,
                        ",",
                        V1,
                        "),",
                        V1,
                        "=as.numeric(",
                        V1,
                        ")), method='",
                        cor_type,
                        "',include_factors=TRUE))$tau",
                        sep = ""
                    ),
                    r
                )
            ),
            r = ifelse(
                V1_class == "continuous" & V2_class == "nominal",
                paste(
                    "with(data, sqrt(summary(lm(",
                    V1,
                    "~",
                    V2,
                    "))$r.squared))"
                ),
                ifelse(
                    V1_class == "nominal" & V2_class == "continuous",
                    paste(
                        "with(data, sqrt(summary(lm(",
                        V2,
                        "~",
                        V1,
                        "))$r.squared))"
                    ),
                    r
                )
            ),
            r = ifelse(
                cor_type == "kruskal",
                paste(
                    "with(summarise(group_by(select(data,",
                    V1,
                    ",",
                    V2,
                    "),",
                    V1,
                    ",",
                    V2,
                    "),Freq=n()), Lambda(xtabs(Freq~",
                    V1,
                    "+",
                    V2,
                    ")))"
                ),
                r
            ),
            r = ifelse(
                V1_class == "ordinal" & V2_class == "nominal",
                paste(
                    "with(data, sqrt(summary(lm(as.integer(",
                    V1,
                    ")~",
                    V2,
                    "))$r.squared))"
                ),
                ifelse(
                    V1_class == "nominal" & V2_class == "ordinal",
                    paste(
                        "with(data, sqrt(summary(lm(as.integer(",
                        V2,
                        ")~",
                        V1,
                        "))$r.squared))"
                    ),
                    r
                )
            ),
            r = ifelse(
                V1_class == "ordinal" & V2_class == "binary",
                paste("wilcoxonRG(x=data$", V1, ", g=data$", V2, ")", sep =
                          ""),
                ifelse(
                    V1_class == "binary" & V2_class == "ordinal",
                    paste("wilcoxonRG(x=data$", V2, ", g=data$", V1, ")", sep =
                              ""),
                    r
                )
            ),
            r = ifelse(
                grepl("biserial", r) |
                    grepl("point-biserial", r) |
                    grepl("polychoric", r),
                paste(r, "$rho", sep = ""),
                ifelse(
                    grepl("pearson", r) |
                        grepl("tetrachoric", r),
                    paste(r, "$r", sep = ""),
                    r
                )
            )
        )
    suppressMessages({
        cors <- sapply(cor_types$r, function(x) {
            eval(parse(text = x))
        }) %>%
            unname
    })

    #eval(parse(text = cor_types$r[3]))

    # cosa <- data
    # cosa[4,2] <- 6
    # as.data.frame(correlation(select(data,cont2,cont1), method='point-biserial'))$rho
    # 	as.data.frame(correlation(select(data,cont2,ordi1) %>% mutate(ordi1=as.integer(ordi1)), method='point-biserial'))$rho

    cor_types$r <- cors
    cor_types <- cor_types %>%
        select(V1, V2, r) %>%
        mutate_if(is.factor, as.character)

    df2 <- data.frame(
        V1 = c(cor_types[1, 1], last(cor_types$V2)),
        V2 = c(cor_types[1, 1], last(cor_types$V2)),
        r = NA
    ) %>%
        mutate_if(is.factor, as.character)

    cor_types <- do.call(rbind, list(df2[1, ], cor_types))
    cor_types <- do.call(rbind, list(cor_types, df2[2, ]))
    cor_types <- cor_types %>%
        pivot_wider(names_from = V2, values_from = r) %>%
        rename(rowname = V1)
    cor_types <- as.matrix(cor_types[, -1])
    cor_types[lower.tri(cor_types)] <-
        t(cor_types)[lower.tri(t(cor_types))]

    cormat <- cor_types %>%
        as.data.frame() %>%
        mutate(rowname = names(.)) %>%
        select(rowname, names(.))

    cor.mtest <- function(mat, ...) {
        mat <- as.matrix(mat)
        n <- ncol(mat)
        p.mat <- matrix(NA, n, n)
        diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
                tmp <- cor.test(mat[, i], mat[, j], ...)
                p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            }
        }
        colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
        p.mat
    }
    cormat2 <- as.matrix(cormat[, -1])
    diag(cormat2) <- 1
    row.names(cormat2) <- cormat$rowname
    p.mat <- cor.mtest(cormat2)
    plot2 <- corrplot(
        corr = cormat2,
        p.mat = p.mat,
        type = "lower",
        sig.level = 0.05,
        diag = T,
        method = "color",
        addCoef.col = "black",
        number.cex = .8,
        tl.col = "black",
        order = "original",
        tl.srt = 45,
        pch = "X",
        pch.col = "firebrick1",
        addrect = 3
    )
    plot1 <-
        tryCatch({
            network_plot(cormat)
        }, error = function(e)
            NULL)

    diag(cormat[, -1]) <- 1

    list(cormat = cormat,
         network = plot1,
         probability_plot = plot2)
}
