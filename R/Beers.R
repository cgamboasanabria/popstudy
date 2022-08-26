#' Beers multipliers
#'
#' Method to open five-year grouped ages into specific ages.
#'
#' @param data data.drame. It contains at least two variables: five-year grouped ages and population.
#'
#' @param ... Arguments to be passed to \code{dplyr::select}, i.e., age and population, respectively.
#'
#' @return \code{Beers} returns a data.frame with specific ages and populations.
#'
#' @examples
#'
#' Beers(Ecuador1990, age, population)
#'
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @seealso
#'
#' \code{\link{Sprague}}
#'
#' @references
#'
#' \insertRef{Shryock}{popstudy}
#'
#' @export
Beers <- function(data,...){
    vars <- select(data,...) %>%
        as.list
    grouped_age <- as.character(vars[[1]])
    pop <- vars[[2]]
    data <- data.frame(grouped_age=grouped_age, pop=pop)
    limit <- as.numeric(last(gsub("[+]", "", data$grouped_age)))
    top <-data[nrow(data),] %>%
        rename(age=grouped_age)

    constant <- data.frame(group=rep(paste("G", 1:5, sep=""), by=5, each=25),
                           level=rep(rep(paste("L", 1:5, sep=""), each=5), 5),
                           value=c(0.3333, 0.2595, 0.1924, 0.1329, 0.0819, 0.0404,
                                   0.0093, -0.0108, -0.0198, -0.0191, -0.0117,
                                   -0.002, 0.005, 0.006, 0.0027, -0.0015, -0.0038,
                                   -0.0028, 0.0013, 0.0068, 0.0115, 0.0129, 0.0084,
                                   -0.0045, -0.0283, -0.1636, -0.078, 0.0064, 0.0844,
                                   0.1508, 0.2, 0.2268, 0.2272, 0.1992, 0.1468,
                                   0.0804, 0.016, -0.028, -0.04, -0.0284, -0.0084,
                                   0.0072, 0.0112, 0.0028, -0.0128, -0.0284, -0.0356,
                                   -0.0256, 0.01, 0.0796, -0.021, 0.013, 0.0184,
                                   0.0054, -0.0158, -0.0344, -0.0402, -0.0248, 0.0172,
                                   0.0822, 0.157, 0.22, 0.246, 0.22, 0.157, 0.0822,
                                   0.0172, -0.0248, -0.0402, -0.0344, -0.0158, 0.0054,
                                   0.0184, 0.013, -0.021, 0.0796, 0.01, -0.0256, -0.0356,
                                   -0.0284, -0.0128, 0.0028, 0.0112, 0.0072, -0.0084,
                                   -0.0284, -0.04, -0.028, 0.016, 0.0804, 0.1468,
                                   0.1992, 0.2272, 0.2268, 0.2, 0.1508, 0.0844, 0.0064,
                                   -0.078, -0.1636, -0.0283, -0.0045, 0.0084, 0.0129,
                                   0.0115, 0.0068, 0.0013, -0.0028, -0.0038, -0.0015,
                                   0.0027, 0.006, 0.005, -0.002, -0.0117, -0.0191,
                                   -0.0198, -0.0108, 0.0093, 0.0404, 0.0819, 0.1329,
                                   0.1924, 0.2595, 0.3333)) %>%
        mutate(suf=rep(paste(".", 1:5, sep=""), 25),
               level=paste(level, suf, sep="")) %>%
        select(-suf)


    result <- data.frame(age=0:99,
               req_age=c(rep("0-4,5-9,10-14,15-19,20-24", 15),
                         rep("5-9,10-14,15-19,20-24,25-29",5),
                         rep("10-14,15-19,20-24,25-29,30-34",5),
                         rep("15-19,20-24,25-29,30-34,35-39",5),
                         rep("20-24,25-29,30-34,35-39,40-44",5),
                         rep("25-29,30-34,35-39,40-44,45-49",5),
                         rep("30-34,35-39,40-44,45-49,50-54",5),
                         rep("35-39,40-44,45-49,50-54,55-59",5),
                         rep("40-44,45-49,50-54,55-59,60-64",5),
                         rep("45-49,50-54,55-59,60-64,65-69",5),
                         rep("50-54,55-59,60-64,65-69,70-74",5),
                         rep("55-59,60-64,65-69,70-74,75-79",5),
                         rep("60-64,65-69,70-74,75-79,80-84",5),
                         rep("65-69,70-74,75-79,80-84,85-89",5),
                         rep("70-74,75-79,80-84,85-89,90-94",5),
                         rep("75-79,80-84,85-89,90-94,95-99",15))) %>%
        separate(., col=req_age, into = paste("G",1:5, sep=""), sep=",") %>%
        gather(group, grouped_age, -age) %>%
        mutate(level=ifelse(age<5, "L1",
                            ifelse(age<10, "L2",
                                   ifelse(age<90, "L3",
                                          ifelse(age<95, "L4", "L5"))))) %>%
        filter(grouped_age!="NA") %>%
        arrange(age, group, level, grouped_age) %>%
        left_join(., data) %>%
        mutate(suf=c(rep(paste(".", 1:5, sep=""),each=5, 20)),
               level=paste(level, suf, sep="")) %>%
        select(-suf) %>%
        left_join(., constant) %>%
        arrange(age, grouped_age) %>%
        mutate(pop_fix=pop*value) %>%
    group_by(age) %>%
        summarise(pop=round(sum(pop_fix, na.rm = T), 0)) %>%
        filter(age<limit) %>%
        mutate(age=as.character(age))

    do.call(rbind, list(result, top))
}
