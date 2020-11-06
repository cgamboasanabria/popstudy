#' Sprague multipliers
#'
#' Method to open five-year grouped ages into specific ages.
#'
#' @param data data.drame. It contains at least two variables: five-year grouped ages and population.
#'
#' @param ... Arguments to be passed to \code{dplyr::select}, i.e., age and population, respectively.
#'
#' @return \code{Sprague} returns a data.frame with specific ages and its population.
#'
#' @examples
#'
#' Sprague(Ecuador1990, age, population)
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @seealso
#'
#' \code{\link{Beers}}
#'
#' @references
#'
#' \insertRef{Shryock}{popstudy}
#'
#' @export
Sprague <- function(data,...){

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
                           value=c(0.3616, 0.264, 0.184, 0.12, 0.0704, 0.0336, 0.008, -0.008,
                                   -0.016, -0.0176, -0.0128, -0.0016, 0.0064, 0.0064, 0.0016,
                                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -0.2768, -0.096,
                                   0.04, 0.136, 0.1968, 0.2272, 0.232, 0.216, 0.184, 0.1408,
                                   0.0848, 0.0144, -0.0336, -0.0416, -0.024, -0.0144, -0.008,
                                   0, 0.008, 0.0144, 0.0176, 0.016, 0.008, -0.008, -0.0336,
                                   0.1488, 0.04, -0.032, -0.072, -0.0848, -0.0752, -0.048,
                                   -0.008, 0.04, 0.0912, 0.1504, 0.2224, 0.2544, 0.2224,
                                   0.1504, 0.0912, 0.04, -0.008, -0.048, -0.0752, -0.0848,
                                   -0.072, -0.032, 0.04, 0.1488, -0.0336, -0.008, 0.008,
                                   0.016, 0.0176, 0.0144, 0.008, 0, -0.008, -0.0144, -0.024,
                                   -0.0416, -0.0336, 0.0144, 0.0848, 0.1408, 0.184, 0.216,
                                   0.232, 0.2272, 0.1968, 0.136, 0.04, -0.096, -0.2768,
                                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.0016, 0.0064,
                                   0.0064, -0.0016, -0.0128, -0.0176, -0.016, -0.008, 0.008,
                                   0.0336, 0.0704, 0.12, 0.184, 0.264, 0.3616)) %>%
        na.omit() %>%
        mutate(suf=rep(paste(".", 1:5, sep=""), 21),
               level=paste(level, suf, sep="")) %>%
        select(-suf)


    result <- data.frame(age=0:99,
                         req_age=c(rep("0-4,5-9,10-14,15-19,NA", 10),
                                   rep("0-4,5-9,10-14,15-19,20-24", 5),
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
                                   rep("75-79,80-84,85-89,90-94,95-99",5),
                                   rep("NA,80-84,85-89,90-94,95-99",10))) %>%
        separate(., col=req_age, into = paste("G",1:5, sep=""), sep=",") %>%
        gather(group, grouped_age, -age) %>%
        mutate(level=ifelse(age<5, "L1",
                            ifelse(age<10, "L2",
                                   ifelse(age<90, "L3",
                                          ifelse(age<95, "L4", "L5"))))) %>%
        filter(grouped_age!="NA") %>%
        arrange(age, group, level, grouped_age) %>%
        left_join(., data) %>%
        mutate(suf=c(rep(paste(".", 1:5, sep=""),each=4, 2),
                     rep(paste(".", 1:5, sep=""),each=5, 16),
                     rep(paste(".", 1:5, sep=""),each=4, 2)),
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
