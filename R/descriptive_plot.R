#' descriptive_plot
#'
#' Plot density with descriptive statistics for numerical values.
#'
#' @param data data.frame.
#'
#' @param labels A vector with x-axis labels.
#'
#' @param ... additional arguments to be passed to \code{\link{dplyr::select}}.
#'
#' @return \code{descriptive_plot} function returns a plot with density and descriptive statistics.
#'
#' @examples
#'
#' df <- data.frame(var1=rpois(50, 6), var2=rgamma(50, shape=5,rate=.4), var3=rnorm(50, 10))
#' descriptive_plot(df, var1, var3)
#' descriptive_plot(df, var1:var3)
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @export
descriptive_plot <- function(data, ..., labels = NULL, ylab="Density") {
    vars <- select(data, ...) %>%
        as.list

    if (!is.null(labels)) {
        names(vars) <- labels
    }

    plot_fun <- function(variables, name_x) {
        dens1 <- density(variables)

        pos_median <-
            which(dens1$x >= quantile(variables, .25) &
                      dens1$x <= quantile(variables, .75))

        sigmas <- function(var) {
            mean <- mean(var)
            c(
                -4 * sd(var) + mean,
                -3 * sd(var) + mean,
                -2 * sd(var) + mean,
                -sd(var) + mean,
                mean,
                sd(var) + mean,
                2 * sd(var) + mean,
                3 * sd(var) + mean,
                4 * sd(var) + mean
            )
        }

        descr_plot <- ggplot(mapping = aes(x = variables)) +
            geom_density(color = "red", fill = "deepskyblue1") +
            scale_x_continuous(
                expand = c(0, 0),
                limits = range(dens1$x),
                sec.axis = dup_axis(
                    name = "",
                    breaks = c(sigmas(variables),
                               quantile(variables, .25),
                               quantile(variables, .75),
                               quantile(variables, .5),
                               min(variables),
                               max(variables)),
                    labels = c(
                        expression(paste(-4, sigma)),
                        expression(paste(-3, sigma)),
                        expression(paste(-2, sigma)),
                        expression(paste(-1, sigma)),
                        expression(paste(0, sigma)),
                        expression(paste(1, sigma)),
                        expression(paste(2, sigma)),
                        expression(paste(3, sigma)),
                        expression(paste(4, sigma)),
                        "Q1",
                        "Q3",
                        "Q2",
                        "Min",
                        "Max"
                    )
                )
            ) +
            scale_y_continuous(expand = c(0, 0)) +
            theme_classic() +
            theme(#axis.text.y=element_blank(),
                #axis.line.y=element_blank(),
                #axis.ticks.y=element_blank(),
                #axis.title.y = element_blank(),
                panel.grid.major.x = element_line(linetype = "longdash", color = "black")) +
            geom_area(data = with(dens1, data.frame(x, y))[pos_median,],
                      aes(x = x, y = y),
                      fill = "dodgerblue4") +
            geom_vline(
                xintercept = c(
                    min(variables),
                    quantile(variables, probs = c(.25, .5, .75)),
                    max(variables)
                ),
                linetype = "twodash",
                color = "blue"
            ) +
            geom_vline(
                xintercept = sigmas(variables),
                linetype = "dashed",
                color = "darkorange2"
            ) +
            labs(x = name_x,
                 y = ylab)

        measures <-
            data.frame(
                Min = min(variables),
                Q1 = quantile(variables, .25),
                Median = quantile(variables, .5),
                Q3 = quantile(variables, .75),
                Max = max(variables),
                SD = sd(variables),
                Kurtosis = kurtosis(variables),
                Skweness = skewness(variables),
                CV = sd(variables)/abs(mean(variables))
            ) %>%
            mutate_all(round, 2) %>%
            ggtexttable(.,
                        rows = NULL,
                        theme = ttheme(
                            "mBlue",
                            base_size = 10,
                            padding = unit(c(6, 3), "mm")
                        ))

        ggarrange(
            descr_plot,
            measures,
            ncol = 1,
            nrow = 2,
            heights = c(3, .5)
        )
    }

    suppressWarnings(mapply(
        plot_fun,
        name_x = names(vars),
        variables = vars,
        SIMPLIFY = FALSE
    ))
}
