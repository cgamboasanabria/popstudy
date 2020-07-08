#' descriptive_plot
#'
#' Plot density with descriptive statistics for numerical values.
#'
#' @param data data.frame.
#'
#' @param ... additional arguments to be passed to \code{\link{select}}.
#'
#' @return \code{descriptive_plot} function returns a plot with density and descriptive statistics.
#'
#' @examples
#'
#' df <- data.frame(var1=rpois(50, 6), var2=rgamma(50, shape=5,rate=.4), var3=rnorm(50, 10))
#' descriptive_plot(df, var1, var3)
#'
#' @author César Gamboa-Sanabria
#'
#' @export
descriptive_plot <- function(data,...){
    vars <- select(data,...) %>%
        as.list

    plot_fun <- function(variables){
        dens1 <- density(variables)

        pos_median <- which(dens1$x>=quantile(variables, .25) & dens1$x <= quantile(variables, .75))

        sigmas <- function(var){
            median <- median(var)
            c(-4*sd(var)+median,-3*sd(var)+median,-2*sd(var)+median,-sd(var)+median,
              median,
              sd(var)+median, 2*sd(var)+median, 3*sd(var)+median, 4*sd(var)+median)
        }

        descr_plot <- ggplot(mapping = aes(x=variables))+
            geom_density(color="red", fill="deepskyblue1")+
            scale_x_continuous(expand = c(0, 0), limits=range(dens1$x),
                               sec.axis = dup_axis(name="",
                                                   breaks = c(sigmas(variables)),
                                                   labels = c(expression(paste(-4, sigma)),
                                                              expression(paste(-3, sigma)),
                                                              expression(paste(-2, sigma)),
                                                              expression(paste(-1, sigma)),
                                                              expression(paste(0, sigma)),
                                                              expression(paste(1, sigma)),
                                                              expression(paste(2, sigma)),
                                                              expression(paste(3, sigma)),
                                                              expression(paste(4, sigma)))))+
            scale_y_continuous(expand = c(0, 0))+
            theme_classic()+
            theme(axis.text.y=element_blank(),
                  axis.line.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.title.y = element_blank(),
                  panel.grid.major.x = element_line(linetype = "longdash", color = "black"))+
            geom_area(data = with(dens1, data.frame(x, y))[pos_median,],
                      aes(x=x, y=y),
                      fill="dodgerblue4")+
            geom_vline(xintercept = c(min(variables),
                                      quantile(variables, probs = c(.25,.75)),
                                      max(variables)),
                       linetype="twodash", color = "blue")+
            geom_vline(xintercept = sigmas(variables), linetype="dashed", color="darkorange2")+labs(x="")

        measures <- data.frame(Min=min(variables), Q1=quantile(variables, .25), Median=quantile(variables, .5),
                               Q3=quantile(variables, .75), Max=max(variables), SD=sd(variables),
                               Kurtosis=kurtosis(variables), Skweness=skewness(variables)) %>%
            mutate_all(round, 2) %>%
            ggtexttable(., rows = NULL,
                        theme = ttheme("mBlue"))

        ggarrange(descr_plot, measures,
                  ncol = 1, nrow = 2,
                  heights = c(3, 0.5))
    }

    lapply(vars, plot_fun)
}
