#' Lexis diagram
#'
#' Plot a Lexis Diagram from births and deaths data for a given year, month, and day with specific simple ages.
#'
#' @param deaths_data data.frame. A dataset with three variables: date_reg, the registered death date, age, the age of decease; and deaths, the deaths number for that date. See \code{\link{CR_deaths}}.
#'
#' @param births_data data data.frame. A dataset with two variables: date_reg, the registered birth date; and births, the births number for that date. See \code{\link{CR_births}}.
#'
#' @param first.date character. Optional argument that specifies the first date of interest.
#'
#' @param choose_year numeric. The year from which the countdown begins until the desired minimum age is reached.
#'
#' @param choose_month numeric. The month from which the countdown begins until the desired minimum age is reached.
#'
#' @param choose_day numeric. The day from which the countdown begins until the desired minimum age is reached.
#'
#' @param ages numeric. An ages vector to plot the diagram.
#'
#' @param factors numeric. Optional argument to set specific factors to set alpha and delta sections in Lexis Diagram.
#'
#' @return Lexis function returns a list with two objects: diagram, the Lexis diagram; and deaths, the estimated deaths number.
#'
#' @examples
#'
#' Lexis(CR_deaths, CR_births, choose_year=2011, choose_month=1, choose_day=1, ages=0:9)$diagram
#' \dontrun{
#' ##Lexis diagram with specific factors
#' data("births_deaths")
#' Births <- filter(births_deaths$births, sex=="male")
#' Deaths <- filter(births_deaths$deaths, sex=="male")
#' Lexis(deaths_data=Deaths, births_data=Births, first.date = "1999-01-01",
#' choose_year=2007, choose_month=1, choose_day=1, ages=0:4,
#' factors = c(.2,.41,.47,.48,.48))$diagram
#' }
#' @author Cesar Gamboa-Sanabria
#'
#' @references
#'
#' \insertRef{Lexis}{popstudy}
#'
#' @export
Lexis <- function(deaths_data, births_data, first.date=NULL, choose_year, choose_month, choose_day, ages, factors=NULL){
    if(is.null(first.date)){
        first.date <- min(ymd(paste(choose_year, choose_month, choose_day))-years(length(ages)))
    }else({first.date <- ymd(first.date)})

    final.date <- ymd(paste(choose_year, choose_month, choose_day))
    years_study <- year(seq(first.date, final.date, by="year"))
    df <- data.frame(x=seq(year(first.date), year(final.date)-1, by=1),
                     y=rep(min(ages), length(seq(year(first.date), year(final.date)-1, by=1))),
                     xend=rep(year(final.date), length(seq(year(first.date), year(final.date)-1, by=1))),
                     yend=c(rep(NA, length(seq(year(first.date), year(final.date)-1, by=1))-length(c(ages+1))), rev(c(ages+1))))
    df$yend <- length(df$yend):min(df$yend, na.rm=TRUE)
    data <- deaths_data %>%
        filter(date_reg<=ymd(paste(choose_year, choose_month, choose_day))-days(1) &
                   date_reg>=first.date & age %in% ages) %>%
        group_by(date_reg=year(date_reg), age) %>%
        summarise(deaths=sum(deaths)) %>%
        mutate(A=364,
               Li=age*365,
               Ls=Li+A,
               choose_days.fin=ifelse(age<max(age), Ls+1, Ls),
               fac.sep=(1/max(choose_days.fin))*(Li+A/2))
    if(!is.null(factors)){
        data <- data.frame(age=ages, fac.sep=factors) %>%
            left_join(data, ., "age") %>%
            select(-contains(".x")) %>%
            rename(fac.sep=fac.sep.y)
    }
    data <- data %>%
        mutate(delta=round(deaths*fac.sep, 0),
               alpha=deaths-delta) %>%
        gather(cohort, deaths2, -c(date_reg:fac.sep)) %>%
        select(date_reg, cohort, age, deaths2) %>%
        data.frame() %>%
        arrange(date_reg, age, cohort) %>%
        mutate(filter.alpha=rep(seq(min(ages), length(unique(date_reg))-1), each=unique(table(date_reg))),
               filter.delta=rep(seq(min(ages), length(unique(date_reg))-1)-1, each=unique(table(date_reg))),
               filter.delta=ifelse(date_reg==min(date_reg), NA, filter.delta),
               deaths2=ifelse(cohort=="alpha" & age>filter.alpha, NA, deaths2),
               deaths2=ifelse(cohort=="delta" & (age>filter.delta | is.na(filter.delta)), NA, deaths2))

    data1 <- births_data %>%
        group_by(date_reg=year(date_reg)) %>%
        summarise(births=sum(births)) %>%
        right_join(., data, by="date_reg") %>%
        filter(cohort=="delta")

    k1 <- lapply(split(data1$deaths2, data1$age), function(x)x[complete.cases(x)])

    k1 <- colSums(do.call(rbind, lapply(k1, function(x){
        c(x, rep(0, length(unique(data1$filter.alpha))-1-length(x)))
    })))

    data2 <- births_data %>%
        group_by(date_reg=year(date_reg)) %>%
        summarise(births=sum(births)) %>%
        merge(., data, by="date_reg")%>%
        filter(cohort=="alpha")
    k2 <- lapply(split(data2$deaths2, data2$age), function(x)x[complete.cases(x)])

    k2 <- colSums(do.call(rbind, lapply(k2, function(x){
        c(x, rep(0, length(unique(data2$filter.alpha))-length(x)))
    })))

    k <- data.frame(date_reg=unique(data$date_reg),
                    age=unique(data1$filter.alpha),
                    deaths2.total=colSums(rbind(c(k1, rep(0, length(k2)-length(k1))), k2)))

    d_births_data <- births_data %>%
        group_by(date_reg=year(date_reg)) %>%
        summarise(births=sum(births)) %>%
        merge(., data, by=c("date_reg")) %>%
        merge(., k, by=c("date_reg", "age"), all=TRUE) %>%
        mutate(pop.total=births-deaths2.total,
               births=ifelse(age!=0 | cohort=="delta", NA, births),
               pop.total=ifelse(cohort=="delta", pop.total, NA))

    blue_ones <- d_births_data %>%
        filter(date_reg %in% c((choose_year-5):choose_year) & age %in% 0:4 & cohort=="alpha")%>%
        mutate(blue_ones=mapply(rep, "Si", 1:5) %>%
                   lapply(function(x){
                       c(x, rep(NA, 5-length(x)))
                   }) %>%
                   unlist())

    blue_ones <- sum(blue_ones$deaths2[blue_ones$blue_ones=="Si"], na.rm = TRUE)


    red_ones <- d_births_data %>%
        filter(date_reg %in% c((choose_year-5+1):choose_year) & age %in% 0:4 & cohort=="delta")%>%
        mutate(red_ones=mapply(rep, "Si", 1:4) %>%
                   lapply(function(x){
                       c(x, rep(NA, 5-length(x)))
                   }) %>%
                   unlist())

    red_ones <- sum(red_ones$deaths2[red_ones$red_ones=="Si"], na.rm = TRUE)

    diagram <- d_births_data %>%
        ggplot(., aes(x=year(date_reg), y=age))+
        geom_segment(data=df, aes(x=x, y=y, xend=xend, yend=yend))+
        geom_segment(data=df, aes(x=x+1, y=min(yend)-1, xend=x+1, yend=rev(yend)))+
        geom_segment(data=df, aes(x=x+1, y=min(yend)-1, xend=x+1, yend=rev(yend)))+
        geom_segment(data=df, aes(x=x, y=rev(yend-1), xend=max(xend), yend=rev(yend-1)))+
        scale_y_continuous(position="right") +
        geom_text(data=filter(data, cohort=="alpha"), aes(x=date_reg+.75, y=age+.25, label=deaths2, color="red"))+
        geom_text(data=filter(data, cohort=="delta"), aes(x=date_reg+.25, y=age+.75, label=deaths2, color="blue"))+
        geom_text(aes(x=date_reg+.5, y=age-.20, label=births))+
        geom_text(aes(x=max(date_reg)+1.25, y=age+.50, label=pop.total))+
        theme(axis.text.x = element_text(vjust=2.6))+
        theme(line = element_blank(),
              panel.background = element_blank(),
              text = element_text(size=14),
              plot.title = element_text(hjust = 0.5, face="plain"),
              plot.caption=element_text(hjust=0, vjust=0.5,
                                        margin=margin(t=1,10,10,10)),
              legend.position="none")+
        ggtitle(paste("Lexis diagram for period",
                      first.date,
                      "to",
                      final.date,
                      "for\nages",
                      ages[1],
                      "to",
                      ages[length(ages)]))+
        labs(y="Age",
             x="Date")

    deaths <- sum(d_births_data$deaths2, na.rm=TRUE)-red_ones-blue_ones

    list(diagram=diagram, deaths=d_births_data)
}
