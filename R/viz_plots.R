#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve
#' @description (EPI WEEK) Visualize epi curve by epi-weeks (Monday-Sunday) and by WHO region(s).
#'
#' @param df A dataframe with the following: region, country, date, new_cases.
#' For WHO default, region should be factors with levels of AMRO, EURO, SEARO, EMRO, AFRO, and WPRO.
#' Produces an epi curve, region stacked bar plot for each epi-week (Monday-Sunday).
#' @param transparent Default TRUE - returns a transparent plot.
#' @param who.col.pal Default color pallet for all regions. Specify specific region color for individual region.
#' @param regions Default all WHO regions. Specify specific region label for individual region.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

plot_epicurve <- function(df, region = "Global", transparent = T){

  region_abbv <- c("AMRO", "EURO", "SEARO", "EMRO", "AFRO", "WPRO")
  who.col.pal <- c("#aa001e", "#e7b351", "#00818a", "#d26230", "#005e70", "#d4ece8")
  names       <- c("Americas", "Europe", "Southeast Asia", "Eastern \nMediterranean", "Africa", "Western Pacific")
  col_master  <- data.frame(region_abbv, names, who.col.pal)

  if(length(unique(df$region)) > 1){
    regions      <- col_master$names
    pallete      <- col_master$who.col.pal
    gtitle       <- "Confirmed COVID-19 Cases by Week of Report and WHO Region"
    region_label <- "WHO Region"
    legend       <- "right"
  } else {
    regions      <- col_master[region_abbv == as.character(unique(df$who_region)), ]$names
    pallete      <- col_master[region_abbv == as.character(unique(df$who_region)), ]$who.col.pal
    gtitle       <- paste0("Confirmed COVID-19 Cases – ", r, " Region")
    region_label <- ""
    legend       <- "none"
  }

  g <- ggplot2::ggplot(data     = df,
                       mapping = aes(x    = lubridate::floor_date(date, "week", week_start = 1),
                                     y    = new_cases,
                                     fill = region)) +
    ggplot2::geom_bar(position = "stack",
                      stat     = "identity",
                      alpha    = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_manual(values = pallete,
                               labels = regions) +
    ggplot2::ylab("Weekly Cases") +
    ggplot2::xlab("Week of Reporting") +
    ggplot2::labs(fill = region_label) +
    ggplot2::scale_x_date(limits = c(lubridate::floor_date(min(df$date, na.rm = T)-7, "week", week_start = 1),
                                     lubridate::floor_date(max(df$date, na.rm = T)+7, "week", week_start = 1)),
                          breaks = seq.Date(from = as.Date(lubridate::floor_date(min(df$date, na.rm = T), "week", week_start = 1)),
                                            to   = as.Date(lubridate::floor_date(max(df$date, na.rm = T)+7, "week", week_start = 1)),
                                            by   = "3 weeks"),
                          date_labels = "%d\n%b",
                          expand      = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 5000),
                                labels = scales::comma) +
    ggplot2::labs(title    = gtitle,
                  subtitle = paste0(format(min(df$date, na.rm = T), "%B %d, %Y"), " - ",
                                    format(max(df$date, na.rm = T), "%B %d, %Y"))) +
    ggplot2::theme(plot.title      = ggplot2::element_text(size  = 18, face = "bold", family = "Calibri"),
                   axis.text       = ggplot2::element_text(size  = 10, family = "Calibri"),
                   axis.title      = ggplot2::element_text(size  = 12, family = "Calibri"),
                   legend.title    = ggplot2::element_text(size  = 12, face = "bold", family = "Calibri"),
                   legend.text     = ggplot2::element_text(size  = 9,  family = "Calibri"),
                   legend.position = legend) +
    ggplot2::guides(fill = ggplot2::guide_legend(overide.aex  = list(size = 9)))

  if(transparent == T){
    return(g +
             ggplot2::theme(panel.background  = ggplot2::element_rect(fill = "transparent"),
                            plot.background   = ggplot2::element_rect(fill = "transparent"),
                            panel.grid        = ggplot2::element_blank(),
                            legend.background = ggplot2::element_rect(fill = "transparent")))
  }else{
    return(g)
  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve_ind
#' @description (DAILY) Visualize epi curve by cases and deaths.
#' Default viz for individual countries.
#'
#' @param df A dataframe with the following: country, date, cases and/or deaths
#' @param type Default cases.
#' @param incidence Default TRUE. Specify inputs are incidence values or not.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

plot_epicurve_ind <- function(df, type = "cases", incidence = T){

  if(!type %in% c("cases", "deaths")){
    stop("Wrong Type! You must use either cases or deaths!")
  }

  if(!incidence %in% c(T, F)){
    stop("Wrong Incidence! You must use either TRUE or FALSE!")
  }

  if(incidence == F){
    df %>%
      ggplot2::ggplot(aes(x = date, y = if(type == "cases") {cases} else {deaths})) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.9, fill = if(type == "cases") {"dodgerblue4"} else {"red4"}) +
      ggplot2::theme_classic() +
      ggplot2::ylab(if(type == "cases") {"Daily Cases"} else {"Daily Deaths"}) +
      ggplot2::xlab("Date of Reporting") +
      ggplot2::scale_x_date(breaks       = c(by = "3 weeks"),
                            date_labels  = "%d\n%b") +
      ggplot2::scale_y_continuous(labels = comma) +
      ggplot2::labs(title    = if(type == "cases") {paste0("COVID-19 Cases: ", unique(df$country))} else {paste0("COVID-19 Deaths:", unique(df$country))},
                    subtitle = paste0(format(min(df$date, na.rm = T), "%B %d, %Y"), " - ", format(max(df$date, na.rm = T), "%B %d, %Y"))) +
      ggplot2::theme(plot.title   = ggplot2::element_text(size = 14, face = "bold", family = "Calibri"),
                     axis.text    = ggplot2::element_text(size = 10, family = "Calibri"),
                     axis.title   = ggplot2::element_text(size = 12, family = "Calibri"),
                     legend.title = ggplot2::element_text(size = 12, face = "bold", family = "Calibri"),
                     legend.text  = ggplot2::element_text(size = 9, family = "Calibri"))
  } else {
    df %>%
      ggplot2::ggplot(aes(x = date, y = if(type == "cases") {cases} else {deaths})) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.9, fill = if(type == "cases") {"dodgerblue4"} else {"red4"}) +
      ggplot2::theme_classic() +
      ggplot2::ylab(if(type == "cases") {"Daily Cases per 100,000 People"} else {"Daily Deaths per 100,000 People"}) +
      ggplot2::xlab("Date of Reporting") +
      ggplot2::scale_x_date(breaks       = c(by = "3 weeks"),
                            date_labels  = "%d\n%b") +
      ggplot2::scale_y_continuous(labels = comma) +
      ggplot2::labs(title    = if(type == "cases") {paste0("COVID-19 Cases per 100,000 People: ", unique(df$country))} else {paste0("COVID-19 Deaths per 100,000 People: ", unique(df$country))},
                    subtitle = paste0(format(min(df$date, na.rm = T), "%B %d, %Y"), " - ", format(max(df$date, na.rm = T), "%B %d, %Y"))) +
      ggplot2::theme(plot.title   = ggplot2::element_text(size = 14, face = "bold", family = "Calibri"),
                     axis.text    = ggplot2::element_text(size = 8,  family = "Calibri"),
                     axis.title   = ggplot2::element_text(size = 10, family = "Calibri"),
                     legend.title = ggplot2::element_text(size = 12, face = "bold", family = "Calibri"),
                     legend.text  = ggplot2::element_text(size = 9,  family = "Calibri"))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve_epidouble
#' @description (EPI WEEK) Visualize epi curve by cases and deaths.
#' Default viz for individual countries.
#'
#' @param df A dataframe with the following: country, weekdate, cases and deaths
#'
#' @importFrom magrittr `%>%`
#'
#' @export

plot_epicurve_epidouble <- function(df){

  ylim.prim <- c(min(df$case, na.rm = T),
                 max(df$case, na.rm = T))

  ylim.sec  <- c(min(df$death, na.rm = T),
                 max(df$death, na.rm = T))

  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b * ylim.sec[1]

  ggplot2::ggplot(df) +
    ggplot2::geom_bar(aes(x = weekdate, y = case, color = "Cases"), stat = "identity", alpha = 0.9, fill = "lightblue") +
    ggplot2::geom_line(aes(x = weekdate, y = a + death * b, group = 1, color = "Deaths"), size = 1) +
    ggplot2::scale_color_manual(breaks = c("Cases", "Deaths"),
                                values = c("lightblue", "red")) +
    ggplot2::theme_classic() +
    ggplot2::scale_x_date(breaks       = c(by = "4 weeks"),
                          date_labels  = "%d\n%b") +
    ggplot2::scale_y_continuous("Weekly Cases", labels = comma,
                                sec.axis = sec_axis(~ (. - a)/b, name = "Weekly Deaths", labels = comma)) +
    ggplot2::xlab("Date of Reporting") +
    ggplot2::labs(title    = paste0("COVID-19: ", unique(df$country)),
                  subtitle = paste0("Week of:", format(min(df$weekdate, na.rm = T), "%B %d, %Y"), " - ", format(max(df$weekdate, na.rm = T), "%B %d, %Y"))) +
    ggplot2::theme(plot.title      = ggplot2::element_text(size = 16, face = "bold", family = "Calibri"),
                   axis.text       = ggplot2::element_text(size = 14, family = "Calibri"),
                   axis.title      = ggplot2::element_text(size = 14, family = "Calibri"),
                   legend.position = "top",
                   legend.key      = element_blank(),
                   legend.title    = ggplot2::element_blank(),
                   legend.text     = ggplot2::element_text(size = 12, family = "Calibri"))  +
    ggplot2::guides(color          = ggplot2::guide_legend(override.aes = list(fill = c("lightblue", NA))))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve_dailydouble
#' @description (DAILY) Visualize epi curve by cases and deaths.
#' Default viz for individual countries.
#'
#' @param df A dataframe with the following: country, date, cases and deaths
#'
#' @importFrom magrittr `%>%`
#'
#' @export

plot_epicurve_dailydouble <- function(df){

  ylim.prim <- c(min(df$case, na.rm = T),
                 max(df$case, na.rm = T))

  ylim.sec  <- c(min(df$death, na.rm = T),
                 max(df$death, na.rm = T))

  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b * ylim.sec[1]

  ggplot2::ggplot(df) +
    ggplot2::geom_bar(aes(x = date, y = case, color = "Cases"), stat = "identity", alpha = 0.9, fill = "lightblue") +
    ggplot2::geom_line(aes(x = date, y = a + death * b, group = 1, color = "Deaths"), size = 1) +
    ggplot2::scale_color_manual(breaks = c("Cases", "Deaths"),
                                values = c("lightblue", "red")) +
    ggplot2::theme_classic() +
    ggplot2::scale_y_continuous("Cases", labels = comma,
                                sec.axis = sec_axis(~ (. - a)/b, name = "Deaths", labels = comma)) +
    ggplot2::xlab("Date of Reporting") +
    ggplot2::labs(title    = paste0("COVID-19: ", unique(df$country)),
                  subtitle = paste0("Week of:", format(min(df$weekdate, na.rm = T), "%B %d, %Y"), " - ", format(max(df$weekdate, na.rm = T), "%B %d, %Y"))) +
    ggplot2::theme(plot.title      = ggplot2::element_text(size = 16, face = "bold", family = "Calibri"),
                   axis.text       = ggplot2::element_text(size = 14, family = "Calibri"),
                   axis.title      = ggplot2::element_text(size = 14, family = "Calibri"),
                   legend.position = "top",
                   legend.key      = element_blank(),
                   legend.title    = ggplot2::element_blank(),
                   legend.text     = ggplot2::element_text(size = 12, family = "Calibri"))  +
    ggplot2::guides(color          = ggplot2::guide_legend(override.aes = list(fill = c("lightblue", NA))))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_riskmatrix
#' @description Plot risk matrix.
#' @param df A dataframe with riskmatrix stats.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

plot_riskmatrix <- function(df){
  ggplot2::ggplot(data = df, aes(x = percent_change_case, y = week_case_incidence)) +
    ggplot2::geom_point(aes(size = week_case, color = who_region), alpha=0.7) +
    ggplot2::scale_color_manual(values = c("#aa001e", "#e7b351", "#00818a", "#d26230", "#005e70", "#d4ece8"),
                                labels = c("Americas", "Europe", "Southeast Asia", "Eastern \nMediterranean", "Africa", "Western Pacific")) +
    ggrepel::geom_text_repel(aes(label = labels),
                             color              = 'black',
                             size               = 2.7,
                             min.segment.length = 0,
                             seed               = 42,
                             box.padding        = 0.6) +
    ggplot2::scale_size(name   = "Weekly Cases",
                        range  = c(2, 12),
                        breaks = c(100, 1000, 10000, 100000, 250000, 500000, 750000),
                        labels = scales::comma) +
    ggplot2::guides(color = guide_legend(override.aes = list(size = 6), order = 1)) +
    ggplot2::xlim(min(df$percent_change_case, na.rm = T), max(df$percent_change_case, na.rm = T)) +
    ggplot2::ylim(0,max(df$week_case_incidence, na.rm = T)) +
    ggplot2::xlab("% Change in Weekly Cases") + labs(color="WHO Region")+
    ggplot2::ylab("Average Daily Incidence per 100,000") +
    ggplot2::geom_vline(xintercept = 0,    color = 'gray50',    lty = 2) +
    ggplot2::geom_hline(yintercept = 0,    color = "green3",    linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 1.0,  color = "goldenrod1",linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 10.0, color = "orange2",   linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 25.0, color = "red3",      linetype = "dashed") +
    ggplot2::annotate(geom = "text", x = -133, y = 0.6,  label = "< 1.0 per 100k",       color = "green3",    size = 3)+
    ggplot2::annotate(geom = "text", x = -125, y = 1.7,  label = "1.0 - 9.9 per 100k",   color = "goldenrod1",size = 3)+
    ggplot2::annotate(geom = "text", x = -122, y = 10.7, label = "10.0 - 24.9 per 100k", color = "orange2",   size = 3)+
    ggplot2::annotate(geom = "text", x = -133, y = 25.7, label = "25.0+ per 100k",       color = "red3",      size = 3)+
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text     = element_text(size = 8, family = "Calibri"),
                   axis.title    = element_text(size = 10, family = "Calibri"),
                   legend.text   = element_text(size = 7, family = "Calibri"),
                   legend.title  = element_text(size = 9, family = "Calibri"),
                   plot.title    = element_text(size = 16, face = "bold", family = "Calibri"),
                   plot.subtitle = element_text(size = 11, family = "Calibri"),
                   plot.margin   = unit(c(1,1.6,1.3,1),"cm"),
                   plot.caption  = element_text(hjust = 0, size = 11, family = "Calibri")) +
    ggplot2::labs(title    = "Burden and Recent Trends",
                  subtitle = paste0("Average daily incidence per 100,000 population and 7-day percent change, by new cases in past 7 days\n",
                                    format(max(df$date)-13, "%B %d, %Y"), ' - ', format(max(df$date)-7, "%B %d, %Y"), ' to ',
                                    format(max(df$date)-6, "%B %d, %Y"), ' - ', format(max(df$date), "%B %d, %Y")),
                  caption  = "
         \nNotes:
         -Includes countries with a population greater than 10 million people and more than 100 cases in the last week
         -Countries with a population over 10 million are labeled if they are among the top ten highest countries for cases,
         incidence, or weekly percent change in cases.")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_vaxcoverage
#' @description Plot vaccination coverage by region.
#' @param df A dataframe with vaccination stats.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

plot_vaxcoverage <- function(df){

  my_pal_vax <- function(range = c(3, 25)) {
    force(range)
    function(x) scales::rescale(x, to = range, from = c(0, 1))
  }

  ggplot2::ggplot(df, aes(x = people_vaccinated_per_hundred, y = who_region)) +
    ggplot2::geom_point(aes(size = total_vaccinations, fill = who_region),
                        shape = 21,
                        color = 'gray60',
                        alpha = 0.6) +
    ggrepel::geom_text_repel(aes(label = labels, point.size = total_vaccinations),
                             color              = "gray25",
                             min.segment.length = 0,
                             max.overlaps       = Inf,
                             size               = 3,
                             force              = 0.7,
                             force_pull         = 0.7,
                             direction          = "both",
                             box.padding        = 0.4,
                             point.padding      = 0)+
    ggplot2::continuous_scale(aesthetics = c("size", "point.size"), scale_name = "size", palette = my_pal_vax(),
                              labels = scales::comma, breaks = c(1000000, 50000000, 300000000, 750000000),
                              guide  = guide_legend(override.aes = list(label = "")),
                              name   = "Total vaccine \ndoses administered") +
    ggplot2::scale_fill_brewer(name  = "WHO Region", palette = "Set1") +
    ggplot2::scale_x_continuous(name = "People Vaccinated per 100") +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = 8))) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste0("People Vaccinated per 100 people by WHO Region, ", format(max(df$date), "%B %d, %Y")),
                  caption = "
         \nNotes:
      -People Vaccinated per 100: number of people who received at least one vaccine dose; does not represent
      percent of population fully vaccinated
      -Total vaccine doses administered: total doses given, does not represent number of people vaccinated
      -Countries are labeled such that within each WHO Region, labeled countries are those that are the top 3 ranking countries
      for people vaccinated per 100 and the top 3 ranking countries for total vaccine doses administered
      -Vaccine data are incomplete and data may be out of date",
                  legend.title  = element_text(size = 10, face = "bold", family = "Calibri")) +
    ggplot2::theme(plot.title   = element_text(size = 14, face = "bold", family = "Calibri"),
                   axis.title   = element_text(size = 12, family = "Calibri"),
                   plot.caption = element_text(hjust = 0, size = 12, family = "Calibri"))
}
