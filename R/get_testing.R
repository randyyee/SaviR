#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_testing
#' @description Get testing data from OWID and FIND.
#' It will output a cross-sectional dataset and a longitudinal dataset.
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' testing_df <- get_testing()}

get_testing <- function() {

  `%ni%` <- Negate(`%in%`)

  testing <-
    read.csv(
      "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
      stringsAsFactors = FALSE
    )

  testing$Date <- as.Date(testing$date)

  tstx <- testing %>%
    dplyr::filter(location %ni% c("World", "International")) %>%
    dplyr::mutate(
      ou_date_match = paste(iso_code, Date, sep = "_"),
      last30days    = dplyr::if_else(Sys.Date() - Date < 30, 1, 0),
      iso_code      = dplyr::if_else(location == "Kosovo", "XKX", iso_code)
    ) %>%
    dplyr::select(-Date) %>%
    dplyr::mutate(
      adj_total_cases = dplyr::if_else(!is.na(total_tests), total_cases, NA_real_),
      cvd_death_rate  = cardiovasc_death_rate
    ) %>%
    dplyr::rename(iso3code = iso_code)


  #get data from FIND and format in the same way as the OWID
  find <-
    readr::read_csv(
      "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv"
    )

  iso3 <- maps::iso3166 %>%
    dplyr::mutate(
      alpha3 = dplyr::if_else(ISOname == "Kosovo", "XKX", a3),
      unit   = dplyr::if_else(ISOname == "Namibia", "NA", a2)
    ) %>%
    dplyr::select(unit, alpha3)

  findlong <- dplyr::filter(find, set == "country") %>%
    dplyr::mutate(unit = dplyr::if_else(name == "Namibia", "NA", unit)) %>%
    dplyr::left_join(iso3) %>%
    dplyr::mutate(
      date = as.character(time),
      Date = as.Date(time),
      ou_date_match = paste(alpha3, Date, sep = "_"),
      last30days = dplyr::if_else(Sys.Date() - Date < 30, 1, 0)
    ) %>%
    #Rolling average to match OWID
    dplyr::arrange(unit, name, date) %>%
    dplyr::group_by(unit, name) %>%
    #if the case data haven't been updated for the current date yet, but there is testing data, drop the latest testing data until the case data get updated
    dplyr::filter(!(
      date == max(date) &
        !is.na(all_cum_tests) & is.na(all_cum_cases)
    )) %>%
    dplyr::mutate(
      adj_total_cases = dplyr::if_else(!is.na(all_cum_tests), all_cum_cases, NA_real_),
      new_tests_smoothed = pop_100k * cap_new_tests,
      new_tests_smoothed_per_thousand = 1000 * cap_new_tests,
      total_tests_per_thousand = 1000 * all_cum_tests / (100000 * pop_100k),
      new_tests_per_thousand = 1000 * new_tests_orig / (100000 * pop_100k),
      total_cases_per_million = 1000000 * all_cum_cases / (100000 * pop_100k),
      total_deaths_per_million = 1000000 * all_cum_deaths / (100000 * pop_100k),
      new_cases_per_million = 1000000 * new_cases_orig / (100000 * pop_100k),
      new_deaths = all_cum_deaths - lag(all_cum_deaths, n = 1),
      tests_per_case = new_tests_smoothed / new_tests_orig,
      new_deaths_per_million = 1000000 * new_deaths / (100000 * pop_100k),
      new_cases_smoothed = pop_100k * cap_new_cases,
      population = 100000 * pop_100k
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      total_cases = all_cum_cases,
      total_tests = all_cum_tests,
      total_deaths = all_cum_deaths,
      iso3code = alpha3,
      new_tests = new_tests_orig,
      new_cases = new_cases_orig
    ) %>%
    dplyr::select(
      -set,
      -name,
      -unit,
      -time,
      -pop_100k,
      -cap_cum_cases,
      -cap_new_cases,
      -cap_cum_deaths,
      -cap_new_deaths,
      -cap_cum_tests,
      -cap_new_tests,-all_new_cases,
      -all_new_deaths,
      -new_deaths_orig,
      -cap_cum_tests,
      -all_new_tests,
      -pos,
      -Date
    ) %>%
    dplyr::mutate(source = "FIND",
                  tests_units = "FIND data- Unknown")

  owid <- tstx %>%
    dplyr::select(
      iso3code:tests_units,
      population,
      ou_date_match,
      adj_total_cases,
      last30days,
      -location,
      -continent
    ) %>%
    dplyr::mutate(source = "OWID")

  owid_covs <- tstx %>%
    dplyr::select(
      ou_date_match,
      stringency_index,
      population_density:life_expectancy,
      cvd_death_rate
    ) %>%
    unique()

  testinglong <- dplyr::bind_rows(owid, findlong) %>%
    dplyr::left_join(owid_covs, by = "ou_date_match") %>%
    dplyr::mutate(ou_src_match = paste(iso3code, source, sep = "_"))

  testinglong <- testinglong %>%
    dplyr::mutate(perc_positive_testing = (new_cases_smoothed / new_tests_smoothed) * 100) %>%
    dplyr::mutate(tests_per_case = ifelse(( is.nan(tests_per_case) | is.infinite(tests_per_case)), NA, tests_per_case)) %>%
    dplyr::mutate(perc_positive_testing = ifelse((is.nan(perc_positive_testing) | is.infinite(perc_positive_testing)), NA, perc_positive_testing))


  #get cumulative testing results for just the past 30 days- number of tests, tests per pop, number of cases
  tst_cross30 <- testinglong %>%
    dplyr::filter(last30days == 1) %>%
    dplyr::group_by(ou_src_match, population) %>%
    dplyr::summarise(
      cum_tests_last_30 = sum(new_tests, na.rm = TRUE),
      cum_cases_last_30 = sum(new_cases, na.rm = TRUE)
    ) %>%
    dplyr::mutate(cum_tests_last_30_100thousand = 100000 * cum_tests_last_30 / population) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      ou_src_match,
      cum_tests_last_30,
      cum_cases_last_30,
      cum_tests_last_30_100thousand
    )

  #get summary statistics for cross-sectional reporting
  tst_cross <- testinglong %>%
    dplyr::filter(!is.na(adj_total_cases)) %>%
    dplyr::group_by(iso3code, source) %>%
    dplyr::summarise(
      last_date = max(date, na.rm = T),
      cum_cases = max(adj_total_cases, na.rm = T),
      cum_tests = max(total_tests, na.rm = T),
      cum_test_100thousand = max(total_tests_per_thousand, na.rm = T) * 100,
      cum_cases_100thousand = max(total_cases_per_million, na.rm = T) * 0.1,
      population = max(population)
    ) %>%
    dplyr::ungroup()

  #get the type of tests that have been reported (OWID only)
  test_type <- testinglong %>%
    dplyr::filter(!is.na(total_tests) & !is.na(tests_units)) %>%
    dplyr::arrange(ou_date_match, dplyr::desc(date)) %>%
    dplyr::group_by(iso3code, source) %>%
    dplyr::mutate(snum = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(snum == 1) %>%
    dplyr::select(iso3code, source, ou_src_match, tests_units)

  #get the last date that any new tests were reported in the data (FIND data sometimes has new cases but no new tests)
  tst_lastdate_newtests <- testinglong %>%
    dplyr::filter(!is.na(new_tests) & new_tests > 0) %>%
    dplyr::group_by(ou_src_match) %>%
    dplyr::summarise(last_date_new_tests = max(date, na.rm = T)) %>%
    dplyr::ungroup()


  tst_crossx1 <- dplyr::left_join(tst_cross, test_type) %>%
    dplyr::left_join(tst_cross30, by = "ou_src_match") %>%
    dplyr::left_join(tst_lastdate_newtests, by = "ou_src_match") %>%
    #if new tests weren't reported, use the last date with a total reported:
    dplyr::mutate(last_date_new_tests = dplyr::if_else(is.na(last_date_new_tests), last_date, last_date_new_tests))

  #NEW addition
  #add percent positive
  tst_percentpos <- testinglong %>%
    dplyr::filter(!(is.na(perc_positive_testing))) %>%
    dplyr::arrange(iso3code, date) %>%
    dplyr::group_by(ou_src_match) %>%
    dplyr::summarise(perc_positive_testing = dplyr::last(perc_positive_testing)) %>%
    dplyr::ungroup()

  tst_crossx <- dplyr::left_join(tst_crossx1, tst_percentpos)


  #get standardized country and continent names
  geodf <- countries_data %>%
    dplyr::select(iso3code, country, Continent_Name, who_region) %>%
    dplyr::rename(location = country,
                  continent = Continent_Name)

  tst_crossx <- dplyr::left_join(tst_crossx, geodf)

  testinglong <- dplyr::left_join(testinglong, geodf)

  return(list(testinglong, tst_crossx))
}
