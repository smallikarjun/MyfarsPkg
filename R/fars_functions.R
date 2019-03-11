#' Building R Packages - Week 2 Assignment
#' filename: fars_functions.R
#'
#' These functions read in data taken from the US National Highway Traffic Safety Administration's
#' \href{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{Fatality Analysis Reporting System}
#'
#' @title fars_read
#'
#' @description Reads in file to data variable using the read_csv function
#' and creates a dataframe summarizing the contents.
#'
#' @param filename A character object which corresponds to a valid path of the data file.
#' In case the file does not exist an error message is produced and execution stops.
#'
#' @return The function returns a dataframe based on the CSV file.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read(".inst/extdata/accident_2015.csv.bz2")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @title make_filename
#'
#' @description
#' The function creates a filename for a .csv.bz2 file based on the \code{year}
#' argument in a form "accident_<year>.csv.bz2". It requires a numerical or
#' integer input otherwise ends with an error.
#'
#' @param year Numerical or integer input indicating a year of a dataset
#'
#' @return Returns a character string in a format "accident_<year>.csv.bz2" that
#' can be used as a file name
#'
#' @examples
#' \dontrun{
#' make_filename(2015)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title fars_read_years
#'
#' @description
#' The function accepts a vector or list of years and returns a list of dataframe
#' with MONTH and year columns based on data in "accident_<year>.csv.bz2
#' files. The files need to be located in the working directory.
#'
#' @param years A vector or list of years in numeric or integer format
#'
#' @return Returns a list of dataframe with the same number of rows
#' as the data in "accident_<year>.csv.bz2" files sorted by year and MONTH.
#'
#' If any of the objects requested via input is not available as a year file
#' or is not coercible to integer an "invalid year" error message returns.
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' fars_read_years(list(2013, 2014))
#'
#' # Results in a warning
#' fars_read_years(2016)
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat,  year = "YEAR") %>%
        dplyr::select_("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' @title fars_summarize_years
#'
#' @description
#' takes a list of years and reads them in using fars_read_years
#' it then binds these dataframe together and summarizes the data.
#'
#' @param years A vector or list of years (numeric or integer) to
#' read in and summarize
#'
#' The function will take in a vector of years and read in the data using
#' the fars_summarize_years function, it then binds these rows together and
#' groups by the year and MONTH column creating a count column: n.
#' The data is then converted from a long format to a wide format using
#' the spread function in tidyr.
#'
#' @return a data.frame of summarized data which is converted to a wide format
#'
#' @importFrom dplyr bind_rows group_by summarize %>% n
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#'  fars_summarize_years("2015")
#'  fars_summarize_years(c(2013.0,2014))
#'  }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = "n()") %>%
    tidyr::spread_("year", "n")
}

#' @title fars_map_state
#'
#' @description
#' This function takes a state number and set of years as input and shows
#' an overview of fatalities on a map in that particular time period.
#'
#' Uses function make_filename and fars_read from the current package.
#' Removes coordinate outliers - longitude values greater than 900
#' and latitude values greater than 90 are removed.
#'
#' @param state.num The number of a state in the US as used in the FARS dataset
#' Should be numeric or integer.
#' @param year The year of analysis (numeric or integer)
#'
#' @return a graphical overview of fatalities on a map in a particular time period.
#' Returns an error if the state or year do not exist in the data set.
#'
#' @examples
#' \dontrun{
#' fars_map_state(45, 2015)
#'
#' # Results in an error
#' fars_map_state(45, 2016)
#' fars_map_state(60, 2015)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  if(!(state.num %in% unique(data$STATE))) {
    stop("invalid STATE number: ", state.num)
  }
  data.sub <- dplyr::filter_(data, .dots = paste0("STATE==", state.num))
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
