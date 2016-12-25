
#' Reading Fatality Analysis Reporting System data
#'
#' This function reads a FARS dataset containig in csv file given in argument. If the file doesn't exist
#' the function stops and prints an arror message saying that the data doesn't exist.
#' If the data exist, the functions reads it and create a data frame from it.
#'
#'
#' @param filename The name of the csv.bz2 file which the data are read from.
#'
#' @return a data frame tbl
#' @examples
#'
#' acc2013 <- fars_read ("accident_2013.csv.bz2")
#'
#' @details
#' If the format of the file is not csv.bz2 or csv, the function returns an error message saying "invalid file argument"
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}





#' Returning a a FARS filename according a year entered as a parameter
#'
#' This function return a character vector containing a formatted combination of text and year value
#'
#' @param a Year
#'
#' @return a character vector
#' @examples
#'
#' name_2017 <- make_filename(2017)
#' name_2017
#' [1] "accident_2017.csv.bz2"
#'
#' @details
#' the file names created is in csv.bz2 format
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}







#' Creating a list of several FARS dataset
#'
#' This function creates a list of truncated FARS datatsets giving th considered years into a list.
#' Returned FARS datasets are truncated because they only conatines the variables year and month
#'
#'
#' @param a list of years
#'
#' @return a list of data frame tbl corresponding to the list of years entered in parameters
#' @examples
#'
#' dat_list <- fars_read_years(list(2013,2014,2015))
#'
#' @details
#'if a unknown year is given into the list of year, an error message is printed
#'indicating that the year is invalid. And so the corresponding element of the returned list is NULL
#'
#'
#' @import dplyr
#' @import tidyverse
#' @importFrom readr read_csv
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}




#' Summarizing the number of events by year and month in a tbl_df object
#'
#' This function creates a tabl_df object containing the numbers of events by year and month
#' contained in the different FARS datasets
#'
#' @param a list of years
#'
#' @return a tbl_object corresponding to the list of years entered in parameters
#' and containing the number of events for each month of each year
#' @examples
#'
#' MySummary<- fars_summarize_years(list(2013,2015))
#'
#' @details
#'if a unknown year is given into the list of year, an error message is printed
#'indicating that the year is invalid. And so the corresponding element of return table is empty
#'
#'
#' @importFrom dplyr bind_rows group_by summarize
#'@importFrom tidyr spread
#' @importFrom readr read_csv
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' Showing where accidents occured
#'
#' This function a graphic showing in a map where the accident occured given a state number and a year
#' @param the state number
#' @param the considered year

#'
#'
#' @return a graphical object

#' @examples
#'
#' MyMap<- fars_map_state(1,2013)
#'
#' @details
#'If the state number is wrong (>56) an error message is printed.
#'If the state considered contains no accident, a message is printed saying
#'there is no accident to plot
#'
#'
#' @import tidyverse
#' @import dplyr
#' @import maps
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
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
