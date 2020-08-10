#' Read in data.
#'
#' This function is given a filename that has both path and name of dataset to download using read_csv function.
#' The function first checks if the data exist, provides an error message if they do not, and downloads the data
#' if they do. The data object is in the form of data frame. Note that dplyr::tbl_df should be tibble::as_tibble
#' as dplyr::tbl_df has become obsolete. This changes import from dplyr to tibble for this function.
#'
#' @param filename A character string giving the path and name of data set to download.
#' @return This function either prints an error message or returns a data frame object depending on whether the data set is
#' found (and can be downloaded).
#' @import readr
#' @import dplyr
#' @import tibble
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}
#' Make a filename.
#'
#' This function returns a filename in C-style string formatting from a character or integer year given by the user and a pre set file name.
#' If the year is provided as a character string, the function will convert it to integer before returning the filename.
#'
#' @param year A character string or an integer provided by user that specifies the year of the file name.
#' @return A character string of a pre set name and user given year.
#' @examples
#' make_filename(2014)
#' make_filename("2014")
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' Download multiple files.
#'
#' This function will download several years of data on one go from a list of years given by user.
#' In case of any error, the function prints warnings of invalid years and returns NULL.
#'
#' @param years A vector or list of years given by user that specifies files to download.
#' @return A list of data frames containing data of specified years. In case of invalid year a warning and a NULL.
#' @import dplyr
#' @import tidyr
#' @examples
#' \dontrun{
#' fars_read_years(c("2013","2014","2015"))
#' fars_read_years(c(2013,2014,2015))
#' }
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
#' Summarizes data.
#'
#' This function reads given years of data and returns a summary in data frame format of n by years and months.
#'
#' @param years A vector or list of years to download and summarize.
#' @return a summary in data frame format of n by years and months.
#' @import dplyr
#' @import tidyr
#' @examples
#' \dontrun{
#' fars_summarize_years(c("2013","2014","2015"))
#' fars_summarize_years(c(2013,2014,2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' Road accident map by state.
#'
#' This function prints a map of road accidents for a state and year(s) given by users.
#'
#' @param state.num Integer or character
#' @param year Integer or character
#' @return Prints invalid state number if state number does not exist or 'no accidents'
#' if accidents don't exist, and a map if both statements are valid and all other
#' parameters exist.
#' @import dplyr
#' @import maps
#' @import graphics
#' @examples
#' \dontrun{
#' fars_map_state(1,"2014")
#' fars_map_state(1,2014)
#' }
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
