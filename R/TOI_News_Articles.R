#'@title Extract Media News
#'
#'@description Creates a DataFrame or Write files to disk by extracting text
#'  data from source based on user's keywords.
#'
#'@param keywords A String, user-defined.
#'
#'@param AsDataFrame Boolean Value, to determine whether the outcome should be a
#'  Dataframe or files written to disk. if set to \emph{FALSE} then retuns the
#'  files will be written to disk at stated working directory (default
#'  \emph{TRUE}).
#'
#'@param start_date Date (Character) Value, provide the starting date \strong{FROM} where
#'  the data should be extracted. \strong{\emph{NOTE}}: only provide
#'  \code{start_date} when \code{IsDate} is set \emph{TRUE}.
#'
#'@param end_date Date (Character) Value, provide the ending date \strong{TO} where the data
#'  should be extracted. \strong{\emph{NOTE}}: only provide \code{end_date} when
#'  \code{IsDate} is set \emph{TRUE}.
#'
#'@return Returns DataFrame or write files to the disk based on \code{keywords}
#'
#'@author Vatsal Aima,  \email{vaima75@@hotmail.com}
#'
#'@import rvest
#'@import xml2
#'@import lubridate
#'@importFrom stats na.omit
#'@importFrom utils winProgressBar setWinProgressBar
#'
#'@seealso \code{\link{TOI_News_Dataset}}
#'
#' @examples
#' #### Creates Dataset by filtering 31 days from current date
#'\donttest{
#'
#' # Creates Dataset by custom filtering through dates
#' NewsDataset = TOI_News_Articles(keywords = "BaseBall",
#' start_date = "2019-09-20",
#' end_date = "2019-10-20")
#'
#' # Write files to disk
#' TOI_News_Articles(keywords = "AirLines", AsDataFrame = FALSE)
#'}
#'@export TOI_News_Articles

TOI_News_Articles <- function(keywords, AsDataFrame = TRUE, start_date = NULL, end_date = NULL){
  dataset <- TOI_News_Dataset(keywords)
  dataset[,2] <- gsub("https://timesofindia.indiatimes.com/articleshow/",NA, dataset[,2])
  dataset[,3] <- as_date(as.character(dataset[,3]))
  dataset <- na.omit(dataset)

  ## Date-Time Parsing for specific Article Extraction
  if (!is.null(start_date) && !is.null(end_date)) {
    message("\nApplying filters by start-end dates....\n")
    Sys.sleep(2)
    dataset <- subset(dataset, dataset[,3] >= start_date & dataset[,3] <= end_date)
  }else if (!is.null(start_date)) {
    message("\nApplying filters by start date....\n")
    Sys.sleep(2)
    dataset <- subset(dataset, dataset[,3] >= start_date)
  }else if (!is.null(end_date)) {
    message("\nApplying filters by end date....\n")
    Sys.sleep(2)
    dataset <- subset(dataset, dataset[,3] <= end_date)
  }


  TOI_links <- dataset[,2]

  ## For append
  ExtractData <- list()

  # List of CSS selector
  art_ext_class <- c('.Normal','._3WlLe','.ga-headlines')
  ts_ext_class <- c(".byline", ".as_byline", ".byline-content")
  message("NewsLinks Fetched: ",length(TOI_links))
  if (length(TOI_links) <= 0) {
    stop("HALT!! Kindly change the filters or topic for extraction")

  }else{
    message("\n\n!...Extraction Begins...!\n\n")
    pb <- winProgressBar(title = "Extracting...", min = 0, max = length(TOI_links), width = 250)
    for (web_url_link in TOI_links){
      # Reading the HTML code from the website
      webpage <- read_html(web_url_link)

      # Scrap the article Details by CSS selector
      for (tse_class in ts_ext_class) {
        ts_html <- html_node(webpage, tse_class)
        if (length(ts_html) > 0) {
          break()
        }
      }

      # Scrap the article section for each CSS selector
      for (ae_class in art_ext_class) {
        data_html <- html_nodes(webpage, ae_class)
        if (length(data_html) > 0) {
          break()
        }
      }

      # Converting the data to text
      data_text <- html_text(data_html)
      ts_text <- html_text(ts_html)

      if (AsDataFrame == TRUE) {
        if (length(data_text) == 0 && length(ts_text) == 0) {
          data_text <- NA
          ts_text <- NA
        } else if (length(data_text) == 0 && length(ts_text) != 0){
          data_text <- NA
        } else if (length(data_text) != 0 && length(ts_text) == 0){
          ts_text <- NA
        } else if (length(data_text) > 1 && length(ts_text) > 1){
          data_text <- paste0(data_text, collapse = " ")
          ts_text <- paste0(ts_text, collapse = " ")
        } else if(length(data_text) > 1){
          data_text <- paste0(data_text, collapse = " ")
        } else if(length(ts_text) > 1){
          ts_text <- paste0(ts_text, collapse = " ")
        } else{
          data_text <- data_text
          ts_text <- ts_text
        }

        l_index <- which(TOI_links == web_url_link, arr.ind = T)

        text_dt <- data.frame(data_text, ts_text, stringsAsFactors = FALSE)
        ExtractData[[l_index]] <- text_dt

        setWinProgressBar(pb, l_index, title = paste0( round(l_index/length(TOI_links)*100), "% completed"))
        Sys.sleep(.01)

        if (l_index == length(TOI_links)){
          close(pb)
          ExtractedDf <- do.call(rbind, ExtractData)
          dataset <- cbind(dataset, ExtractedDf)
          colnames(dataset) <- c("HeadLines","Links","DOP","News","Details")
          message("\014!!!!! Dataset Extracted !!!!!")
          return(dataset)
        }

      }else{
        message("\n Writing Records.....\n")
        write(data_text, paste0(as.character(l_index),"_TOI_text.txt"))
        if (web_url_link == TOI_links[length(TOI_links)]) {
          message("\n!!! Records Written in File System !!!", fill = TRUE)
        }
      }
    }
  }
}
