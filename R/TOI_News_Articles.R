#'@title Extract Media News
#'
#'@description Creates a DataFrame or Write files to disk by extracting text
#'  data from source based on user's keywords.
#'
#'@param keywords A String, user-defined.
#'
#'@param IsDataFrame Boolean Value, to determine whether the outcome should be a
#'  Dataframe or files written to disk. if set to \emph{FALSE} then retuns the
#'  files will be written to disk at stated working directory (default
#'  \emph{TRUE}).
#'
#'@param IsDate Boolean Value, to determine whether to perform filtering
#'  operations based on dates. If set to \emph{TRUE} then Date operations will
#'  be applied (default \emph{FALSE}).
#'
#'@param start_date Date Value, provide the starting date \strong{FROM} where
#'  the data should be extracted. \strong{\emph{NOTE}}: only provide
#'  \code{start_date} when \code{IsDate} is set \emph{TRUE}.
#'
#'@param end_date Date Value, provide the ending date \strong{TO} where the data
#'  should be extracted. \strong{\emph{NOTE}}: only provide \code{end_date} when
#'  \code{IsDate} is set \emph{TRUE}.
#'
#'@return Returns DataFrame or write files to the disk based on \code{keywords}
#'
#'@author Vatsal Aima,  \email{vaima75@@hotmail.com}
#'
#'@import rvest
#'@import lubridate
#'@import svMisc
#'@import xml2
#'@importFrom stats na.omit
#'
#'@seealso \code{\link{TOI_News_Dataset}}
#'
#' @examples
#' #### Creates Dataset by filtering 31 days from current date
#'\donttest{
#' NewsDataset1 = TOI_News_Articles(keywords = "Politics In US",
#' IsDate = TRUE,
#' start_date = Sys.Date()- 31,
#' end_date = Sys.Date())
#'
#' # Creates Dataset by custom filtering through dates
#' NewsDataset2 = TOI_News_Articles(keywords = "BaseBall",
#' IsDate = TRUE,
#' start_date = "2019-09-20",
#' end_date = "2019-10-20")
#'
#' # Write files to disk
#' TOI_News_Articles(keywords = "Goibibo", IsDataFrame = FALSE)
#'}
#'@export TOI_News_Articles

TOI_News_Articles <- function(keywords, IsDataFrame = TRUE, IsDate = FALSE, start_date, end_date){
  message("\nPlease wait while I fetch pages based on keywords.....\n\n")
  dataset <- TOI_News_Dataset(keywords)
  dataset[,2] <- gsub("https://timesofindia.indiatimes.com/articleshow/",NA, dataset[,2])
  dataset[,3] <- as_date(as.character(dataset[,3]))
  dataset <- na.omit(dataset)

  ## Date-Time Parsing for specific Article Extraction
  if (IsDate == TRUE) {
    message("\nApplying filters....\n")
    Sys.sleep(3)
    dataset <- subset(dataset, dataset[,3] >= start_date & dataset[,3] <= end_date)
  }

  TOI_links <- dataset[,2]

  ## For append
  ExtractData <- list()

  # List of CSS selector
  art_ext_class <- c('.Normal','._3WlLe')
  ts_ext_class <- c(".byline", ".as_byline", ".byline-content")
  message("NewsLinks Fetched: ",length(TOI_links))
  if (length(TOI_links) <= 0) {
    message("\nExtraction HALT!!\n", "Kindly change the filters","\n          OR","Switch to new topic for extraction\n")

  }else{
    message("\n\n!...Extraction Begins...!\n\n")
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

      l_index <- which(TOI_links == web_url_link, arr.ind = T)
      if (IsDataFrame == TRUE) {
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

        # cat("\n#####################\n")
        # cat("Link ID : ", l_index,"\n")
        # cat("Link :", web_url_link,"\n")
        # cat("Data Length : ",length(data_text),"\n")
        # cat("Text Lenght : ",length(ts_text),"\n")
        text_dt <- data.frame(data_text, ts_text, stringsAsFactors = FALSE)
        ExtractData[[l_index]] <- text_dt

        progress(l_index, progress.bar = TRUE)
        Sys.sleep(0.01)
        if (l_index == length(TOI_links)) message("\nExtraction Done\n")

        if (web_url_link == TOI_links[length(TOI_links)]) {
          ExtractedDf <- do.call(rbind, ExtractData)
          dataset <- cbind(dataset, ExtractedDf)
          # dataset[[4]] <- I(ExtractData)
          colnames(dataset) <- c("HeadLines","Links","DOP","News","Details")
          message("\n!!!!! Dataset Extracted !!!!!")
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
