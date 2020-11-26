#'@title Creates Interim Dataset
#'
#'@description Creates an interim news dataset based on user-defined keywords
#'  for all possible links extracted from respective source.
#'
#'@param keywords A String, user-defined.
#'
#'@return Returns DataFrame based on \code{keywords}
#'
#'@author Vatsal Aima,  \email{vaima75@@hotmail.com}
#'
#'@import rvest
#'@import xml2
#'@import lubridate
#'@importFrom stats na.omit
#'@importFrom utils winProgressBar setWinProgressBar
#'
#'@seealso \code{\link{TOI_News_Articles}}
#'
#' @examples
#' #### Creates Dataset based on keysword
#' \donttest{
#' NewsData = TOI_News_Dataset("AirLines")
#'}
#'@export TOI_News_Dataset

TOI_News_Dataset <- function(keywords){
  message("Extracting Pages....")
  url_link_list <- TOI_News_Links(keywords)
  datalist <- list()
  for (link in url_link_list) {
    # Reading the HTML code from the website
    newss <- read_html(link)
    # List ID
    link_index <- which(url_link_list == link, arr.ind = TRUE)

    # Using CSS selectors to scrap links
    news_links <- html_nodes(newss, '.article .content a')
    html_links <- html_attr(news_links, 'href')
    html_links <- paste0("https://timesofindia.indiatimes.com", html_links)

    # Using CSS selectors to scrap the section
    headsLines <- html_nodes(newss,'.article .content a .title')

    # Converting the data to text
    news_headsLines <- html_text(headsLines)

    # Using CSS selectors to scrap the section
    TimeLines <- html_nodes(newss,'.meta')

    # Converting date to text
    news_TimeLines <- html_text(TimeLines)

    if (length(html_links) == 0 || length(news_headsLines) == 0 || length(TimeLines) == 0) {
      break()
    }
    # Make a Dataframe
    news_TOI <- data.frame(news_headsLines, html_links, news_TimeLines, stringsAsFactors = FALSE)

    # List of Dataframes
    datalist[[link_index]] <- news_TOI

    if (link_index == length(url_link_list)) message("Pages Fetched !!")
  }
  news_TOI_bigdata <- do.call(rbind, datalist)
  news_TOI_bigdata <- news_TOI_bigdata[!duplicated(news_TOI_bigdata[,1]),]
  news_TOI_bigdata[,3] <- gsub("T(.*?)Z","",news_TOI_bigdata[,3])
  news_TOI_bigdata[,3] <- parse_date_time(news_TOI_bigdata[,3],
                                          orders = c("%d %b %Y","%Y-%m-%d"),
                                          locale = "eng")
  return(news_TOI_bigdata)
}

#'@describeIn TOI_News_Dataset Extracts Source Links
TOI_News_Links <- function(keywords){
  keys <- gsub(" ","-",keywords)
  TOI_link <- "https://timesofindia.indiatimes.com"
  url_link <- paste0(TOI_link, "/topic/", keys)

  # Reading the HTML code from the website
  newss <- read_html(url_link)

  # Using CSS selectors to scrap the section
  url_section <- html_nodes(newss,'.pagination a')

  # Using CSS selectors to scrap links
  html_links <- html_attr(url_section, 'href')
  html_links <- paste0(TOI_link, html_links)
  html_links <- html_links[-c(1, length(html_links))]

  if (length(html_links) == 0) {
    return(url_link)
  }else{
    return(html_links)
  }
}
