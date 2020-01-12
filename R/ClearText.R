#'@title Text Cleaning: Custom Method
#'
#'@description Cleans text and introduce custom stopwords to remove unwanted
#'  words from given data.
#'
#'@param Text A String or Character vector, user-defined.
#'
#'@param CustomList A Character vector (Optional), user-defined vector to
#'  introduce stopwords ("english") in \code{Text}.
#'
#'@return Returns Character
#'
#'@author Vatsal Aima,  \email{vaima75@@hotmail.com}
#'
#'@import stopwords
#'
#'@seealso \code{\link{TOI_News_Articles}},  \code{\link{TOI_News_Dataset}}
#'
#' @examples
#' ################### Methodology #####################
#' ###### For DataFrame ######
#' #### Creates Dataset based on keysword
#'\donttest{
#' NewsData = TOI_News_Articles("Goibibo")
#'
#' ## Identify any potential factor columns
#' vc = sapply(NewsData, is.factor)
#'
#' ## Convert factors to characters
#' NewsData[vc] = lapply(NewsData[vc], as.character)
#'
#' ## Clean text on specific character columns
#' for (i in 1:nrow(NewsData)) NewsData$News[i] = ClearText(NewsData$News[i])
#'}
#' ######## For Character Variable #### Ex2 ####
#'
#' para = "Moreover, the text data we get is noisy. But, if we can learn some
#' methods useful to extract important features from the noisy data, wouldn't
#' scandal that be amazing ? In this tuto23rial, you'll saadc@@ruby.com
#' learn #world all ab33out regu12lar expressions from scratch. At first, 32324
#' detective you might find these confusing, or complicated, but after
#' https://anaconda.com/anaconda-enters-new-chapter/ expressions tricky,
#' scooby-doo doing practical hands-on exercises (done below)
#' you should feel bcc: @@MikeQuindazzi quite comfortable with it.
#' In addition, we'll also cartoon-network learn about string 121manipulation
#' functions in R. This formidable combination of #DL #4IR #Robots
#' #ArtificialIntelligence string manipulation functions and regular
#' expressions will prepare you for text mining."
#'
#' clearpara = ClearText(para,
#'                        CustomList = c("scooby-doo",
#'                                       "cartoon-network",
#'                                        "detective",
#'                                        "scandal"))
#'########### For List #############
#'\donttest{
#' paraList = list(para, 1213, factor('aasd;kasdioasd'))
#' paraList = lapply(paraList, as.character)
#' for (x in 1:length(paraList)) paraList[[x]] = ClearText(paraList[[x]])
#'}
#'@export ClearText

ClearText = function(Text, CustomList = c("")){

  ## Custom Functions
  removeHT <- function(x) gsub("\\B[#@]\\S+\\b"," ",x)
  removeURL <- function(x) gsub("http[^[:blank:]]+"," ",x)
  removeEMAILS <- function(x) gsub("\\S*@\\S*"," ",x)
  removeHTMLtags <- function(x) gsub("<.*?>"," ",x)
  removeWORDS <- function(xstr){
    xstr = unlist(strsplit(xstr, " "))
    JS = c("boolean","byte", "const", "eval", "goto", "instanceof", "int", "null", "static", "typeof", "var", "void", "enum")
    CList = append(JS, CustomList)
    en_stopwords = append(CList, stopwords(source = "smart"))
    en_stopwords = unique(en_stopwords)
    paste(xstr[!xstr %in% en_stopwords], collapse = " ")
  }
  removeWORDS2 <- function(x) gsub("\\b[A-z]{1,2}\\b"," ",x)
  removeWORDS46 <- function(x) gsub("\\b[A-z]{46,}\\b"," ",x)
  removeSPACES <- function(x) gsub("[[:space:]]+"," ",x)
  removeNONA <- function(x) gsub("[^[:alnum:]]+", " ", x)
  removePUNC <- function(x) gsub("[[:punct:]]+", " ", x)
  removeNUMS <- function(x) gsub("[[:digit:]]","",x)

  ## Function Execution
  if (class(Text) == "character") {
    Text <- tolower(Text)
    Text <- removeURL(Text)
    Text <- removeEMAILS(Text)
    Text <- removeHTMLtags(Text)
    Text <- removeHT(Text)
    Text <- removeWORDS(Text)
    Text <- removePUNC(Text)
    Text <- removeNONA(Text)
    Text <- removeNUMS(Text)
    Text <- removeWORDS2(Text)
    Text <- removeWORDS46(Text)
    Text <- removeSPACES(Text)
    Text <- removeWORDS(Text)
    Text <- trimws(Text)
    return(Text)
  }else{
    message("\nErr! Is it character variable you passed. \n", fill = TRUE)
  }
}
