## MediaNews - Introduction
  
An implementation to perform analysis on different media channels by extracting textual data from its source, based on users choice of keywords. These data can be used to perform text analysis in order to identify patterns in respective media reporting. The media channels used in this package are print media from india. The data (or news) used are publically available to consumers.
#### Prerequisites

For this package to run into your system (R or RStudio) following packages are requried:

* rvest
* lubridate
* svMisc
* xml2
* stopwords

#### Installing

You can install the library as follows:

```r
### Install Prerequisites
pkgs <- c("rvest","lubridate","svMisc","xml2","stopwords")
install.packages(pkgs)
### Load Prerequisites - Not Requried. Installation well be enough 
lapply(pkgs, library, character.only = TRUE)

### Install package from GitHub
install.package("devtools") #Run only once
library(devtools)
install_github("vaima75/MediaNews") #Run only once
### Load the package
library(MediaNews)

```

## Running the tests

#### 1. Extraction

Following examples show how to extract text and get it in the form of DataFrame or write to disk

``` r
# Creates Dataset by filtering 31 days from current date
NewsDataset1 = TOI_News_Articles(keywords = "Politics In US", IsDate = TRUE, start_date = Sys.Date()- 31, end_date = Sys.Date())

# Creates Dataset by custom filtering through dates
NewsDataset2 = TOI_News_Articles(keywords = "BaseBall", IsDate = TRUE, start_date = "2019-09-20", end_date = "2019-10-20")

# Creates Dataset on keywords
NewsDataset3 = TOI_News_Articles(keywords = "Goibibo")

# Write files to disk
TOI_News_Articles(keywords = "Goibibo",  IsDataFrame = FALSE)
```

#### 2. Cleaning

After extraction data in the form of DataFrame you can use customized text cleaning function to remove unwanted text from body.

```r
## Creates Dataset based on keysword 
NewsData = TOI_News_Articles("Goibibo")

## Identify any potential factor columns
vc = sapply(NewsData, is.factor)

## Convert factors to characters
NewsData[vc] = lapply(NewsData[vc], as.character)

## Clean text on specific character columns
for (i in 1:nrow(NewsData)) NewsData$News[i] = ClearText(NewsData$News[i])

```
## Author
[Vatsal Aima](https://vaima75.github.io/)

## License

This project is licensed under the [GNU Lesser General Public License version 3](https://github.com/vaima75/MediaNews/blob/master/LICENSE).
