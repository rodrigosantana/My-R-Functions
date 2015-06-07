########################################################################
## Description: Function developed to download, unzip and load datasets
## in R.
## 
## Maintainer: Rodrigo Sant'Ana
## Author: Rodrigo Sant Ana
## Created: Sáb Jun  6 21:12:43 2015 (-0300)
## Version: 0.0.1
## Last-Updated: Sáb Jun  6 21:16:14 2015 (-0300)
##           By: Rodrigo Sant Ana
## 
## URL: github.com/rodrigosantana
## Doc URL: github.com/rodrigosantana/My-R-Functions
## 
## Database info: 
## 
### Commentary: 
## 
### Code:
########################################################################

### Funtion to unzip and import data files...
read.webzip <- function(url, header = TRUE, dec = ".", sep = ";") {
    # Creating a folder to storage the data files...
    print("Creating data folder!!!")
    if(file.exists("data")) {
        print("Folder called data already exist")
    } else {
        dir.create("data")
    }
    # Downloading the data file in data folder...
    print("Downloading dataset!!!")
    download.file(url, destfile = "data/Dataset.zip", method = "curl")
    # Set data's folder as path ...
    path <- "data"
    # Unzip files to the choose directory...
    print("Unzipping data file!!!")
    unzip("data/Dataset.zip", exdir = path)
    # Removing zip file...
    file.remove("data/Dataset.zip")
    # List files inside the directory ...
    files <- list.files(path)
    # Import data bases ... If the list of files have only one file, the
    # function import this file ... if the list have more than one file,
    # these function imports all bases to R ...
    # Importing one file ...
    if(length(files) == 1) {
        print("Importing one archive!")
    # Setting the complete link for the file...
        file <- paste(path, files[1], sep = "/")
    # Reading file...
        dados <- read.table(file, header = header, dec = dec, sep = sep,
                            quote = "", stringsAsFactors = FALSE,
                            na.strings = c("NA", "", "?"))
    # Importing more than one file ...
    } else if(length(files) > 1) {
        print("Importing more than one file!")
        dados <- list(NULL)
        for(i in 1:length(files)) {
            file <- paste(path, files[i], sep = "/")
            dados[[i]] <- read.table(file, header = header, sep = sep,
                                     dec = dec, quote = "",
                                     stringsAsFactors = FALSE,
                                     na.strings = c("NA", "", "?"))
        }
    # Empty zip files - returning error ...
    } else { print("The zip file is empty!") }
    return(invisible(dados))
}

########################################################################
## 
## The MIT License (MIT)
## 
## Copyright (c) 2014 Rodrigo Sant'Ana
## 
## Permission is hereby granted, free of charge, to any person 
## obtaining a copy of this software and associated documentation 
## files (the 'Software'), to deal in the Software without 
## restriction, including without limitation the rights to use, 
## copy, modify, merge, publish, distribute, sublicense, and/or 
## sell copies of the Software, and to permit persons to whom the 
## Software is furnished to do so, subject to the following 
## conditions: 
## 
## The above copyright notice and this permission notice shall be 
## included in all copies or substantial portions of the Software.
## 
## THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, 
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
## OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS 
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN 
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
## SOFTWARE.
## 
########################################################################
