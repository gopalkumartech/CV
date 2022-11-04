####### pdf extract using R

library(pdftools)
pdf_file <- "https://github.com/ropensci/tabulizer/raw/master/inst/examples/data.pdf"
txt <- pdf_text(pdf_file)
cat(txt[1])


pdf_data(pdf_file)[[1]]         # All textboxes on page 1



#
pdf.file <- "https://file-examples-com.github.io/uploads/2017/10/file-sample_150kB.pdf"
# setwd("D:/RStudio/PDFEXTRACT/")
download.file(pdf.file, destfile = "sample.pdf", mode = "wb") # download sample file
pdf.text <- pdftools::pdf_text("sample.pdf")
cat(pdf.text[[2]])   # page 2 of pdf

# extract a particular word from these pages, unlist the data and convert it into lower case letters
pdf.text<-unlist(pdf.text)
pdf.text<-tolower(pdf.text)
library(stringr)
res<-data.frame(str_detect(pdf.text,"suspendisse")) # find The word “suspendisse” pages number
colnames(res)<-"Result"
res<-subset(res,res$Result==TRUE)
row.names(res)  # The word “suspendisse” contains on pages number 2 and 3.



###### scan from pdf
library(tesseract)
eng <- tesseract("eng")
text <- tesseract::ocr("http://jeroen.github.io/images/testocr.png", engine = eng)
cat(text)

#The ocr_data() function returns all words in the image along with a bounding box and confidence rate.
results <- tesseract::ocr_data("http://jeroen.github.io/images/testocr.png", engine = eng)
results

# install additional language training data using tesseract_download()
# eg dutch
# Only need to do download once:
tesseract_download("nld")
# Now load the dictionary
(dutch <- tesseract("nld"))

library(magick)
input <- image_read("https://jeroen.github.io/images/bowers.jpg")

text <- input %>%
    image_resize("2000x") %>%
    image_convert(type = 'Grayscale') %>%
    image_trim(fuzz = 40) %>%
    image_write(format = 'png', density = '300x300') %>%
    tesseract::ocr() 

cat(text)

######
#If your images are stored in PDF files they first need to be converted to a proper image format

pngfile <- pdftools::pdf_convert('https://jeroen.github.io/images/ocrscan.pdf', dpi = 600)

text <- tesseract::ocr(pngfile)
cat(text)
