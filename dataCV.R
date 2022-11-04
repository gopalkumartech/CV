# install.packages("devtools")
devtools::install_github("nstrayer/datadrivencv")


# run ?datadrivencv::use_datadriven_cv to see more details
datadrivencv::use_datadriven_cv(
    full_name = "gk Strayer",
    #data_location = "https://docs.google.com/spreadsheets/d/14MQICF2F8-vf8CKPF1m4lyGKO6_thG-4aSwat1e2TWc",
    #pdf_location = "https://github.com/nstrayer/cv/raw/master/strayer_cv.pdf",
    #html_location = "nickstrayer.me/cv/",
    #source_location = "https://github.com/nstrayer/cv"
)

datadrivencv::use_datadriven_cv(full_name = "My Name")


?use_datadriven_cv
use_datadriven_cv
datadrivencv::use_datadriven_cv()


#full_name	Your full name, used in title of document and header
#data_location	Path of the spreadsheets holding all your data. This can be either a URL to a google sheet with multiple sheets containing the four data types or a path to a folder containing four .csvs with the neccesary data.
#pdf_location	What location will the PDF of this CV be hosted at?
#html_location	What location will the HTML version of this CV be hosted at?
#source_location	Where is the code to build your CV hosted?
#open_files	Should the added files be opened after creation?
#use_network_logo	Should log

# This code is all that’s needed to setup a full CV. It outputs five files:
#File	Description
#cv.Rmd	An RMarkdown file with various sections filled in. Edit this to fit your personal needs.
#dd_cv.css	A custom set of CSS styles that build on the default Pagedown “resume” template. Again, edit these as desired.
#render_cv.R	Use this script to build your CV in both PDF and HTML at the same time.



# Knit the HTML version
rmarkdown::render("cv.Rmd",
                  params = list(pdf_mode = FALSE),
                  output_file = "cv.html")

# Knit the PDF version to temporary html location
tmp_html_cv_loc <- fs::file_temp(ext = ".html")
rmarkdown::render("cv.Rmd",
                  params = list(pdf_mode = TRUE),
                  output_file = tmp_html_cv_loc)

# Convert to PDF using Pagedown
pagedown::chrome_print(input = tmp_html_cv_loc,
                       output = "cv.pdf")




#CV_printing_functions.R	A series of functions that perform the dirty work of turning your spreadsheet data into 
