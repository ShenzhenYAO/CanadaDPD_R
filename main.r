    # based on R 3.6.1

    #remove all objects from the memory
    #example: 
    rm(list=ls())

    # Prevent the default behavior of R that, in dataframe, converting character values into factors
    options(stringsAsFactors = FALSE)

    # loading self-developped r functions
    source('tools.r')

    # load a package to deal with JSON objects 
    # do not use the package 'rjson'. It cannot handle df with nested lists
    library('jsonlite')
    # load a package to save and extract files from zips
    # install.packages('zip')
    library('zip')

    # install.packages('gtools') # for define macros
    # library('gtools')
    #note: dplyr, not dbplyr!
    #install.packages('dplyr')
    #library('dplyr')

    #initializae the dpd.list
    dpd.list <- list()

    # define name_thezip
    indexcolname <- 'DRUG_CODE'

    # define the names of the zip files
    zipnames <- c('allfiles.zip', 'allfiles_ap.zip', 'allfiles_dr.zip','allfiles_ia.zip')
    
    # loop for the four zipfiles, get files, and save as 'data/dpdjson.zip'    
    for (thezipname in zipnames) {
        #unzip all tables, merge as a big zip, and add to dpd.json
        dpd.list <- zip2dfjson_dpd_my(thezipname, dpd.list, indexcolname )
        alarm() 
    }


    # zip the json file
    # need to install.packages('zip'), and library('zip')
    zipfile <- 'data/dpdjson.zip'
    files <- 'data/dpd.json'
    # https://cran.r-project.org/web/packages/zip/zip.pdf
    zip(zipfile, files )

    # need to install.packages('zip'), and library('zip')
    # unzip the json file (note, in the zip, the file name is with path)
    # it recognizes 'data/dpd.json', but not 'dpd.json'
    unzip(zipfile, files = 'data/dpd.json', overwrite = TRUE, junkpaths = FALSE,exdir = ".")

    # read the dpd.json into a list with 4 dfs
    dpdfromjsonfile.list <- json2dfs_dpd_my()








