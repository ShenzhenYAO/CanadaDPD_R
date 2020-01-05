    #remove all objects from the memory
    #example: 
    rm(list=ls())

    options(stringsAsFactors = FALSE)

    source('tools.r')
    # do not use the package 'rjson'. It cannot handle df with nested lists
    library('jsonlite')
    # install.packages('gtools') # for define macros
    # library('gtools')
    #note: dplyr, not dbplyr!
    #install.packages('dplyr')
    #library('dplyr')

    # define name_thezip
    name_thezip <-  'allfiles.zip'
    indexcolname <- 'DRUG_CODE'
    # download the zip file
    tmpzip <- downloadzip(name_thezip)

    # make a dataframe with tablename, varname, and vartype of all tables 
    tables.df <- maketableinfo_df_my()

    # get unique table names
    # get a vector of all values in the column 'table' as a vector
    tablenames.vector <- tables.df[,'table']
    # from above, get the unique values
    uniquetablenames.vector = unique(tablenames.vector)

    # drop the tablename 'inactive' if the data zip is not for inactive drugs
    if (name_thezip != 'allfiles_ia.zip') {
        uniquetablenames.list <-lapply(uniquetablenames.vector, function(x){x[!x == "inactive"]})
        #Note: lapply() results in a list, need to convert it back to a vector
        uniquetablenames.vector = unlist(uniquetablenames.list)
        uniquetablenames.vector
    }

    # loop for each table name, import data into dfs named by the tablename (e.g.,thedata_drug.df )
    for (thetablename in uniquetablenames.vector) {
        # print(thetablename)
        
        #determine the name of the df
        datadfnamestr=paste0(thetablename, '.df')
        # get the data and same into the df named by the string 'datadfnamestr'
        # note: not to save the df to 'datadfnamestr', but to save the df to a df named by the str value of 'datadfnamestr'
        # e.g., datadfnamestr <- 'thedata_drug.df', the assign() saves the data to thedata_drug.df
        assign(datadfnamestr, getDataOfATableInAZip_df(thetablename, tmpzip))
        # display the head rows of the df thedata_drug.df
        head(get(datadfnamestr))

        # get the # rows of the df
        nrows<- nrow(get(datadfnamestr))
        print(paste0('the data frame ',datadfnamestr, ' contains ', nrows, ' rows'))
 
        # check if the current table is unique by drug code
        # do something like put the repeating rows in vectors
        # make a datafile from the raw data, each row contains a distinct drug_code
        # for the same drug_code, the other variables might have multpile values
        # These values are saved as a list in a row
        distinctdfstr <- paste0(thetablename, '_distinct.df')
        assign(distinctdfstr, makeDataFile(get(datadfnamestr), indexcolname))
        nrows_distinctdf<- nrow(get(distinctdfstr))
        print(paste0('the data frame ',distinctdfstr, ' contains ', nrows_distinctdf, ' rows'))

        # next is to connect the dataframes by DRUG_CODE

        #see history.C_DINATCLIST.R
        #c2 <- merge(drugs, c1, by= c("DRUG_CODE"))
    }

#check out the way to convert to JSON and save as local files in learning.r


