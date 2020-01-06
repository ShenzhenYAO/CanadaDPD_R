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

    # define the file file
    final.list <- list()

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

    filecount <- 0
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
        # make a datafile from the raw data, each row contains a distinct drug_code
        # for the same drug_code, the other variables might have multpile values
        # These values are saved as a list in a row
        distinctdfstr <- paste0(thetablename, '_distinct.df')
        assign(distinctdfstr, makeDataFile(get(datadfnamestr), indexcolname))
        nrows_distinctdf<- nrow(get(distinctdfstr))
        print(paste0('the data frame ',distinctdfstr, ' contains ', nrows_distinctdf, ' rows'))
        
        # reduce the DRUG_CODE column from a list to a vector of a single value 
        # the column DRUG_CODE will be used as the key column to join data frames.
        # such key columns cannot have nested lists.
        # The following shows how to reduce the column to a single-value vector when using dynamic df names
        # need to save the df (named by a parameter 'datadfnamestr') to a tmp df with a fixed name ('thetmpdf')
        thetmpdf <- get(distinctdfstr)
        # unlist the column (indexcolname, e.g. 'DRUG_CODE')
        # In the following line, cannot use get(distinctdfstr) on the left to represent the df 'vet_distinct.df' directly)
        # e.g., it does not work: get(distinctdfstr) <- unlist(thetmpdf[[indexcolname]])
        thetmpdf[indexcolname] <- unlist(thetmpdf[[indexcolname]])
        # save the tmp df back to the df named by the parameter
        assign(distinctdfstr, thetmpdf)

        # convert the distinct df file into a JSON, and save to local as a txt file
        targetfile <- paste0(distinctdfstr, '.json')
        saveAsTxt_my(distinctdfstr, paste0('data/', targetfile))

        # next is to connect the dataframes by DRUG_CODE
        if (filecount == 1){
            mergedtmp.df <- get(distinctdfstr)
        } else {
            # merge the current df with target.df by the indexcol 'DRUG_CODE'
            mergedtmp.df <- merge(mergedtmp.df, get(distinctdfstr), 
            by.x= c(indexcolname), by.y=c(indexcolname), all=TRUE)
        }

        saveAsTxt_my('mergedtmp.df', 'data/mergedtmp.json')
    }

# after all files are merged, make a list with the zipfile name, and the mergedtmp.df
tmp.list <- list(zipname = name_thezip, data=mergedtmp.df)
final.list <- list()
final.list <- rbind(final.list, tmp.list)
View(final.list)
saveAsTxt_my('final.list', 'data/final.json')
alarm()

# read the final.json
finalfromjsonfile.df <- fromJSON("final.json" )
head(finalfromjsonfile.df)


# the df from json is different from the 
for (x in finalfromjsonfile.df ){
    # get the name of the zipfile
    name_srczip <- x[[1]]
    # print(name_srczip)
    # remove the extension 'zip'
    name_currentdf <- sub('\\.zip$', '', name_currentzip)
    # print(name_currentdf)
    themeregeddfname = paste0(name_currentdf, '.df')
    # print(themeregeddfname)
    assign(themeregeddfname,  x[-c(1) ])  
}
View(get(themeregeddfname))

head(vet_distinct.df)



