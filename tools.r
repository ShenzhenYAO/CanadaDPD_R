# to define table name and vars
definetable_list_my <- function (name_str, vars_str, dlmt) {

    print('start definetable_list_my ===============================')

   #by default, let dlmt=' '
    if (missing(dlmt)) {
        dlmt=' '
    }
    # turn the vars string into a collection
    vars_collection <- strsplit(vars_str, dlmt) 
    result <- list(
        table=name_str,
        vars=vars_collection
    )
    return (result)
}


# a function to convert a string into a vector, ignoring empty elements if not specified to keep empty elements
str2vector_my <- function (thestr, dlmt, noempty) {

    print('start str2vector_my ===============================')

    # by default, dlmt is blank space, or line breaker
    if (missing(dlmt)) { dlmt <- "[ \n]+" }
    # by default, ignore empty elements
    if (missing(noempty)) { noempty <- 1 }
    # print (noempty)
    # split the str into a vector. Note: the strsplit() results in a list, need to convert it back to a vector
    thevector_list <- strsplit(thestr, split="[ \n]+");
    thevector <- unlist(thevector_list)

    # remove the empty elements
    if (noempty == 1 || tolower(noempty) == 'y' || tolower(noempty) == 'yes' || tolower(noempty) == TRUE ){
        thevector_list <-lapply(thevector, function(x){x[!x == ""]})
        #Note: lapply() results in a list, need to convert it back to a vector
        thevector = unlist(thevector_list)
        # print(thevector)
    }
    return (thevector)
}

# a tool to make a df with information of tables (table name, varname, and vartype)
maketableinfo_dpd_my <- function (){

    print('start maketableinfo_dpd_my ===============================')

    # input the raw table info (i.e., input the table name, var name, and var type )
    rawtablestr_vector <- inputrawtableinfo_vector_my()

    #create an empty vector
    tables_vector <- vector()
    #loop for each element in the vector 'rawtablestr_vector', 
    # convert the element (a string containing raw table info) into a vector
    for (x in rawtablestr_vector) {
        # convert the string (table name and vars) into a vector
        therawtableEles_vector <- str2vector_my(x)
        # the eles in therawtableEles_vector include the table name, followed by (varname1, vartype1), (varname2, vartype2)... repeatedly
        #get the table name
        thetablename <- therawtableEles_vector[1]
        #put the rest elements into a separate vector by removing the first element
        varnametypes_vector <- therawtableEles_vector[-1]
        #convert the varnametypes_vector into a matrix of 2 columns (varname, vartype)
        varnametypes_matrix <- matrix(varnametypes_vector, ncol=2, byrow=TRUE)
        # add col names
        colnames(varnametypes_matrix) <- c("varname", 'vartype')
        # split the matrix by column into 2 vectors
        varname_vector= varnametypes_matrix[,1]
        vartype_vector= varnametypes_matrix[,2]

        # convert the table info into a list with three fields (i.e., table, varname, vartype)
        thetable_list <- list(
            table=thetablename,
            varname = varname_vector,
            vartype = vartype_vector
        )

        # push the current table into the big vector which contains all tables
        tables_vector = c(tables_vector, thetable_list)

        # Preferred to convert a list to a JSON, not a dataframe (in a dataframe, the values in the col 'table' repeat)
        # covert the list to a JSON
        # thetable_JSON <- toJSON(thetable_list)

        # convert theJSON back to a list
        # thetable_list <- fromJSON(thetable_JSON)
    }# end of loop


    #################################################################
    # the following 3 steps are for practice
    
    # # convert the tables_vector to a list with one field, 'tables'
    # tables_list <- list (
    #     tables = tables_vector
    # )

    # # covert the tables_list to a JSON
    # tables_JSON <- toJSON(tables_list)

    # # write/save to a local file (as a text file)
    # # fileConn<-file("tables.json")
    # # writeLines(tables_JSON, fileConn)
    # # close(fileConn)

    # # convert theJSON back to a list
    # tables_list <- fromJSON(tables_JSON)

    # # convert the tables_vector back to a vector
    # tables_vector <- (tables_list$tables)
    # # tables_vector
    ##################################################################

    # loop for each table in tables_vector
    i <-0
    # create a blank dataframe
    tables_df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(tables_df) <- c('table', 'varname', 'vartype')
    # tables_df 
    for (x in tables_vector) {
        i=i+1
        # get the table name
        if (i %% 3 == 1) {
            thetablename <- unlist(x)
        } else {
            # get the var name
            if (i %% 3 == 2) {
                    varname_vector <- unlist(x)
                } else {
                    # get the var type
                    vartype_vector <- unlist(x)

                    # make a list of three cols
                    thetable_list = list(table=thetablename, varname=varname_vector, vartype=vartype_vector)
                    # make a data frame from the list
                    thetable_df=data.frame(thetable_list)
                    # append thetable_df to the big dataframe: tables_df
                    tables_df <- rbind(tables_df, thetable_df)
                } # end else
        } # end if/else
    } # end for loop

    return (tables_df)

} # end function make df of tables


#unzip all tables, merge as a big zip, and save as a json
zip2dfjson_dpd_my <- function( name_thezip, dpd.list, indexcolname) {

    print('start zip2dfjson_dpd_my ===============================')

    #make a surfix like '_', or '_ap', etc.
    thezipsurfix <- sub('\\.zip$', '', name_thezip)
    thezipsurfix <- sub('allfiles', '', thezipsurfix)

    # download the zip file
    tmpzip <- downloadzip(name_thezip)

    # make a dataframe with tablename, varname, and vartype of all tables 
    tables.df <- maketableinfo_dpd_my()

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
        # uniquetablenames.vector
    }

    filecount <- 0
    # loop for each table name, import data into dfs named by the tablename (e.g.,thedata_drug.df )
    for (thetablename in uniquetablenames.vector) {
        # print(thetablename)
        filecount= filecount + 1
        #unzip the table, convert to a df, and merge it into a big df
        mergedtmp.df <- addfile2df_dpd_my(
                            tables.df,
                            thetablename,
                            thezipsurfix,
                            tmpzip,
                            indexcolname,
                            filecount,
                            mergedtmp.df
                        )
    } #end for

    # after all files are merged, make a list with the zipfile name, and the mergedtmp.df
    tmp.list <- list(zipname = name_thezip, data = mergedtmp.df)
    dpd.list <- rbind(dpd.list, tmp.list)

    # View(dpd.list)
    saveAsTxt_my(dpd.list, 'data/dpd.json')

    return (dpd.list)

} # end founction

# get data of a specified data table in a zip file
getDataOfATableInAZip_df <- function (tables.df, thetablename, thezipsurfix, zipfileobj){
    print('start getDataOfATableInAZip_df ===============================')
    # zipfileobj <- tmpzip
    thetablename_surfix <- paste0(thetablename, thezipsurfix)
    # there is an exception:
    # in allfiles_ia.zip, the file inactive.txt does not have a surfix _ia
    if (thetablename == 'inactive') {
        name_thetxt <- paste0(thetablename, ".txt")
    } else {
        name_thetxt <- paste0(thetablename_surfix, ".txt")
    }
    
    # unzip the text file
    data.list <- read.table(unz(zipfileobj, name_thetxt),  header=F, quote="\"", sep=",")

    #https://stackoverflow.com/questions/12460938/r-reading-in-a-zip-data-file-without-unzipping-it
    #data <- read.table(unz(name_thezip, name_thetxt), nrows=10, header=F, quote="\"", sep=",")
    #order by v1
    data.list <- data.list[order(data.list$V1),]

    #rename the cols of data
    # get the varnames of the current table
    # from the tables.df, select varnames where table name matches the current tablename
    thetable.df <- tables.df[which(tables.df$table == thetablename), ]
    # get a vector of varnames
    thevars.vector <- thetable.df[,'varname']
    # rename the data cols according to varnames in the varname vector
    colnames(data.list) <- thevars.vector

    # change data into a df
    # Note! turn off the string as factors setting does not work for datalist that has been created as stringasfactors=true
    thedata.df <- data.frame(data.list, stringsAsFactors =FALSE)
    # reset row index
    rownames(thedata.df) <- NULL
    # head(thedata.df)

    return (thedata.df)
} # end function

# download and unzip thezipfiles without saving to local
downloadzip <- function (zipname){
    
    print('start downloadzip ===============================')

    # zipname <- allfiles.zip 
    #name_thezip <- "F:/Personal/Dropbox/Project/Canada DPD/SASProject/Data/allfiles.zip"
    name_thezip <- paste0("https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/", zipname)
    #https://rpubs.com/otienodominic/398952
    #create a tmp file
    tmpzip <- tempfile()
    download.file(name_thezip, tmpzip)
    return (tmpzip)
}

# convert the source df into a JSON and save as a local txt file
saveAsTxt_my <- function (srcdataobj, targetfilenamestr) {

    print('start saveAsTxt_my ===============================')

    #Note: the jsonlite fromJSON automatically convert escape chars into utf-8 code. which may cause error...
    tmp.df <- srcdataobj
    #change all NULL to NA (NULL is inside a list, NA is blank). This is required before using the library jsonlite
    #https://stackoverflow.com/questions/40379021/function-to-change-blanks-to-na
    tmp.df[tmp.df == 'NULL'] <- NA

    # df to JSON (package=jsonlite)
    tmp.JSON <- toJSON(tmp.df, null='null')
    # tmp.JSON

    #write the json obj to a local file
    # Wrong. write_json is incorrect. Do not use it.
    #write_json(vet.JSON, 'test.json')
    # the correct way is directly write as a txt file!
    # write/save to a local file (as a text file)
    fileConn<- file(targetfilenamestr)
    writeLines(tmp.JSON, fileConn)
    close(fileConn)
}


#unzip the table, convert to a df, and merge it into a big df
addfile2df_dpd_my <- function(
        tables.df,
        thetablename,
        thezipsurfix,
        tmpzip,
        indexcolname,
        filecount,
        mergedtmp.df
    ) {

    print('start addfile2df_dpd_my ===============================')

    # by default,let file count =1
    if (missing(filecount)) {filecount <- 1 }
    
    # determine name of the df according to the file name 
    thetablename_surfix <- paste0(thetablename, thezipsurfix)
    # thetablename_surfix    
    
    #determine the name of the df
    datadfnamestr=paste0(thetablename_surfix, '.df')
    # get the data and name into the df named by the string 'datadfnamestr'
    # note: not to save the df to 'datadfnamestr', but to save the df to a df named by the str value of 'datadfnamestr'
    # e.g., datadfnamestr <- 'thedata_drug.df', the assign() saves the data to thedata_drug.df
    assign(datadfnamestr, getDataOfATableInAZip_df(tables.df, thetablename, thezipsurfix, tmpzip))
    # display the head rows of the df thedata_drug.df
    # head(get(datadfnamestr))

    # get the # rows of the df
    nrows<- nrow(get(datadfnamestr))
    print(paste0('the data frame ',datadfnamestr, ' contains ', nrows, ' rows'))

    # check if the current table is unique by drug code
    # make a datafile from the raw data, each row contains a distinct drug_code
    # for the same drug_code, the other variables might have multpile values
    # These values are saved as a list in a row
    thetmpdf <- makeDataFile(get(datadfnamestr), indexcolname)
    # reduce the DRUG_CODE column from a list to a vector of a single value 
    # the column DRUG_CODE will be used as the key column to join data frames.
    # such key columns cannot have nested lists.
    # The following shows how to reduce the column to a single-value vector when using dynamic df names
    # need to save the df (named by a parameter 'distinctdfstr') to a tmp df with a fixed name ('thetmpdf')

    # unlist the column (indexcolname, e.g. 'DRUG_CODE')
    # In the following line, cannot use get(distinctdfstr) on the left to represent the df 'vet_distinct.df' directly)
    # e.g., it does not work: get(distinctdfstr) <- unlist(thetmpdf[[indexcolname]])
    thetmpdf[indexcolname] <- unlist(thetmpdf[[indexcolname]])
    # save the tmp df  to the df named by the parameter
    distinctdfstr <- paste0(thetablename_surfix, '_distinct.df')
    assign(distinctdfstr, thetmpdf)        
    nrows_distinctdf <- nrow(thetmpdf)
    print(paste0('the data frame ',distinctdfstr, ' contains ', nrows_distinctdf, ' rows'))

    # convert the distinct df file into a JSON, and save to local as a txt file
    targetfile <- paste0(distinctdfstr, '.json')
    #targetfile

    saveAsTxt_my(get(distinctdfstr), paste0('data/', targetfile))

    # next is to merge the dataframes by DRUG_CODE
    if (filecount == 1){
        mergedtmp.df <- get(distinctdfstr)
    } else {
        # merge the current df with target.df by the indexcol 'DRUG_CODE'
        mergedtmp.df <- merge(mergedtmp.df, get(distinctdfstr), 
        by.x= c(indexcolname), by.y=c(indexcolname), all=TRUE)
    }

    zipjsonnamestr <- paste0('data/allfiles', thezipsurfix, '.json')
    # save the merged table as a json like 'allfiles_ap.json'    
    saveAsTxt_my(mergedtmp.df, zipjsonnamestr)

    return (mergedtmp.df)

} # end function 


# read the dpd.json into a list with 4 dfs
json2dfs_dpd_my <- function () {
    # get the dpd list from the local file dpd.json
    dpdfromjsonfile.list <- fromJSON("data/dpd.json" )
    # loop to have dfs from drugs of diferent status (in market, under application, dormant, or inactive)
    jj=0
    for (x in dpdfromjsonfile.list ){
        jj = jj+1 # jj to control whether to view a particular df, or all the four dfs
            if (jj > 0 ){
                # # get the name of the zipfile
                name_srczip <- x[[1]]
                print(name_srczip)
                # remove the extension 'zip'
                name_currentdf <- sub('\\.zip$', '', name_srczip)
                # print(name_currentdf)
                themeregeddfname = paste0(name_currentdf, '.df')
                print(themeregeddfname)
                assign(themeregeddfname,  x[-c(1) ])
                # # not to view the name of the df, you dumb dumb!
                # View(themeregeddfname)
                # view the df named by the str.
                View(get(themeregeddfname))  
            }
    }
    return (dpdfromjsonfile.list)
}


# covert the raw table vars info into a vector
# Note: rename the columns of DIN, brand name, history date, so that they won't be duplciated with the same columns in other files
inputrawtableinfo_vector_my <- function (){
    x <- c(
    "ingred
    DRUG_CODE NUMBER(8)
    ACTIVE_INGREDIENT_CODE NUMBER(6)
    INGREDIENT VARCHAR2(200)
    INGREDIENT_SUPPLIED_IND VARCHAR2(2)
    STRENGTH VARCHAR2(20)
    STRENGTH_UNIT VARCHAR2(20)
    STRENGTH_TYPE VARCHAR2(1)
    DOSAGE_VALUE VARCHAR2(20)
    BASE VARCHAR2(2)
    DOSAGE_UNIT VARCHAR2(10)
    NOTES VARCHAR2(110)
    INGREDIENT_F VARCHAR2(270)
    STRENGTH_UNIT_F VARCHAR2(80)
    STRENGTH_TYPE_F VARCHAR2(1)
    DOSAGE_UNIT_F VARCHAR2(20)
    ",
    "
    comp
    DRUG_CODE NUMBER(8)
    MFR_CODE VARCHAR2(10)
    COMPANY_CODE NUMBER(6)
    COMPANY_NAME VARCHAR2(100)
    COMPANY_TYPE VARCHAR2(40)
    ADDRESS_MAILING_FLAG VARCHAR2(2)
    ADDRESS_BILLING_FLAG VARCHAR2(2)
    ADDRESS_NOTIFICATION_FLAG VARCHAR2(2)
    ADDRESS_OTHER VARCHAR2(2)
    SUITE_NUMBER VARCHAR2(40)
    STREET_NAME VARCHAR2(80)
    CITY_NAME VARCHAR2(40)
    PROVINCE VARCHAR2(30)
    COUNTRY VARCHAR2(20)
    POSTAL_CODE VARCHAR2(20)
    POST_OFFICE_BOX VARCHAR2(20)
    PROVINCE_F VARCHAR2(30)
    COUNTRY_F VARCHAR2(30)
    ",
    "drug
    DRUG_CODE NUMBER(8)
    PRODUCT_CATEGORIZATION VARCHAR2(50)
    CLASS VARCHAR2(20)
    DRUG_IDENTIFICATION_NUMBER VARCHAR2(40)
    BRAND_NAME VARCHAR2(200)
    DESCRIPTOR VARCHAR2(200)
    PEDIATRIC_FLAG VARCHAR2(2)
    ACCESSION_NUMBER VARCHAR2(10)
    NUMBER_OF_AIS VARCHAR2(10)
    LAST_UPDATE_DATE DATE
    AI_GROUP_NO VARCHAR2(20)
    CLASS_F VARCHAR2(20)
    BRAND_NAME_F VARCHAR2(120)
    DESCRIPTOR_F VARCHAR2(140)
    ",
    "
    status
    DRUG_CODE NUMBER(8)
    CURRENT_STATUS_FLAG VARCHAR2(2)
    STATUS VARCHAR2(40)
    HISTORY_DATE DATE
    STATUS_F VARCHAR2(40)
    LOT_NUMBER VARCHAR2(50)
    EXPIRATION_DATE DATE
    ",
    "
    form
    DRUG_CODE NUMBER(8)
    PHARM_FORM_CODE NUMBER(7)
    PHARMACEUTICAL_FORM VARCHAR2(60)
    PHARMACEUTICAL_FORM_F VARCHAR2(60)
    ",
    "
    package
    DRUG_CODE NUMBER(8)
    UPC VARCHAR2(20)
    PACKAGE_SIZE_UNIT VARCHAR2(20)
    PACKAGE_TYPE VARCHAR2(25)
    PACKAGE_SIZE VARCHAR2(10)
    PRODUCT_INFORMATION VARCHAR2(100)
    PACKAGE_SIZE_UNIT_F VARCHAR2(1)
    PACKAGE_TYPE_F VARCHAR2(1)
    ",
    "pharm
    DRUG_CODE NUMBER(8)
    PHARMACEUTICAL_STD VARCHAR2(10)
    ",
    "route
    DRUG_CODE NUMBER(8)
    ROUTE_OF_ADMINISTRATION_CODE NUMBER(6)
    ROUTE_OF_ADMINISTRATION VARCHAR2(40)
    ROUTE_OF_ADMINISTRATION_F VARCHAR2(65)
    ",
    "schedule
    DRUG_CODE NUMBER(8)
    SCHEDULE VARCHAR2(30)
    SCHEDULE_F VARCHAR2(30)
    ",
    "ther
    DRUG_CODE NUMBER(8)
    TC_ATC_NUMBER VARCHAR2(10)
    TC_ATC VARCHAR2(80)
    TC_AHFS_NUMBER VARCHAR2(20)
    TC_AHFS VARCHAR2(80)
    TC_ATC_F VARCHAR2(1)
    TC_AHFS_F VARCHAR2(60)
    ",
    "vet
    DRUG_CODE NUMBER(8)
    VET_SPECIES VARCHAR2(50)
    VET_SUB_SPECIES VARCHAR2(40)
    VET_SPECIES_F VARCHAR2(50)
    ",
    "
    inactive
    DRUG_CODE NUMBER(8)
    DRUG_IDENTIFICATION_NUMBER_IA VARCHAR2(29)
    BRAND_NAME_IA VARCHAR2(200)
    HISTORY_DATE_IA DATE
    "
    )

    return (x)
}

# make a datafile from the raw data, each row contains a distinct drug_code
# for the same drug_code, the other variables might have multpile values
# These values are saved as a list in a row
makeDataFile <- function(thedf.df, indexcolname) {
    print('start makeDataFile ===============================')
    # thedf.df <- pharm.df
    # head(pharm.df)

    # determine the indexcol
    # indexcolname <- 'DRUG_CODE'

    # get the number of columns and names from the df
    colnames.list <- colnames(thedf.df)
    # colnames.list

    # sort by id
    thedf.df <- thedf.df[order(thedf.df[indexcolname]),]
    # reset row numbers
    rownames(thedf.df) <- NULL

    #prepare the target df (an empty df) with the same colnames as in thedf.df
    # add all distinct index col values
    target.df <- data.frame(matrix(data=list(), ncol = ncol(thedf.df), nrow = 0))
    colnames(target.df) <- colnames.list

    # get all indexcol values
    indexcolvalues.list <- thedf.df[indexcolname]
    distinctindexcolvalues.list <- unlist(unique(indexcolvalues.list))
    i <- 0
    for (v in distinctindexcolvalues.list) {
        i <- i + 1
        target.df[i, indexcolname] = v
    }

    # for each row in the df, if the id is unchanged, push the value in atc into a vector
    #create tmp vectors to hold values of a col in rows where the indexcolname value is the same
    # create a tmp vector for each col in thedf, except the indexcol

    # from the colname.list, exclude the indexcol
    targetcolnames.vector<- unlist(lapply(colnames.list, function(x){x[!x == indexcolname]}))
    # targetcolnames.vector = unlist(lapply(targetcolnames.vector, function(x){x[!x == 'atc']}))
    # get the last col 's value 
    lastcolname <- targetcolnames.vector[length(targetcolnames.vector)] 

    # for each col in targetcolnames.vector, create a tmp vector
    for (x in targetcolnames.vector){
        # determine the tmpvectorname
        tmpvectorname <- paste0('tmp_', x, '.vector') # like tmp_DIN.vector
        # assign an empty vector to the tmpvectorname
        assign(tmpvectorname, vector() )
    }

    #create a var to hold the value of the indexcol
    retainedindexcolvalue <-''
    i <- 0
    for (row in (1:nrow(thedf.df))) {
        i <- i + 1
        # get the vlaue of the current index col
        theindexcolvalue <- unlist(thedf.df[row,indexcolname])
        # thecurcoln=1
        # for a given var, get the distinct values of a col of thedf.df
        for (thecurcol in targetcolnames.vector) {

            #make a tmp vector to hold distinct values in the current col
            tmpvectorname <-  paste0('tmp_', thecurcol, '.vector') # like tmp_DIN.vector
            # make rows of distinct indexcol values
            # e.g., putting all drug_code=2 rows into 1 row. 
            # multiple values in other columns are collapsed to a vector 

            # get the value of the current col
            thecolvalue <- unlist(thedf.df[row,thecurcol])
            # if the indexcol's value remains unchanged
            if (retainedindexcolvalue == theindexcolvalue){
                # push the value of the current col into the tmp.vector
                if ( ! thecolvalue %in% get(tmpvectorname)){
                assign(tmpvectorname, c(get(tmpvectorname), thecolvalue) )   
                }
            } else {# if the indexcol's value has changed
                    if (i > 1 ){
                        target.df[[target.df[indexcolname] == retainedindexcolvalue, thecurcol]] <- get(tmpvectorname)
                    }
                    # update the retained value and the tmp.vector
                    # if the current col is the last col in targetcolnames.vector, 
                    # update the retainedindexcolvalue
                    if (thecurcol == lastcolname) {
                        retainedindexcolvalue <- theindexcolvalue 
                    }           
                    assign(tmpvectorname, c(thecolvalue))         
            }

            #finally, if it is the last row, append the id and the values of the current col again
            if (i == nrow(thedf.df)){
                target.df[[target.df[indexcolname] == retainedindexcolvalue, thecurcol]] <- get(tmpvectorname)
            }
        }
    } # end loop

    return (target.df)
}





