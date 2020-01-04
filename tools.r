# to define table name and vars
definetable_list_my <- function (name_str, vars_str, dlmt) {
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

maketableinfo_df_my <- function (){
    # input the raw table info (i.e., input the table name, var name, and var type )
    rawtablestr_vector <- inputrawtableinfo_vector_my()

    #create an empty vector
    tables_vector <- vector()
    #loop for each element in the vector 'rawtablestr_vector', convert the element (a string containing raw table info) into a vector
    for (x in rawtablestr_vector) {
        therawtableEles_vector <- str2vector_my(x)
        # the eles in therawtableEles_vector include the table name, followed by (varname1, vartype1), (varname2, vartype2)... repeatedly
        #get the table name
        thetablename <- therawtableEles_vector[1]
        #put the rest elements into a separate vector by removing the first element
        varnametypes_vector <- therawtableEles_vector[-1]
        #covert the varnametypes_vector into a matrix of 2 columns (varname, vartype)
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

    # convert the tables_vector to a list with one field, 'tables'
    tables_list <- list (
        tables = tables_vector
    )

    # covert the tables_list to a JSON
    tables_JSON <- toJSON(tables_list)

    # write/save to a local file (as a text file)
    # fileConn<-file("tables.json")
    # writeLines(tables_JSON, fileConn)
    # close(fileConn)

    # convert theJSON back to a list
    tables_list <- fromJSON(tables_JSON)

    # convert the tables_vector back to a vector
    tables_vector <- (tables_list$tables)
    tables_vector

    # loop for each table in tables_vector
    i <-0
    # create a blank dataframe
    tables_df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(tables_df) <- c('table', 'varname', 'vartype')
    tables_df 
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


# get data of a specified data table in a zip file
getDataOfATableInAZip_df <- function (thetablename, zipfileobj){

    zipfileobj <- tmpzip
    name_thetxt <- paste0(thetablename, ".txt")
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
    thedata.df <- data.frame(data.list)
    # reset row index
    rownames(thedata.df) <- NULL
    # head(thedata.df)

    return (thedata.df)
} # end function


# covert the raw table info into a vector
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
    DRUG_IDENTIFICATION_NUMBER VARCHAR2(29)
    BRAND_NAME VARCHAR2(200)
    HISTORY_DATE DATE
    "
    )

    return (x)
}
