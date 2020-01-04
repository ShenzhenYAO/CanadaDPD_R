source('tools.r')
library('rjson')
install.packages('gtools') # for define macros
library('gtools')

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
    datadfnamestr

    # check if the current table is unique by drug code
    # do something like put the repeating rows in vectors
    # ...
}


#

# testing compressing cols
test.df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(test.df) <- c('id', 'atcs', 'comp')
col1<-c(3,2,2,1,1,1)
col2<-c('a', 'b', 'b', 'c,e', NA, 'd,f')
col3<-c('z','y','x', 'w', 'w', NA)
test.df <- data.frame(id=col1, atc=col2, comp=col3, stringsAsFactors=F)
test.df

# sort by id
test.df <- test.df[order(test.df$id),]
# reset row numbers
rownames(test.df) <- NULL
target.df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(target.df) <- c('id', 'atcs')
# for each row in the df, if the id is unchanged, push the value in atc into a vector
atc.vector<-vector()
retainedID <-''
i <- 0
for (row in (1:nrow(test.df))) {
    i <- i+1
    theID <- unlist(test.df[row,'id'])
    theATC <- unlist(test.df[row,'atc'])
    # print('theID is =============')
    # print( theID)
    # print('theATC is =============')
    # print( theATC)
    # print('atc.vector before is =============')
    # print(atc.vector)
    # if the current id is the same as the retained id
    if (retainedID == theID){
        # if the ATC value is not in the ATC vector, push it into the ATC vector
        if ( ! theATC %in% atc.vector){
           atc.vector <- c(atc.vector, theATC)
        }
    } else {
        if (i > 1 ){
            # print('new push')
            # push the id and the vector into the final df
            tmprow.df <- matrix(data=list(),nrow=1,ncol=2)
            tmprow.df[1,1] <- retainedID
            tmprow.df[[1,2]] <- atc.vector
            colnames(tmprow.df) <- c("id", "atcs")

            # print('tmprow.df and target.df =========')
            # print(tmprow.df)
            # append it to the target.df
            target.df <- rbind(target.df, tmprow.df)

        }
    
        #update the retained ID and the atc.vector
        retainedID <- theID
        atc.vector<-c(theATC)
    }
    # print('atc.vector after is =============')
    # print(atc.vector)

    #finally, if it is the last row, append the id and atc again
    if (i == nrow(test.df)){
        # print('new push')
        # push the id and the vector into the final df
        tmprow.df <- matrix(data=list(),nrow=1,ncol=2)
        tmprow.df[1,1] <- retainedID
        tmprow.df[[1,2]] <- atc.vector
        colnames(tmprow.df) <- c("id", "atcs")
        # print('tmprow.df and target.df =========')
        # print(tmprow.df)
        # append it to the target.df
        target.df <- rbind(target.df, tmprow.df)
        # print(target.df)
    }
}
atc.vector
target.df
View(target.df)
# colnames(target.df) <- c("id", "atcs")
