
# for each index value, get the distinct values of a col of thedf.df,  and save in a vector
# note: global variables!
makeDistinctRow_index <- function (
        indexcolname,
        thedf.df, row, thecurcol,
        theindexcolvalue, lastcolname
    ) {   
    
    # get the value of the current col
    thecolvalue <- unlist(thedf.df[row,thecurcol])
    # if the indexcol's value remains unchanged
    if (retainedindexcolvalue == theindexcolvalue){
        # push the value of the current col into the tmp.vector
        if ( ! thecolvalue %in% get(tmpvectorname)){
         assign(tmpvectorname, c(get(tmpvectorname), thecolvalue), envir = .GlobalEnv )   
        }
    } else {# if the indexcol's value has changed
            if (i > 1 ){
                target.df[[target.df[indexcolname] == retainedindexcolvalue, thecurcol]] <<- get(tmpvectorname)
            }
            # update the retained value and the tmp.vector
            # if the current col is the last col in targetcolnames.list, 
            # update the retainedindexcolvalue
            if (thecurcol == lastcolname) {

                retainedindexcolvalue <<- theindexcolvalue 
            }           
            assign(tmpvectorname, c(thecolvalue), envir = .GlobalEnv)         
    }

    #finally, if it is the last row, append the id and the values of the current col again
    if (i == nrow(thedf.df)){
        target.df[[target.df[indexcolname] == retainedindexcolvalue, thecurcol]] <<- get(tmpvectorname)
    }
} # end function




# make a dummy df
# testing compressing cols
# thedf.df <- data.frame(matrix(ncol = 3, nrow = 0))
# colnames(thedf.df) <- c('id', 'atcs', 'comp')
# col1<-c(3,2,2,1,1,1,0)
# col2<-c('a', 'b', 'b', 'c,e', NA, 'd,f', 'g')
# col3<-c('z','y','x', 'w', 'w', NA, 'u')
# thedf.df <- data.frame(id=col1, atc=col2, comp=col3, stringsAsFactors=F)
# thedf.df


thedf.df <- drug.df
head(thedf.df)
# determine the indexcol
indexcolname <- 'DRUG_CODE'

# get the number of columns and names from the df
colnames.list <- colnames(thedf.df)
colnames.list

# sort by id
thedf.df <- thedf.df[order(thedf.df[indexcolname]),]
# reset row numbers
rownames(thedf.df) <- NULL

#prepare the target df with the same colnames as in thedf.df
# add all distinct index col values

target.df <- data.frame(matrix(data=list(), ncol = ncol(thedf.df), nrow = 0))
colnames(target.df) <- colnames.list

# get all indexcol values
indexcolvalues.list = thedf.df[indexcolname]
distinctindexcolvalues.list = unlist(unique(indexcolvalues.list))
i <- 0
for (v in distinctindexcolvalues.list) {
    i=i+1
    target.df[i, indexcolname] = v
}


# for each row in the df, if the id is unchanged, push the value in atc into a vector
#create tmp vectors to hold values of a col in rows where the indexcolname value is the same
# create a tmp vector for each col in thedf, except the indexcol

# from the colname.list, exclude the indexcol
targetcolnames.list <- unlist(lapply(colnames.list, function(x){x[!x == indexcolname]}))
# targetcolnames.list = unlist(lapply(targetcolnames.list, function(x){x[!x == 'atc']}))
# get the last col 's value 
lastcolname <- targetcolnames.list[length(targetcolnames.list)] 


# for each col in targetcolname.list, create a tmp vector
for (x in targetcolnames.list){
    # determine the tmpvectorname
    tmpvectorname <- paste0('tmp_', x, '.vector') # like tmp_DIN.vector
    # assign an empty vector to the tmpvectorname
    assign(tmpvectorname, vector(), envir = .GlobalEnv )
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
    for (thecurcol in targetcolnames.list) {
        tmpvectorname <-  paste0('tmp_', thecurcol, '.vector') # like tmp_DIN.vector
        # make rows of distinct indexcol values
        # e.g., putting all drug_code=2 rows into 1 row. 
        # multiple values in other columns are collapsed to a vector 
        makeDistinctRow_index (indexcolname,
            thedf.df, row, thecurcol,
            theindexcolvalue, lastcolname
            )
    }
} # end loop
head(target.df)
# View(target.df)











