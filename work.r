# read vet and pharm into df


alarm()
indexcolname

View(target.df)
str(ingred_distinct.df)

uniquetablenames.vector

# loop for each table (uniquetablenames.vector contains all file names
# the table 'inactive' is taken out for zip files not named 'allfiles_ia.zip')
filecount <- 0
for (thefilename in uniquetablenames.vector) {

    #count the files
    filecount <-  filecount  + 1
    curdfname = paste0(thefilename, '_distinct.df')

    thetmpdf <- get(curdfname)
    # unlist the column (indexcolname, e.g. 'DRUG_CODE')
    # In the following line, cannot use get(distinctdfstr) on the left to represent the df 'vet_distinct.df' directly)
    # e.g., it does not work: get(distinctdfstr) <- unlist(thetmpdf[[indexcolname]])
    thetmpdf[indexcolname] <- unlist(thetmpdf[[indexcolname]])
    # save the tmp df back to the df named by the parameter
    assign(curdfname, thetmpdf)

    if (filecount == 1){
        target.df <- get(curdfname)
    } else {
        # merge the current df with target.df by the indexcol 'DRUG_CODE'
        target.df <- merge(target.df, get(curdfname), 
        by.x= c(indexcolname), by.y=c(indexcolname), all=TRUE)
    }
}
View(target.df)
# add a column to indicate the current zip

final1.list <- list(zipname = 'allfiles.zip', data=target.df)

final2.list <- list(zipname = 'allfiles2.zip', data=target.df)

final.df <- rbind(final1.df, final2.df)


savedf_txt('final.df', 'final.json')