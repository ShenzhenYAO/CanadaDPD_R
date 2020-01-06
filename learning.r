
# how to save it (?csv, ?RDS, ?JSON)
notrun_aboutToFromJSON_RSD_CSV <- function () {

    # in this case, it won't work as the df contains list objects 
    # sink() does not work, as the nested table may not be complete 
    # write.csv(vet_distinct.df, file="test.csv")


    #instead write to a JSON file use 'jsonlite'
    # install.packages('jsonlite')
    # library(jsonlite)

    #Note: the jsonlite fromJSON automatically convert escape into utf-8. which may cause error...
    testsrc.df <- vet_distinct.df
    # rownames(testsrc.df) <- c()
    #change all NULL to NA (NULL is inside a list, NA is blank)
    #https://stackoverflow.com/questions/40379021/function-to-change-blanks-to-na
    testsrc.df[testsrc.df == 'NULL'] <- NA
    # testsrc.df
    View(testsrc.df)
    # all.equal(xxx, fromJSON(toJSON(xxx)))

    # df to JSON (package=jsonlite)
    testobj.JSON <- toJSON(testsrc.df, null='null')
    # testobj.JSON
    # Wrong. write_json is incorrect. Do not use it.
    #write_json(vet.JSON, 'test.json')

    # df from JSON obj (package=jsonlite)
    testfromjsonobj.df <- fromJSON(testobj.JSON)
    View(testfromjsonobj.df)
    all.equal(testfromjsonobj.df, testsrc.df)

    # the correct way is directly write as a txt file!
    # write/save to a local file (as a text file)
    fileConn<-file("test.json")
    writeLines(testobj.JSON, fileConn)
    close(fileConn)

    # read from JSON file (package=jsonlite)
    testfromjsonfile.df <- fromJSON("test.json" )
    # head(testfromjsonfile.df)
    View(testfromjsonfile.df)
    all.equal(testfromjsonfile.df, testsrc.df)

    # write to an R data
    # saveRDS(importDPD_tmp1, paste(path, "/", filehead, "s", ".rds", sep=""))
    saveRDS(testsrc.df, paste('test', ".rds", sep=""))
    # read RDS data
    # testfromrds.df <- readRDS("test.rds")
    # Note. the reloaded rds is NOT the same as the original. 
    # all(testsrc.df == testfromrds)

} # end not run


notrun2 <- function () {

#check out the way to convert to JSON and save as local files in learning.r
#http://www.datasciencemadesimple.com/join-in-r-merge-in-r/
tmp.df <- merge(ingred_distinct.df, comp_distinct.df, by= c(indexcolname), all=TRUE)

# merge data frame, if the 'by' column is a single value (a vector of one element)
a <- data.frame(id= c(1,2,3), v1=c(9,8,7))
a
b <- data.frame(id=c(1,4, 3), v2=c(99,55,77))
z1 <- merge(a, b, by= c('id'), all=TRUE)
z1
z2 <- merge(a, b, by.x= c('id'), by.y= c('id'), all=TRUE)
z2

# merge data frame, if the 'by' column is a list( multiple values in a cell)
c <- data.frame(matrix(data=list(), ncol = 2, nrow = 0))
colnames(c) <- c('id', 'v3')
c[[1,1]] <- 4
c[[1,2]] <- 88
c[[2,1]] <- 1
c[[2,2]] <- list(y)
str(c)
d <- data.frame(matrix(data=list(), ncol = 2, nrow = 0))
colnames(d) <- c('id', 'v3')
d[[1,1]] <- 4
d[[1,2]] <- 88
d[[2,1]] <- 2
d[[2,2]] <- list(y)
str(d)
# cannot merge the above two df, as the 'by' column id is a list
zz1 <- merge(c, d, by= c('id'), all=TRUE)
zz1
# the following also not work
zz2 <- merge(c, d, by.x=c(c$id), by.y=c(d$id), all=TRUE)
zz2

# to merge the above two df, the 'by' column 'id' need to be reduced to a single value (i.e., a vector with one value)
e <- c
# Note: in e['id'], each element in the column 'id' is treated as a vector of a single value
# in e$id, each element in the column 'id' is treated as a list of multiple values
# the following is to reduce e$id (lists) into e['id'] (single value vectors)
e['id'] <- unlist(e$id)
str(e)
f <- d
f['id'] <- unlist(f$id)
str(f)
# zzz <- merge(e, f, by= c('id'), all=TRUE)
zzz <- merge(e, f, by.x= c('id'), by.y= c('id'), all=TRUE)
zzz

} # not run2