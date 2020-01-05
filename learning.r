
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

    # read from JSON obj (package=jsonlite)
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