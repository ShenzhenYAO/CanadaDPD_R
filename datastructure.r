# create a collection of values
form_varnames <- c("DRUG_CODE", "FORM_CODE", "PHARMACEUTICAL_FORM", "PHARMACEUTICAL_FORM_F")
form_varnames
form_tablename <- "form"
form_tablename
frame_form <- list(
    table = form_tablename,
    vars = form_varnames
)
frame_form

route_varnames <- c("DRUG_CODE", "ROUTE_OF_ADMINISTRATION_CODE", "ROUTE_OF_ADMINISTRATION", 
"ROUTE_OF_ADMINISTRATION_F")
route_varnames
route_tablename <- "route"
route_tablename
frame_route <- list(
    table = route_tablename,
    vars = route_varnames
)
frame_route

a<-"================================================"
a
datastructure <- c(frame_form, frame_route)
datastructure


#install rjson
#install.packages('rjson')
#library('rjson')
x <- toJSON (frame_route)
x
y <- toJSON(datastructure)
y







