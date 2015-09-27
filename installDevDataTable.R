#install devtools
if(!require("devtools", character.only = TRUE) ){
        install.packages("devtools")    
        require(devtools)
}
# First remove the current version
remove.packages("data.table")  
# install dev version of data.table
install_github("Rdatatable/data.table", build_vignettes = FALSE)  
