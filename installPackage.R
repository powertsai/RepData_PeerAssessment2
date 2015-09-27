#install devtools
if(!require("devtools", character.only = TRUE) ){
        install.packages("devtools")    
        require(devtools)
}
# First remove the current version
remove.packages("data.table")  
# install dev version of data.table
install_github("Rdatatable/data.table", build_vignettes = FALSE)  

#using dplyr to cleaning data
if(!require(dplyr)) {
        install.packages("dplyr")
        require(dplyr)
}

# using ggplot2 to create graph
if(!require(ggplot2)) {
        install.packages("ggplot2")
        require(ggplot2)
}

#using grid
if(!require(grid)) {
        install.packages("grid")
        require(grid)
}

