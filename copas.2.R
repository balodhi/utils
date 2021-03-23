 read_data <- function(file, tofmin = 60, tofmax = 2000, extmin = 0,
                      extmax = 10000, SVM = TRUE, levels = 2, ...){
    
    # Read the raw sorter files and make the row names
    
    plate <- readFile(file, tofmin, tofmax, extmin, extmax, ...)
    if (all(plate$row == 0)) {
            modplate <- with(plate,
                             data.frame(row=stringr::str_split_fixed(Source.well, "[0-9]", 2)[,1],
                                        col=as.factor(stringr::str_split_fixed(Source.well, "[A-Z]", 2)[,2]),
                                        sort=Sorted.status,
                                        TOF=TOF,
                                        EXT=Extinction,
                                        time=as.numeric(Time),
                                        green=Green,
                                        yellow=Yellow,
                                        red=Red))