library(dplyr)
library(knitr)
library(ggplot2)
library(reshape2)

readFile <- function(file,sep=',') {
#data <- read.delim(file=file, header=T, na.strings=c("n/a"), as.is=T, stringsAsFactors=F)
data <- read.delim(file=file, header=T, na.strings=c("n/a"), sep=sep)
return (data)
}

plateread <- function(file, reflx=FALSE, extmin=0, extmax=10000, tofmin=0, tofmax=10000, sep=',',droplast=0) {
  plate <- readFile(file)
  plate <- plate[1:(length(plate)-droplast)]
  if(!all(is.na(plate$Source.well)) & reflx) {
    stop("Data is from reflx please turn off the the reflx option")
  }
  plate <- plate[!is.na(plate$TOF),] #remove rows in TOF if NA
  plate <- plate[,!is.na(plate[1,])] # remove NA columns
  if (!reflx) {
    plate <- plate[(plate$Extinction>=extmin & plate$Extinction<=extmax) | plate$Extinction == -1,] #get data in range
    
    plate$Column <- as.factor(plate$Column) #convert columns's column to catagories
    plate$Row <- as.factor(plate$Row) #convert Row's column to catagories
  }

  #modplate <- with(plate, data.frame(row=Row, col=as.factor(Column), sort=Sorted.status, TOF=TOF, EXT=Extinction, time=Time, green=Green, yellow=Yellow, red=Red))
  #modplate <- modplate %>% group_by(modplate$row, modplate$col) %>% do(extractTime(.)) %>% data.frame()
  #modplate[,10:13] <- apply(modplate[,c(5, 7:9)], 2, function(x){x/modplate$TOF})
  #colnames(modplate)[10:13] <- c("norm.EXT", "norm.green", "norm.yellow", "norm.red")
  return (plate)
}

extractTime <- function(plate){
  plate$time <- plate$time - min(plate$time)
  return(plate)
}

fillWells <- function(plate, rename=TRUE){
  complete = data.frame(row=rep(LETTERS[1:8], each=12), col=rep(1:12, 8))
  if (rename == TRUE) {
    colnames(plate)[4] <- "row"
    colnames(plate)[5] <- "col"
  }
  
 plate = merge(plate, complete, by=c("row", "col"), all=TRUE)
  return(plate)
}

removeWells <- function(plate, badWells, drop=FALSE) {
  sp.bw <- str_split(badWells, "", 3)
  if(!drop){
    for (i in seq(1, length(sp.bw))) {
      if(length(sp.bw) > 0){
        row <- as.character(sp.bw[[i]][2])
        col <- as.character(sp.bw[[i]][3])
        plate[which(plate$row == row & plate$col == col),which(colnames(plate)=="n"):ncol(plate)] <- NA
      }
    }
  } else {
    for (i in seq(1, length(sp.bw))) {
      if(length(sp.bw) > 0){
        row <- sp.bw[[i]][2]
        col <- sp.bw[[i]][3]
        plate = plate[plate$row != row | plate$col != col,]
      }
    }
  }
  return(plate)
}

removeNAWells <- function(plate) {
  plate <-  na.omit(plate)
  
  return (plate)
}

summarizePlate <- function(plate, ends=FALSE, quantiles=FALSE) {
  plate <- fillWells(plate)
  plate[2:14] <- lapply(plate[2:14], as.numeric)
  processed <- suppressWarnings(plate %>% group_by(row, col) %>% 
                                  summarise(n=ifelse(length(TOF[!is.na(TOF)])==0, NA, length(TOF[!is.na(TOF)])), 
                                            n.sorted=sum(Sorted.status==6 | Sorted.status==12), 
                                            sTOF = sum(TOF),
                                            
                                            mean.TOF = mean(TOF, na.rm=TRUE), 
                                            min.TOF=as.numeric(quantile(TOF, na.rm=TRUE)[1]), 
                                            q10.TOF=as.numeric(quantile(TOF, probs=0.1, na.rm=TRUE)[1]), 
                                            q25.TOF=as.numeric(quantile(TOF, probs=0.25, na.rm=TRUE)[1]), 
                                            median.TOF=median(TOF, na.rm=TRUE), 
                                            q75.TOF=as.numeric(quantile(TOF, probs=0.75, na.rm=TRUE)[1]), 
                                            q90.TOF=as.numeric(quantile(TOF, probs=0.90, na.rm=TRUE)[1]), 
                                            max.TOF=as.numeric(quantile(TOF, na.rm=TRUE)[5]),
                                
                                            mean.Extinction = mean(Extinction, na.rm=TRUE), 
                                            min.Extinction=as.numeric(quantile(Extinction, na.rm=TRUE)[1]), 
                                            q10.Extinction=as.numeric(quantile(Extinction, probs=0.1, na.rm=TRUE)[1]), 
                                            q25.Extinction=as.numeric(quantile(Extinction, probs=0.25, na.rm=TRUE)[1]), 
                                            median.Extinction=median(Extinction, na.rm=TRUE), 
                                            q75.Extinction=as.numeric(quantile(Extinction, probs=0.75, na.rm=TRUE)[1]), 
                                            q90.Extinction=as.numeric(quantile(Extinction, probs=0.90, na.rm=TRUE)[1]), 
                                            max.Extinction=as.numeric(quantile(Extinction, na.rm=TRUE)[5]),
                                            
                                            mean.red=mean(Red, na.rm=TRUE),
                                            min.red=as.numeric(quantile(Red, na.rm=TRUE)[1]),
                                            q10.red=as.numeric(quantile(Red, probs=0.1, na.rm=TRUE)[1]),
                                            q25.red=as.numeric(quantile(Red, probs=0.25, na.rm=TRUE)[1]),
                                            median.red=median(Red, na.rm=TRUE),
                                            q75.red=as.numeric(quantile(Red, probs=0.75, na.rm=TRUE)[1]),
                                            q90.red=as.numeric(quantile(Red, probs=0.9, na.rm=TRUE)[1]),
                                            max.red=as.numeric(quantile(Red, na.rm=TRUE)[5]),
                                            
                                            mean.green=mean(Green, na.rm=TRUE),
                                            min.green=as.numeric(quantile(Green, na.rm=TRUE)[1]),
                                            q10.green=as.numeric(quantile(Green, probs=0.1, na.rm=TRUE)[1]),
                                            q25.green=as.numeric(quantile(Green, probs=0.25, na.rm=TRUE)[1]),
                                            median.green=median(Green, na.rm=TRUE),
                                            q75.green=as.numeric(quantile(Green, probs=0.75, na.rm=TRUE)[1]),
                                            q90.green=as.numeric(quantile(Green, probs=0.9, na.rm=TRUE)[1]),
                                            max.green=as.numeric(quantile(Green, na.rm=TRUE)[5]),
                                            
                                            mean.yellow=mean(Yellow, na.rm=TRUE),
                                            min.yellow=as.numeric(quantile(Yellow, na.rm=TRUE)[1]),
                                            q10.yellow=as.numeric(quantile(Yellow, probs=0.1, na.rm=TRUE)[1]),
                                            q25.yellow=as.numeric(quantile(Yellow, probs=0.25, na.rm=TRUE)[1]),
                                            median.yellow=median(Yellow, na.rm=TRUE),
                                            q75.yellow=as.numeric(quantile(Yellow, probs=0.75, na.rm=TRUE)[1]),
                                            q90.yellow=as.numeric(quantile(Yellow, probs=0.9, na.rm=TRUE)[1]),
                                            max.yellow=as.numeric(quantile(Yellow, na.rm=TRUE)[5])
                                            ))
  if(!ends){
    processed <- processed[,-(grep("min", colnames(processed)))]
    processed <- processed[,-(grep("max", colnames(processed)))]
  }
  if(!quantiles){
    processed <- processed[,-(grep("q", colnames(processed)))]
  }
  analysis <- processed
  analysis[analysis$mean.TOF==-1 | is.na(analysis$mean.TOF),which(colnames(analysis)=="n"):ncol(analysis)] <- NA
  if (colnames(processed)[5] == "sTOF") {
    colnames(processed)[5] <- "TOF"
  }
  
  return(processed)
}

plotTrait = function(plate, trait, trait2 = NULL, type="heat", keyword=""){
  plate = data.frame(plate)
  print(trait)
  if (colnames(plate)[4] == "Row") {
    colnames(plate)[4] <- "row"
    colnames(plate)[5] <- "col"
  }
  if(type == "heat"){
    plate$label <- ifelse(is.na(plate[,which(colnames(plate)==trait)]), "", plate[,which(colnames(plate)==trait)])
    plot = ggplot(plate) + geom_rect(aes_string(xmin=0,xmax=5,ymin=0,ymax=5,fill=trait)) +
      geom_text(data=plate,aes(x=2.5,y=2.5, label = label, colour="white"), colour="white")+
      theme(axis.ticks.x=element_line(size = 0), axis.ticks.y=element_line(size = 0), axis.text.x=element_text(size = 0), axis.text.y=element_text(size = 0)) +
      xlab("Columns") + ylab("Rows")
  } else if(type == "hist"){
    if(sum(is.na(plate[,which(colnames(plate)==trait)])) > 0){
      badWells = plate[is.na(plate[,which(colnames(plate)==trait)]),c("row", "col")]
      badWells = paste0(badWells$row, badWells$col)
      plate = removeWells(plate, badWells, drop=TRUE)
    }
   
    plot = ggplot(plate) + geom_histogram(aes_string(x = trait), binwidth = diff(range(plate[[trait]]))/15) + xlab(trait) + ylab("Count") +
      theme(axis.text.x=element_text(size="10", angle=45, hjust=1), axis.text.y=element_text(size="10"))
  }
  else if(type == "scatter"){

    if (keyword == 'stage') {
      #str1 <- paste('"log10(',trait,')"', sep="")
      plot = ggplot(plate) + geom_point(aes_string(x = log10(plate[[trait]]), y = log10(plate[[trait2]]), colour = "stage"), size=1.5) + xlab(trait) + ylab(trait2) +
        theme(axis.text.x=element_text(size="10", angle=45, hjust=1), axis.text.y=element_text(size="10"))
      
    } else {
      
    plot = ggplot(plate) + geom_point(aes_string(x = trait, y = trait2), size=1.5) + xlab(trait) + ylab(trait2) +
      theme(axis.text.x=element_text(size="10", angle=45, hjust=1), axis.text.y=element_text(size="10")) }
  }
  else if (type == 'line') {
    plot = ggplot(plate) + geom_line(aes_string(x = trait, y=trait2)) + xlab(trait) + ylab(trait2) +
      theme(axis.text.x=element_text(size="10", angle=45, hjust=1), axis.text.y=element_text(size="10"))
  }
  else {
    stop("Unrecognized plot type")
  }
  plot = plot + facet_grid(row~col, drop=FALSE)
  return(plot)
}

plotCorMatrix = function(plate_summary1, plate_summary2=plate_summary1){
  plate_summary1 = fillWells(plate_summary1, rename=FALSE)
  plate_summary2 = fillWells(plate_summary2, rename=FALSE)
  if(nrow(plate_summary1) != 96 | nrow(plate_summary2) != 96){
    stop("Both plates must be summarized")
  }
  if(ncol(plate_summary1) != ncol(plate_summary2)){
    stop("Both plates have to have the same number of traits")
  }
  corDF = melt(cor(plate_summary1[,-(1:2)], plate_summary2[,-(1:2)], use = "complete.obs"))
  colnames(corDF) = c("Plate1", "Plate2", "Correlation")
  ggplot(corDF, aes(Plate1, Plate2, fill = Correlation)) + 
    geom_tile() + scale_fill_gradient2("Correlation",low = "blue", high = "red", mid = "green", limits = c(-1,1)) +
    theme(axis.text.x=element_text(angle = 90, hjust = 1)) + xlab("Plate 1") + ylab("Plate 2")
}

#' Detect edge effects on 96-well plates
#' 
#' Test for an effect of the position of wells in a 96 well plate. This function will split a plate population by edge wells and center wells and test the two populations for significant differences in either a specific trait or all traits if a trait is not specified.
edgeEffect = function(plate_summary, trait=NULL){
  plate_summary = data.frame(plate_summary)
  if(nrow(plate_summary) != 96){
    stop("plate_summary must be summarized and filled first")
  }
  edgeWells = plate_summary[plate_summary$row == "A" | plate_summary$row == "H" | plate_summary$col == 1 | plate_summary$col == 12,-(1:2)]
  edgeWells$pos = "edge"
  centerWells = plate_summary[!(plate_summary$row == "A" | plate_summary$row == "H" | plate_summary$col == 1 | plate_summary$col == 12),-(1:2)]
  centerWells$pos = "center"
  total = rbind(edgeWells, centerWells)
  pos = total$pos
  total = total[,-(ncol(total))]
  if(missing(trait)){
    pval = as.data.frame(apply(total, 2, function(x){
      wilcox.test(x[which(pos == "edge")], x[which(pos == "center")])$p.value
    }))
    pval$Trait = rownames(pval)
    rownames(pval) = NULL
    colnames(pval) = c("PValue", "Trait")
    pval = pval[,c(2,1)]
  } else {
    pval = wilcox.test(total[,trait][which(pos == "edge")], total[,trait][which(pos == "center")])$p.value
  }
  return(pval)
}

#' Visualize and compare values and distributions across multiple plates
#' 
#' Plot the value (bar plot, if summarized) or distribution (boxplot, if unsummarized) of the data from each well across multiple plates.

  plotComp = function(plates, trait, plateNames=NULL){
    if(!is.null(plateNames) & length(plateNames) != length(plates)){
      stop("Length of plateNames must match length of plates")
    }
    for(i in seq(1, length(plates))){
      if(is.null(plateNames)){
        plateName_col =  rep(i, nrow(plates[[i]]))
        plates[[i]] = as.data.frame(cbind(plates[[i]],plateName_col))
      } else {
        #plateName_col = rep(plateNames[i], nrow(plates[[i]]))
        #plates[[i]] = as.data.frame(cbind(as_tibble(plates[[i]]),plateName_col))
        plateName_col = rep(plateNames[i],nrow(plates[[i]]))
        plates[[i]] <- as.data.frame(cbind(plates[[i]],plateName_col))
        
      }
      #colnames(plates[[i]])[ncol(plates[[i]])] = "Plate"
      
      if (nrow(plates[[i]]) == 96) {
        print("plate")
        sapply(plates[[i]], class)
        c_factor_2_char <- sapply(plates[[i]], is.factor)
        plates[[i]][c_factor_2_char]<- lapply(plates[[i]][c_factor_2_char], as.character)
      }
      if(FALSE){
        if (TRUE) {
          print("hello")
        }
      else {
        print("data")
        sapply(plates[[i]], class)
        c_factor_2_char <- sapply(plates[[i]], is.factor)
        plates[[i]][c_factor_2_char]<- lapply(plates[[i]][c_factor_2_char], as.factor)
      }
      
    } }
    
    wholeDF = suppressWarnings(rbind_all(plates))
    wholeDF[,c("row", "col")] <- list(as.factor(wholeDF$row), as.factor(wholeDF$col))
    levels(wholeDF$row) <- LETTERS[1:8]
    levels(wholeDF$col) <- 1:12
    
    if(nrow(wholeDF) %% 96 == 0 & trait %in% colnames(wholeDF)){
      ggplot(wholeDF, aes_string(x = "Plate", y = trait, fill = "Plate")) + geom_bar(stat="identity") + facet_grid(row~col, drop=FALSE) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else if(trait %in% colnames(wholeDF)){
      ggplot(wholeDF, aes_string(x = "Plate", y = trait, fill = "Plate")) + geom_boxplot() + facet_grid(row~col, drop=FALSE) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } 
  }
  
  #' Plot a dose response curve by strain across a plate
  plotDR = function(plate_summary, dosages, trait="n"){
    plate_summary = data.frame(cbind(dose = dosages, plate_summary))
    plate_summary = plate_summary[,-c(3,4)]
    plate_summary = plate_summary[!is.na(plate_summary$strain)&!is.na(plate_summary$dose),]
    plate_summary = melt(plate_summary, id=c("strain", "dose"))
    plate_summary = plate_summary %>% group_by(strain, dose, variable) %>% summarize(mean = mean(value))
    plate_summary = reshape2::dcast(plate_summary, strain+dose~variable, value.var="mean")
    ggplot(plate_summary, aes_string(x="dose", y=trait, colour="strain")) + geom_point() + geom_line() + scale_colour_discrete(name="Strain") + xlab("Dose") + ylab(trait)
  }
  
  #' Plot dose response curves for all traits
  
  plotDR_allTraits = function(plate_summary, dosages){
    plots = list()
    for(trait in colnames(plate_summary)[4:ncol(plate_summary)]){
      plots = append(plots, list(plotDR(plate_summary, dosages, trait)))
    }
    names(plots) = colnames(plate_summary)[4:ncol(plate_summary)]
    return(plots)
  }
  plottest = function(plates, trait, plateNames=NULL){
    if(!is.null(plateNames) & length(plateNames) != length(plates)){
      stop("Length of plateNames must match length of plates")
    }
    for(i in seq(1, length(plates))){
      if(is.null(plateNames)){
        plateName_col =  rep(i, nrow(plates[[i]]))
        plates[[i]] = as.data.frame(cbind(plates[[i]],plateName_col))
      } else if(nrow(plates[[i]]) == 96){
        plateName_col = rep(plateNames[i],nrow(plates[[i]]))
        plates[[i]] <- as.data.frame(cbind(as_tibble(plates[[i]]),plateName_col))
      }
      else {
        #plateName_col = rep(plateNames[i], nrow(plates[[i]]))
        #plates[[i]] = as.data.frame(cbind(as_tibble(plates[[i]]),plateName_col))
        plateName_col = rep(plateNames[i],nrow(plates[[i]]))
        plates[[i]] <- as.data.frame(cbind(plates[[i]],plateName_col))
        
      }
      colnames(plates[[i]])[ncol(plates[[i]])] <- "Plate"
    }
    wholeDF = suppressWarnings(bind_rows(plates))
    wholeDF[,c("row", "col")] <- list(as.factor(wholeDF$row), as.factor(wholeDF$col))
    levels(wholeDF$row) <- LETTERS[1:8]
    levels(wholeDF$col) <- 1:12
    
    if(nrow(wholeDF) %% 96 == 0 & trait %in% colnames(wholeDF)){
      ggplot(wholeDF, aes_string(x = "Plate", y = trait, fill = "Plate")) + geom_bar(stat="identity") + facet_grid(row~col, drop=FALSE) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else if(trait %in% colnames(wholeDF)){
      ggplot(wholeDF, aes_string(x = "Plate", y = trait, fill = "Plate")) + geom_boxplot() + facet_grid(row~col, drop=FALSE) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } 
  }

#file <- '~/Documents/PI staining setup/sorting/adult sorting1_.txt'
file <- '~/Documents/PI staining setup/sorting/sorting_example.csv'
data <-plateread(file=file)
plate <- summarizePlate(plate=data)
plate <- removeNAWells(plate)


# plotting
plotTrait(plate= plate, trait = "n")
plotTrait(plate = plate, trait = "TOF", type="heat")

plotTrait(plate = data, trait = "TOF", type="hist")
plotTrait(plate = data, trait = "TOF","Green", type="line")
plotTrait(plate=data, "TOF", "Extinction", type="scatter")

plotCorMatrix(plate)

edgeEffect(plate)

file2 <- '~/Documents/PI staining setup/sorting/adult sorting1.txt'
data2 <-plateread(file=file, sep = '\t', droplast = 1)

file3 <- '~/Documents/PI staining setup/data/1.csv'
data2 <-plateread(file=file3, sep = ',', droplast = 1, reflx=FALSE)
plate <- summarizePlate(plate=data)



