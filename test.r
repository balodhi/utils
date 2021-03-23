

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
        plateName_col = rep("setup",nrow(plates[[i]]))
        plates[[i]] <- as.data.frame(cbind(plates[[i]],plateName_col))
        
      }
      #colnames(plates[[i]])[ncol(plates[[i]])] = "Plate"
      if(FALSE) {
      if (nrow(plates[[i]]) == 96) {
        print("plate")
        sapply(plates[[i]], class)
        c_factor_2_char <- sapply(plates[[i]], is.factor)
        plates[[i]][c_factor_2_char]<- lapply(plates[[i]][c_factor_2_char], as.character)
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


