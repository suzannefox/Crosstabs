
# =======================================
# This version - 7th October 2017
# =======================================

crosstab <- function(Data, Side, Head, TileSide=0, TileHead=0, diagnostics=0) {
  library(dplyr)

  if (diagnostics==1) print("1.1 variable types")
  print(paste("Data :",class(Data)))
  print(paste("Side var :",Side,":",class(Data[,c(Side)])))
  print(paste("Head var :",Head,":",class(Data[,c(Head)])))
  
  # if (is.na(nrow(Side))) {
  #   print("ERROR : No data in Side var")
  #   break
  # }
  # if (is.na(nrow(Head))) {
  #   print("ERROR : No data in Head var")
  #   break
  # }
  # if (nrow(Side) != nrow(Head)) {
  #   print("ERROR : Side and Head must have equal row count")
  #   break
  # }
  
  # define numeric variable types
  vars.nums <- c("numeric","integer")

  # make a dataframe of the variables
  if (diagnostics==1) print("1.2 make df")
  #Data.In <- data.frame(Side, Head)
  Data.In <- Data[,c(Side, Head)]
  names(Data.In) <- c("Side", "Head")
  Data.Orig <- Data.In
  
  # ================================================
  if (diagnostics==1) print("1.3 recode side")
  # recode into ranges if required
  
  if (TileSide > 0 & class(Data.In$Side) %in% vars.nums) {  
    if (diagnostics==1) print("1.3 recoding")
    
    # split numerics into equal partitions
    Data.In$SideNew <- dplyr::ntile(Data.In$Side, TileSide) 
    
    SideNew.labs <- Data.In %>% 
      group_by(SideNew) %>%
      summarise(m1 = min(Side), m2 = max(Side)) %>%
      mutate(ans = paste0(SideNew," : ",m1,"-",m2)) %>%
      pull()
    
    Data.In$SideOrig <- Data.In$Side
    Data.In$Side <- factor(Data.In$SideNew, labels = SideNew.labs) 
    Data.In <- Data.In %>%
      select(-SideNew)
  }

  # ================================================
  if (diagnostics==1) print("1.4 recode head")
  if (TileHead > 0 & class(Data.In$Head) %in% vars.nums) {
    if (diagnostics==1) print("1.4 recoding")
    Data.In$HeadNew <- dplyr::ntile(Data.In$Head, TileHead) 
    
    HeadNew.labs <- Data.In %>% 
      group_by(HeadNew) %>%
      summarise(m1 = min(Head), m2 = max(Head)) %>%
      mutate(ans = paste0(HeadNew," : ",m1,"-",m2)) %>%
      pull()
    
    Data.In$Head <- factor(Data.In$HeadNew, labels = HeadNew.labs) 
    
    Data.In <- Data.In %>%
      select(-HeadNew)
  }

  # =============================================
  # get the basic crosstab
  if (diagnostics==1) print("2. basic dataframe")
  mytable.cell <- table(Data.In$Side, Data.In$Head) 
  
  # get margin totals
  mytable.tots <- addmargins(mytable.cell)
  mytable.tots <- as.data.frame.matrix(mytable.tots)
  
  # =============================================
  # change cells to percentages
  #prop.table(mytable) # cell percentages
  #prop.table(mytable, 1) # row percentages
  mytable.cell <- prop.table(mytable.cell, 2) #col
  mytable.cell <- round(mytable.cell * 100, digits=0)
  mytable.cell <- as.data.frame.matrix(mytable.cell)
  mytable.cell <- mytable.cell %>%
    mutate_if(is.numeric, as.character)
  
  # add text for %
  for (i in 0:nrow(mytable.cell)) {
    if (i==0) next
    for (j in 0:ncol(mytable.cell)) {
      if (j==0) next
        mytable.cell[i,j] <- paste0(mytable.cell[i,j],"%")
    }
  }
  
  # =============================================
  # merge figure totals and percent totals
  mytable.df <- mytable.tots
  mytable.df[1:nrow(mytable.cell),1:ncol(mytable.cell)] <- mytable.cell
  
  # move col and row total to first element
  mytable.df <- mytable.df %>%
    select(Sum,everything())
  x.r <- nrow(mytable.df)
  mytable.df <- mytable.df[c(x.r,1:x.r-1),]
  
  # =============================================
  # rename the total column and row
  colnames(mytable.df)[1] <- c("TOTAL")
  rownames(mytable.df)[1] <- c("TOTAL")
  
  mytable.df <- mytable.df %>% 
    mutate(names = rownames(.)) %>%
    select(names, everything()) %>%
    mutate_if(is.numeric, as.character)
  
  # =============================================
  # Add means for numeric side variables

  if (class(Data.In$SideOrig) %in% vars.nums) {  
    stat.1 <- Data.In %>%
      summarise(Mean=mean(SideOrig,na.rm = TRUE),
                StdDev=sd(SideOrig,na.rm=TRUE),
                Median=median(SideOrig,na.rm=TRUE)) %>%
      mutate(STATS="TOTAL",
             Mean=round(Mean, digits=2),
             StdDev=round(StdDev, digits=2),
             Median=round(Median, digits=2)) %>%
      select(STATS, Mean, StdDev, Median) %>%
      mutate_if(is.numeric, as.character)
  
    stat.2 <- Data.In %>%
      group_by(Head) %>%
      summarise(Mean=mean(SideOrig,na.rm=TRUE),
                StdDev=sd(SideOrig,na.rm=TRUE),
                Median=median(SideOrig,na.rm=TRUE)) %>%
      mutate(STATS=Head,
             Mean=round(Mean, digits=2),
             StdDev=round(StdDev, digits=2),
             Median=round(Median, digits=2)) %>%
      select(STATS, Mean, StdDev, Median) %>%
      mutate_if(is.numeric, as.character)
  
    stat.3 <- data.frame(t(bind_rows(stat.1, stat.2)))
    stat.3 <- stat.3 %>%
      mutate(names = rownames(.)) %>%
      select(names, everything())

    names(stat.3) <- names(mytable.df)
    mytable.df <- bind_rows(mytable.df, stat.3)
  }

  # =============================================
  # return the result
  return(list(mytable.df, Data.Tab, stat.3))
  
}