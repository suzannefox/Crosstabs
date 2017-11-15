
# =======================================
# This version - 8th November 2017
# =======================================
# arguments
# Data - a dataframe
# Side - string variable with the variable name for the side
# Head - string variable with the variable name for the header
# BinsSide - number of bins to split side continuous variables into
# BinsHead - number of bins to split header continuous variables into
# diagnostics - if == 1 then show messages

crosstab <- function(Data, Side, Head, BinsSide=0, BinsHead=0, diagnostics=0) {
  library(dplyr)

  proc_start <- proc.time()
  # CHECK : variable names validity
  data.names <- names(Data)

  # ============================================================
  if (diagnostics==1) print("0.1 input checks - Side variable")

  result = tryCatch({
    class(Side)
  }, error = function(e) {
    return(FALSE)
  })
  
  if (result == FALSE) {
    print("ERROR IN SIDE VARIABLE : must be specified as a character")
    return()
  }
  
  if (class(Side) != "character") {
    print(paste0("ERROR IN SIDE VARIABLE : must be specified as a character"))
    return()
  }
  
  data.check <- setdiff(data.names, Side)
  if (identical(data.names, data.check)) {
    print(paste0("ERROR IN SIDE VARIABLE : ",Side," not found in the Data"))
    return()
  }
  
  # ============================================================
  if (diagnostics==1) print("0.1 input checks - Header variable")
  result = tryCatch({
    class(Head)
  }, error = function(e) {
    return(FALSE)
  })
  
  if (result == FALSE) {
    print("ERROR IN HEADER VARIABLE : must be specified as a character")
    return()
  }
  
  if (class(Side) != "character") {
    print(paste0("ERROR IN HEADER VARIABLE : must be specified as a character"))
    return()
  }
  
  data.check <- setdiff(data.names, Head)
  if (identical(data.names, data.check)) {
    print(paste0("ERROR IN HEADER VARIABLE : ",Head," not found in the Data"))
    return()
  }
  
  # ====================================================
  if (diagnostics==1) print("1.1 variable types")
  print(paste("Data :",class(Data)))
  print(paste("Side var :",Side,":",class(Data[,c(Side)])))
  print(paste("Head var :",Head,":",class(Data[,c(Head)])))

  # define numeric variable types
  vars.nums <- c("numeric","integer")
  
  # make a dataframe of the variables
  if (diagnostics==1) print("1.2 make df")
  #Data.In <- data.frame(Side, Head)
  Data.In <- Data[,c(Side, Head)]
  names(Data.In) <- c("Side", "Head")
  Data.Orig <- Data.In

  # keep a record of the original variable
  Data.In$SideOrig <- Data.In$Side
  Data.In$HeadOrig <- Data.In$Head
  
  # ================================================
  if (diagnostics==1) print("1.3 recode side")
  # recode into ranges if required
  
  if (BinsSide > 0 & class(Data.In$Side) %in% vars.nums) {  
    if (diagnostics==1) print("1.3 recoding")
    
    # if number of bins is less than or equal to
    # number of unique answers, ignore this step
    SideCount <- Data.In %>% distinct(Side) %>% count() %>% pull
    if (SideCount <= BinsSide) {
      if (diagnostics==1) print(paste0("1.3 Side var has only ",SideCount," distinct values"))
    } else {
      # split numerics into equal partitions
      Data.In <- Data.In %>%
        mutate(SideNew = cut(Side, BinsSide))
  
      Data.In$Side <- Data.In$SideNew
      Data.In <- Data.In %>%
        select(-SideNew)
    }
  }
  
  # ================================================
  if (diagnostics==1) print("1.4 recode head")
  # recode into ranges if required
  
  if (BinsHead > 0 & class(Data.In$Head) %in% vars.nums) {  
    if (diagnostics==1) print("1.4 recoding")
    
    # if number of bins is less than or equal to
    # number of unique answers, ignore this step
    HeadCount <- Data.In %>% distinct(Head) %>% count() %>% pull
    if (HeadCount <= BinsHead) {
      if (diagnostics==1) print(paste0("1.4 Header var has only ",HeadCount," distinct values"))
    } else {
      # split numerics into equal partitions
      Data.In <- Data.In %>%
        mutate(HeadNew = cut(Head, BinsHead))
      
      Data.In$Head <- Data.In$HeadNew
      Data.In <- Data.In %>%
        select(-HeadNew)
    }
  }
  
  # =============================================
  # get the basic crosstab
  if (diagnostics==1) print("2. basic dataframe")
  mytable.cell <- table(Data.In$Side, Data.In$Head) 
  
  # get margin totals
  if (diagnostics==1) print("2.1 margin totals")
  mytable.tots <- addmargins(mytable.cell)
  mytable.tots <- as.data.frame.matrix(mytable.tots)
  
  # =============================================
  # change cells to percentages
  if (diagnostics==1) print("2.2 percentages")
  #prop.table(mytable) # cell percentages
  #prop.table(mytable, 1) # row percentages
  mytable.cell <- prop.table(mytable.cell, 2) #col
  mytable.cell <- round(mytable.cell * 100, digits=0)
  mytable.cell <- as.data.frame.matrix(mytable.cell)
  mytable.cell <- mytable.cell %>%
    mutate_if(is.numeric, as.character)
  
  # add text for %
  if (diagnostics==1) print("2.3 text")
  for (i in 0:nrow(mytable.cell)) {
    if (i==0) next
    for (j in 0:ncol(mytable.cell)) {
      if (j==0) next
      mytable.cell[i,j] <- paste0(mytable.cell[i,j],"%")
    }
  }
  
  # =============================================
  # merge figure totals and percent totals
  if (diagnostics==1) print("2.4 merge values")
  mytable.df <- mytable.tots
  mytable.df[1:nrow(mytable.cell),1:ncol(mytable.cell)] <- mytable.cell
  
  # move col and row total to first element
  mytable.df <- mytable.df %>%
    select(Sum,everything())
  x.r <- nrow(mytable.df)
  mytable.df <- mytable.df[c(x.r,1:x.r-1),]
  
  # =============================================
  # rename the total column and row
  if (diagnostics==1) print("2.5 texts")
  colnames(mytable.df)[1] <- c("TOTAL")
  rownames(mytable.df)[1] <- c("TOTAL")
  
  mytable.df <- mytable.df %>% 
    mutate(names = rownames(.)) %>%
    select(names, everything()) %>%
    mutate_if(is.numeric, as.character)
  
  # =============================================
  # Add means for numeric side variables
  if (diagnostics==1) print("2.6 means for numerics")
  if (diagnostics==1) print(paste("2.6 SideOrig is",class(Data.In$SideOrig)))
  
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

    stat.1 <- stat.1 %>% mutate_if(is.factor, as.character)
    stat.2 <- stat.2 %>% mutate_if(is.factor, as.character)

    stat.3 <- data.frame(t(bind_rows(stat.1, stat.2)))
    stat.3 <- stat.3 %>%
      mutate(names = rownames(.)) %>%
      select(names, everything())

    names(stat.3) <- names(mytable.df)
    mytable.df <- mytable.df %>% mutate_if(is.factor, as.character)
    stat.3 <- stat.3 %>% mutate_if(is.factor, as.character)
    mytable.df <- bind_rows(mytable.df, stat.3)
  }
  
  # =============================================
  # return the result
  proc_final <- proc.time() - proc_start
  print(paste0("Time taken : ",proc_final[1]," secs"))
  
  if (diagnostics==1) print("3. finished")
  return(mytable.df)
  
}