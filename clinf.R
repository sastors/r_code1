cond <- function(dsin = NULL, cond = 1==1){
  if (missing(dsin)) {
    stop("Please input dataset!")
  }
  if(!is.data.table(dsin)){
    clinf_tmp1 <- as.data.table(dsin)
  }else {
    clinf_tmp1 <- dsin
  }
  invisible(clinf_tmp1[eval(substitute(cond)), ])
  
}

vars <- function(dsin = NULL, var_row = NULL, var_col = NULL, var_by = NULL, rbind = TRUE, ord = c("a", "-a", "n", "-n")){
  
  ord <- match.arg(ord)
  
  if (missing(dsin)){
    stop("Please input dataset!")
  }
  if(!is.data.table(dsin)){
    clinf_tmp1 <- as.data.table(dsin)
  }else {
    clinf_tmp1 <- dsin
  }
  #remove row viriable is na
  clinf_tmp1[!is.na(get(var_row[1])), ]
  
  
  #id variable
  if ("USUBJID" %in% toupper(names(dsin))){
    idkey = "USUBJID"
  }else if("SUBJID" %in% toupper(names(dsin))){
    idkey = "SUBJID"
  }else if("SUBJECT" %in% toupper(names(dsin))){
    idkey = "SUBJECT"
  }else {
    stop("USUBJID, SUBJID or SUBJECT does not exist in dataset!")
  }
  
  #check input arguments
  if (!missing(var_row)){
    for (i in 1:length(var_row)){
      var_rown <- NULL
      if (!paste0(var_row[i],"N") %in% toupper(names(dsin))){
        clinf_tmp1[,.(as.name(paste0(var_row[i],"N")) := var_row[i])]
        var_rown <- c(var_rown, paste0(var_row[i],"N"))
      }else{
        var_rown <- c(var_rown, paste0(var_row[i],"N"))
      }
      if (!var_row[i] %in% toupper(names(dsin))){
        stop(paste0(var_row[i], " not exist in ", dsin))
      }
    }
  } 
  
  if (!missing(var_by)){
    for (i in 1:length(var_by)){
      if (!var_by[i] %in% toupper(names(dsin))){
        stop(paste0(var_by[i], " not exist in ", dsin))
      }
    }
  } 
  
  if (!missing(var_col)){
    for (i in 1:length(var_col)){
      for (j in 1:length(var_col[[i]])){
        if (!var_col[[i]][j] %in% toupper(names(dsin))){
          stop(paste0(var_col[[i]][j], " not exist in ", dsin))
        }
      }
    }
  }  
  
  #check the rows, if 0 return the dsin
  if (clinf_tmp1[,.N]==0){
    print("Input dataset has 0 record, consider dummy result.")
    invisible(clinf_tmp1)
  }else {
    clinf_tmp2 <- NULL
    #loop the var_col, var_col = c("AESOC", "AEDECOD")
    if (is.atomic(var_col) == TRUE){
      clinf_tmp2 <- dcast(
        merge(
          #count events
          clinf_tmp1[, .(M_COUNT = .N), by = c(var_row, var_rown, var_col, var_by)],
          #count ids
          unique(clinf_tmp1, by = c(idkey, var_row, var_rown, var_col, var_by))[, .(N_COUNT = .N), by = c(var_row, var_rown, var_col, var_by)],
          by = c(var_row, var_rown, var_col, var_by)
        ),
        as.formula(paste(paste(unique(c(var_col, var_by)), collapse = " + "), paste(paste0("reorder(factor(", var_row, "), ", var_rown, ")"), collapse = " + "), sep = " ~ ")), value.var = c("M_COUNT", "N_COUNT")
      )
    }else {
      #loop the var_col, var_col = list("STUDYID", c("AESOC", "AEDECOD"), "AEDECOD")
      for (i in 1:length(var_col)){
        clinf_tmp3 <- dcast(
          merge(
            #count events
            clinf_tmp1[, .(M_COUNT = .N), by = c(var_row, var_rown, var_col[[i]], var_by)],
            #count ids
            unique(clinf_tmp1, by = c(idkey, var_row, var_rown, var_col[[i]], var_by))[, .(N_COUNT = .N), by = c(var_row, var_rown, var_col[[i]], var_by)],
            by = c(var_row, var_rown, var_col[[i]], var_by)
          ),
          as.formula(paste(paste(unique(c(var_col[[i]], var_by)), collapse = " + "), paste(paste0("reorder(factor(", var_row, "), ", var_rown, ")"), collapse = " + "), sep = " ~ ")), value.var = c("M_COUNT", "N_COUNT")
        )[, LOOP_I := i]
        
        if (rbind == TRUE){
          clinf_tmp2 <- rbind(clinf_tmp2, clinf_tmp3, fill=TRUE)
        }else {
          clinf_tmp2 <- cbind(clinf_tmp2, clinf_tmp3)
        }
        
        if (!missing(ord)){
          names_row <- sub("^N_COUNT_", "", names(clinf_tmp3)[grepl("^N_COUNT_", names(clinf_tmp3))])
          names_row_m <- paste0("M_COUNT_", names_row)
          names_row_n <- paste0("N_COUNT_", names_row)
          names_row_n2 <- paste0("ORD_", names_row, "_", i)
          names_row_keep <- c(var_col[[i]], names_row_n)
          
          clinf_tmp4 <- clinf_tmp3[, ..names_row_keep]
          setnames(clinf_tmp4, names_row_n, names_row_n2)
          assign(paste0("clinf_ord", i), clinf_tmp4)
        }
 
      }
      if (!missing(ord)){
        #left join table
        for (i in 1:length(var_col)){
          clinf_tmp2 <- get(paste0("clinf_ord", i))[clinf_tmp2, on = var_col[[i]]]
        }
        
        if (grepl("a", ord)){
          if (grepl("-", ord)){
            setorderv(clinf_tmp2, c("LOOP_I", unlist(var_col)), c(1, rep.int(-1, length(unlist(var_col)))))
          } else{
            setorderv(clinf_tmp2, c("LOOP_I", unlist(var_col)))
          }
        }else if(grepl("n", ord)){
          a <- NULL
        }
        
      }
      
    }
    
    #fill 0 with na
    names_row <- sub("^N_COUNT_", "", names(clinf_tmp2)[grepl("^N_COUNT_", names(clinf_tmp2))])
    names_row_m <- paste0("M_COUNT_", names_row)
    names_row_n <- paste0("N_COUNT_", names_row)

    for (i in c(names_row_m, names_row_n)){
      clinf_tmp2[is.na(get(i)), (i) := 0] 
    }

    invisible(clinf_tmp2)
    
  }

}  

pops <- function(dsin = NULL, popds = adsl, cond = NULL, var_row = NULL, var_by = NULL){
  if (missing(dsin)){
    stop("Please input dataset!")
  }
  if(!is.data.table(dsin)){
    clinf_tmp1 <- as.data.table(dsin)
  }else {
    clinf_tmp1 <- dsin
  }
  
  if (!(exists(deparse(substitute(popds))) & (is.data.frame(popds) | (is.data.table(popds))))){
    stop(paste0(popds, " dose noe exist!"))
  }
  if(!is.data.table(popds)){
    clinf_tmp2 <- as.data.table(popds)
  }else {
    clinf_tmp2 <- popds
  }
  
  #id variable
  if ("USUBJID" %in% toupper(names(popds))){
    idkey = "USUBJID"
  }else if("SUBJID" %in% toupper(names(popds))){
    idkey = "SUBJID"
  }else if("SUBJECT" %in% toupper(names(popds))){
    idkey = "SUBJECT"
  }else {
    stop("USUBJID, SUBJID or SUBJECT does not exist in dataset!")
  }
  
  #check input arguments
  if (!missing(var_row)){
    for (i in 1:length(var_row)){
      if (!var_row[i] %in% toupper(names(popds))){
        stop(paste0(var_row[i], " not exist in ", popds))
      }
    }
  } 
  
  if (!missing(var_by)){
    for (i in 1:length(var_by)){
      if (!var_by[i] %in% toupper(names(popds))){
        stop(paste0(var_by[i], " not exist in ", popds))
      }
    }
  } 
  
  clinf_tmp2[eval(substitute(cond)), ]
  clinf_tmp2[!is.na(get(var_row[1])), ]
  
  
  if (!is.null(var_by)){
    clinf_tmp3 <- dcast(
      #count ids
      clinf_tmp2[, .(POP_COUNT = .N), by = c(var_row, var_by)],
      
      as.formula(paste(paste(var_by, collapse = " + "), paste(var_row, collapse = " + "), sep = " ~ ")), value.var = c("POP_COUNT")
    )
    names_row <- names(clinf_tmp3)[paste0("N_COUNT_", names(clinf_tmp3)) %in% names(clinf_tmp1)]
    names_row_pop <- paste0("POP_COUNT_", names_row)
    setnames(clinf_tmp3, names_row, names_row_pop)
    
    invisible(merge(clinf_tmp1, clinf_tmp3, by = var_by))
    
  }else {
    clinf_tmp3 <- dcast(
      #count ids
      clinf_tmp2[, .(POP_COUNT = .N), by = c(var_row)],
      
      as.formula(paste(".", paste(var_row, collapse = " + "), sep = " ~ ")), value.var = c("POP_COUNT")
    )
    
    names_row <- names(clinf_tmp3)[paste0("N_COUNT_", names(clinf_tmp3)) %in% names(clinf_tmp1)]
    names_row_p <- paste0("PERCENT_", names_row)
    names_row_n <- paste0("N_COUNT_", names_row)
    names_row_pop <- paste0("POP_COUNT_", names_row)
    setnames(clinf_tmp3, names_row, names_row_pop)
    
    invisible(cbind(clinf_tmp1, clinf_tmp3)[, (names_row_p) := Map(`/`, mget(names_row_n), mget(names_row_pop))])
    
  }
  
} 

form <- function(dsin = NULL, decimal = 1, style = c("N", "M", "N (X)", "N (X%)", "N [M] (X)", "N [M] (X%)")){
  if (missing(dsin)){
    stop("Please input dataset!")
  }
  if(!is.data.table(dsin)){
    clinf_tmp1 <- as.data.table(dsin)
  }else {
    clinf_tmp1 <- dsin
  }
  
  style <- match.arg(style)
  
  #set decimal
  names_row <- sub("^N_COUNT_", "", names(clinf_tmp1)[grepl("^N_COUNT_", names(clinf_tmp1))])
  names_row_p <- paste0("PERCENT_", names_row)
  names_row_n <- paste0("N_COUNT_", names_row)
  names_row_pop <- paste0("POP_COUNT_", names_row)
  
  clinf_tmp1[, (names_row_p) := round(.SD, decimal), .SDcols = names_row_p]
  
  #switch(style, mean = mean(x), median = median(x), trimmed = mean(x, trim = 0.1))
  
  for (t in names_row){
    clinf_tmp1[, eval(substitute(t)) := gsub("X", mget(paste0("PERCENT_", t)), gsub("M", mget(paste0("M_COUNT_", t)), gsub("N", mget(paste0("N_COUNT_", t)), style))), by = list(row.names(clinf_tmp1))]
  }
  
  invisible(clinf_tmp1)
} 

ord <- function(dsin = NULL, style = c("a", "-a", "n", "-n")){
  if (missing(dsin)){
    stop("Please input dataset!")
  }
  if(!is.data.table(dsin)){
    clinf_tmp1 <- as.data.table(dsin)
  }else {
    clinf_tmp1 <- dsin
  }
  
  style <- match.arg(style)
  
  #N_COUNT
  names_row <- sub("^N_COUNT_", "", names(clinf_tmp1)[grepl("^N_COUNT_", names(clinf_tmp1))])
  names_row_n <- paste0("N_COUNT_", names_row)
  
  for (i in 1:eval(max(check$LOOP_I)-1)){
    
    
  }
  
  invisible(clinf_tmp1)
} 

check <- adae %>% cond(SAFFL =="Y" & AESER == "Y") %>% 
  vars(var_row = c("TRTA"), var_col = list("STUDYID", "AESOC", c("AESOC", "AEDECOD")), ord = "a") %>% 
  pops(var_row = c("TRT01A")) %>% 
  form(decimal = 2, style = "N (X%)")
