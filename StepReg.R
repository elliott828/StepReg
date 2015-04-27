
# STEP 0.1
# Download all related packages
all.pcg <- c("rJava", "xlsxjars", "xts", "xlsx", 
             "car","MASS","plyr","lmtest","zoo","stats", "plyr", "tidyr", "dplyr",
             "lubridate", "ggplot2", "reshape", "Rcpp", "colorspace")

req.pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, function(x)suppressMessages(require(x, character.only = TRUE)))
}

req.pkg(all.pcg)
# invisible(req.pkg(all.pcg))



StepReg() <- function{
  
  
  # STEP 1.2
  # Read Data
  repeat{
    data.name <- readline("| Please enter the name of raw data file: ")
    endstr <- substr(data.name, nchar(data.name)-3, nchar(data.name))
    cat("\n")
    if (!endstr %in% c(".csv", "xlsx", ".xls")){
      message('| Only "*.csv", "*.xlsx" or "*.xls" file is expected!\n')
    }else if(!file.exists(data.name)){
      message('| "', data.name, '" does not exist in your work directory!')
      message('| Please re-check it in the file explorer!')
      readline('| Press <Enter> to reload the DDI program...')
      cat("\n")
      cat(paste(rep("-", getOption("width")-2), collapse = ""))
      cat("\n")
      return(StepReg())
    }else if(endstr == ".csv"){
      data <- read.csv(data.name, stringsAsFactors = F)
      break
    }else{
      sht.names <- names(getSheets(loadWorkbook(data.name)))
      repeat{
        sht.name <- readline("| Please enter the name of worksheet: ")
        cat("\n")
        if (!sht.name %in% sht.names){
          message('| The worksheet "', sht.name, '" is not found!\n')
        }else{
          break
        }
      }
      data <- read.xlsx(data.name, sheetName = sht.name, stringsAsFactors = F)
      break
    }
  }
  message('| "',data.name, '" is loaded.') 
  message('| There are ', dim(data)[1], ' observations and ', 
          dim(data)[2], ' variables in the raw dataset.\n')
  
  
  ######################################
  ## STEP 2 DATA CREATION & TRANSFORM ##
  ######################################
  
  # STEP 2.1 choose the response variable
  repeat{
    resp <- readline("| Please enter the name of response variable: ")
    cat("\n")
    if (resp %in% names(data)){
      break
    }else{
      message(paste('| The variable "', resp, 
                    '" does not exist in the database!', sep=""))
      cat("\n")
    }    
  }
  
  # STEP 2.2 choose the time-line variable and its format
  repeat{
    tvar <- readline("| Please enter the name of time variable: ")
    cat("\n")
    if (tvar %in% names(data)){
      break
    }else{
      message(paste('| The variable "', resp, 
                    '" does not exist in the database!', sep=""))
      cat("\n")
    }    
  }
  
  repeat{
    cat("| Which kind of date format is it in the raw data? \n")
    cat("|   1. m/d/y\n|   2. d/m/y\n|   3. y/m/d\n")
    t_f <- readline("\n| Please choose a format number: ")
    cat("\n")
    if(!t_f %in% c("1","2","3")){
      message("| Only a number between 1 and 3 is acceptable!\n")
    }else{
      t_f <- as.numeric(t_f)
      break
    }
  }
  
  
  
}