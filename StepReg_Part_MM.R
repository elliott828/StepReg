e <- new.env()

##==================================================================================
## FUNDAMENTAL FUNCTIONS                 | 
## 1. co(x, co.rate)                     | ~ carryover effect transformation
## 2. sc(x, lamda1, lamda2)              | ~ s-curve transformation
## 3. pc(x, exponent) #abandonned        | ~ power curve transformation
## 4. cs(x, co.rate, lamda1, lamda2)     | ~ carryover + s-curve transformation
## 5. cp(x, co.rate, expnent)            | ~ carryover + power curve transformation
##==================================================================================

#-------------------#
# 1. co(x, co.rate) #
#-------------------#

co <- function(x, co.rate){
  #---------------------------------
  # carry over effect
  # formula: period2 = period1 * carryover rate + period2
  # x: variable to be transformed
  # co.rate: carry over rate
  # make sure the 'x' already exists in the environment
  #---------------------------------
  
  for (p in 2:length(x)){
    if (is.na(x[p-1])){
      # p: position of the element
      x[p] = x[p]
    }else{
      x[p] = x[p-1] * co.rate + x[p]
    }
  }
  
  return(x)
}  

#=====================================================================

#--------------------------#
# 2. sc(x, lamda1, lamda2) #
#--------------------------#

sc <- function(x, lamda1, lamda2) 1 - exp(-lamda1 * x^lamda2)
# s-curve transformation
# formula: 1-e^(-lamda1 * x^lamda2)
# x: variable to be transformed
# lamda1 & lamda2: 2 parameters of the s-curve
# make sure the 'x' already exists in the environment

#=====================================================================

#--------------------#
# 3. pc(x, exponent) #
#--------------------#

# pc <- function(x,exponent) x^exponent
# power curve transformation
# formula: x^i
# x: variable to be transformed
# exponent: exponent...
# make sure the 'x' already exists in the environment

#=====================================================================

#-----------------------------------#
# 4. cs(x, co.rate, lamda1, lamda2) #
#-----------------------------------#

cs <- function(x, co.rate, lamda1, lamda2){
  # carryover + s-curve
  # x <- sc(co(x, co.rate), lamda1, lamda2)
  x <- 1 - exp(-lamda1 * co(x, co.rate)^lamda2)
  #x
}

#=====================================================================

#-----------------------------------#
# 5. cp(x, co.rate, exponent) #
#-----------------------------------#

cp <- function(x, co.rate, exponent){
  # carryover + power curve
  # x <- pc(co(x, co.rate), exponent)
  x <- co(x, co.rate)^exponent
  #x
}


#-------------------#
# trial()           #
#-------------------#



#---------#
# recom() #
#---------#

recom <- function(pred, resp, df, type, fit = NULL, st.row){
  #---------------------------------
  # pred: predictor to be inserted to the model, to be quoted. i.e.: "cly" (mtcars)
  # resp: response variable in the model, to be quoted. i.e.: "mpg" (mtcars)
  # df: data base for the model
  # type: transformation method for the predictor
  # fit: default value is NULL; if a model exists, then put the name of fit list
  # st.row: modeling starting row
  #---------------------------------
  # When type = 1: test all combinations of parameters of carryover & s-curve
  # When type = 2: test all combinations of parameters of carryover & power curve
  # When type = 3: do both of the actions above
  # Check the best R-square / p-value of coef, give recommendations
  #---------------------------------
  
  #----------------------#
  # PART I - Preparation #
  #----------------------#
  if(!"plyr" %in% installed.packages()){
    install.packages("plyr")
    require(plyr)
  }
  
  if(!"plyr" %in% installed.packages()){
    install.packages("dplyr")
    require(dplyr)
  }
  
  #---------------------------------
  
  # Load the data

  df1 <- df[st.row:nrow(df), ]  # df to be modeled
  x0 <- df[[pred]]
  y0 <- df[[resp]]
  x1 <- x0[st.row:length(x0)]
  y1 <- y0[st.row:length(y0)]
  
  #---------------------------------#
  # PART II - Fundamental functions #
  #---------------------------------#
  
  # The default parameter ranges are as below:
  # (To be modified on demand)
  co.range <- seq(0.05, 0.95, 0.05)
  lamda1.range <- seq(0.0001, 0.0007, 0.0001)
  lamda2.range <- seq(1.1, 1.7, 0.1)
  pc.range <- seq(0.4, 0.95, 0.05)
  
  # Function 1.1
  #---------------------------------
  # Set sub functions (cs.trans & cp.trans) for generating predictor matrix 
  # (transformed by different group of parameters)
  cstrans <- function(x){
    cs.mat <- t(mdply(expand.grid(co.rate = co.range, 
                                  lamda1 = lamda1.range, 
                                  lamda2 = lamda2.range),
                      cs, x))
    # return(cs.mat)
  }
  
  # Function 1.2
  #---------------------------------
  cptrans <- function(x){
    cp.mat <- t(mdply(expand.grid(co.rate = co.range, 
                                  exponent = pc.range),
                      cp, x))
    # return(cp.mat)
  }
  
  # Function 2
  #---------------------------------
  # Check if a fit exists, update the fit if it does, otherwise create one
  fit.update <- function(resp, x, fit.check = NULL, pred, df){
    df[[pred]] <- x
    fit.new <-  if (is.null(fit.check)){
      lm(as.formula(sprintf("%s ~ %s", resp, pred)), data = df)
    }else{
      update(fit.check, as.formula(sprintf("~. + %s", pred)), data = df)
    }
    # return(fit.new)
  }
  
  # Function 3
  #---------------------------------
  # Extract the key modeling summary statatistics
  # Call Function 2: fit.update()
  summary.stats <- function(resp, x, fit.coef = fit, pred, df){
    smr <- summary(fit.update(resp, x, fit.coef, pred, df))
    smr.coef <- subset(smr$coefficients, rownames(smr$coefficients)==pred)[c(1,4)]
    smr.key <- c(smr.coef, smr$r.squared, smr$adj.r.squared)
    names(smr.key) <- c("Coefficient", "P.value", "R.squared", "Adj.R.squared")
    # to generate a group of key statistics, how many variables corresponding to how many groups
    return(t(smr.key))
  }
  
  # Function 4
  #---------------------------------
  # Test all the combinations of parameters & the respective modeling result
  testall <- function(resp, x, pred, fit.coef = fit, type, df){
    opt <- LETTERS[as.numeric(type)]
    #both <- function(x)list(cstrans(x),cptrans(x))
    
    mat <- switch(opt,
                  A = cstrans(x),
                  B = cptrans(x))
    
    len <- switch(opt,
                  A = 3,
                  B = 2)
    
    nam <- switch(opt,
                  A = "cs",
                  B = "cp")
    
    met <- switch(opt,
                  A = "Carryover + S-curve",
                  B = "Carryover + Power curve")
    
    prmt <- mat[1:len,]    # capture the parameter combinations
    var0 <- as.data.frame(mat[(len+1):nrow(mat),])    # capture the transfored variables
    #colnames(var) <- paste(pred, 1:ncol(var), sep = "")
    var <- var0[st.row:dim(var0)[1], ]
    
    test.stats <- t(sapply(var, summary.stats, resp = resp, pred = pred, 
                           fit.coef = fit.coef, df = df))
    # coef <- test.stats[1:2]
    # rsq <- test.stats[3]
    # adj.rsq <- test.stats[4]
    
    prmt.all <- as.data.frame(cbind(t(prmt), test.stats))
    prmt.all <- prmt.all[complete.cases(prmt.all), ]
    if(nrow(prmt.all) == 0){
      op.recom <- as.data.frame(matrix(1, nrow = 1, ncol = 9))
      message("This variable cannot get into the model!\n")
      message("Please switch another variable!\n")
    } else {
      curve.prmt <- ifelse(len == 2, "exponent", c("lamda1","lamda2"))
      colnames(prmt.all) <- c("carryover.rate", curve.prmt, "coef",
                              "p-value", "r.squared", "adj.r.squared")
      rownames(prmt.all) <- NULL
      
      # export all parameters and related model statistics to local working directory
      write.csv(prmt.all, paste("prmt", nam, pred, "csv",sep = "."))
      message(paste("\nThe parameter reference: 'prmt", nam, 
                    pred, "csv' is exported!",sep = "."))
      
      # capture the best group of parameters
      # get the best adj. r squared first then allocate the position
      posi <- which.max(as.numeric(prmt.all[, ncol(prmt.all)]))
      
      best.stats <- prmt.all[posi,]
      rownames(best.stats) <- NULL   # remove the row index assigned automatically by the program
      
      # print(paste("the size of prmt.all is ", paste(dim(prmt.all),collapse=" and "),sep=""))
      # print(paste("the value of best is ", best,sep=""))
      # print(paste("the best row number is ", which(round(prmt.all$adjusted.r.squared,6)==round(best,6)), sep =""))
      # print(paste("the size of best.stats is ", paste(dim(best.stats),collapse=" and "),sep=""))
      
      # print the message indicating the best transformation parameters and results
      cat("",
          paste(" For the transformation method ", met, ":", sep=""),
          paste(" ", paste(rep("-",40), collapse = ""),sep = ""),
          paste(" - The recommended carryover rate is ", best.stats[1], sep=""),
          sep = "\n")
      
      if(as.character(type)=="1"){
        cat(paste(" - The recommended lamda1(S-curve) is ", best.stats[2], sep = ""),
            paste(" - The recommended lamda2(S-curve) is ", best.stats[3], sep = ""),
            sep = "\n")
        curve.prmt <- c(best.stats[1], NA, best.stats[2], best.stats[3],
                        best.stats[ncol(prmt.all)])
      }else if(as.character(type)=="2"){
        cat(paste(" - The recommended power rate is ", best.stats[2], sep = ""),
            sep = "\n")
        curve.prmt <- c(best.stats[1], best.stats[2], NA, NA,
                        best.stats[ncol(prmt.all)])
      }
      
      cat(paste(" ", paste(rep("-",40), collapse = ""),sep = ""),"\n")
      
      if(is.na(best.stats[ncol(prmt.all)-2])){
        message("This variable is not advised to be added to the model!\n")
      }else if (best.stats[ncol(prmt.all)-2] > 0.2){
        message("Please be aware that the p-value of predictor coefficient is larger than 0.2!")
        message("The estimate of coefficient is not significant!\n")
      }
      
      names(curve.prmt) <- c("Carryover.Rate","Power.Rate","Lamda1","Lamda2","Adj.r2")
    }
    
    return(curve.prmt)
  }
  
  #-------------------------------------#
  # PART III - Application of functions #
  #-------------------------------------#
  
  if(as.character(type) == "1"){
    # carryover + s-curve only
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 1, data = df1)
  }else if(as.character(type) == "2"){
    # carryover + power curve only
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 2, data = df1)
  }else if(as.character(type) == "3"){
    # compary carryover + s-curve and carryover + power curve 
    prmt.cs <- testall(resp, x0, pred, fit.coef = fit, type = 1, data = df1)
    prmt.cp <- testall(resp, x0, pred, fit.coef = fit, type = 2, data = df1)
    if(is.na(prmt.cs[1])|is.na(prmt.cs[1])){
      prmt.rec <- rep(NA, 5)
      message("There is no recommendation!\n")
    }else if(as.numeric(prmt.cs[5]) > as.numeric(prmt.cp[5])){
      prmt.rec <- prmt.cs
      message("Concerning r-squared, the method **CARRY-OVER + S-CURVE** is preferred.\n")
    }else if(as.numeric(prmt.cs[5]) < as.numeric(prmt.cp[5])){
      prmt.rec <- prmt.cp
      message("Concerning r-squared, the method **CARRY-OVER + POWER CURVE** is preferred.\n")
    }else if(as.numeric(prmt.cs[5]) == as.numeric(prmt.cp[5])){
      prmt.rec <- prmt.cp
      message("Both transformation methods are OK for the model.\n")
    }
  }
  return(prmt.rec[1:4])
}

#---------#
# modif() #
#---------#

modif <- function(pred, data, tm.prmt){
  #---------------------------------
  # transform specified variable with(out) parameters
  # this function returns **a dataset** with selected variable transformed
  # make sure the raw data is already loaded into global environment
  #---------------------------------
  # pred: the name of variable to be transormed of class "character"
  # data: data frame to store the variable transformed (original var. will be covered)
  # tm.prmt: transformation parameters
  #---------------------------------
  
  df <- data
  co.r <- tm.prmt[1]
  pc.r <- tm.prmt[2]
  sc.1 <- tm.prmt[3]
  sc.2 <- tm.prmt[4]
  
  if(co.r == NA){
    df[[pred]] <- data[[pred]]
  }else if(pc.r == NA){
    df[[pred]] <- cs(data[[pred]], co.r, sc.1, sc.2)
  }else{
    df[[pred]] <- cp(data[[pred]], co.r, pc.r)
  }
  return(df)
}

#-------------#
# questions() #
#-------------#
questions <- function(index, pred = NULL, df = NULL, coef = NULL, opt = NULL){
  # index: number of question to be used
  # df: data frame, class - data.frame 
  # coef: fit$coef, class - data.frame
  # opt: add a vriable to (1) or remove one (2) from the model
  
  # index = 1
  q2.2 <- function(){
    # Question 2.2 "What's next move?"
    repeat{
      cat("| What is the next move?",
          "|  1. Continue modelling & add a predictor!",
          "|  2. Remove a predictor from the model.",
          "|  3. Stop! Show me the final model and the statistics!",
          "", sep = "\n")
      opt <- readline("| Enter an option number please: ")
      cat("\n")
      if(!opt %in% as.character(1:3)){
        message("| Only the number between 1 and 3 is acceptable, dude!\n")
      }else{
        opt <- as.numeric(opt)
        break
      }
    }
  }
  
  # index = 2
  q2.2.1_2 <- function(df, coef, opt){
    # Question 2.2.1 / 2.2.2 "Indicate the predictor's name to be added to the model"
    # also applicable for question at step 2.2.2: 
    # 	"Indicate the predictor's name to be removed from the model"
    repeat{
      pred.name <- readline("| Please indicate the name of predictor: ")
      cat("\n")
      if(opt == 1){
        # Check if the predictor exists in the df 
        if(!pred.name %in% names(df)){
          message('| The predictor "', pred.name, '" does not exist in the dataset!\n')
        }else if (pred.name %in% names(coef)){
          message('| The predictor "', pred.name, '" already exists in the model!\n')
        }else break
      }else{
        if(!pred.name %in% names(coef)){
          message('| The predictor "', pred.name, '" does not exist in the model!\n')
        }else break
      }
    }
  }
  
  # index = 3
  q2.2.1.1 <- function(pred){
    # Question 2.2.1.1 "Transformation method"
    repeat{
      cat(
        paste('Which kind of transformation does the variable "', pred, '" need to be adapted?', sep=""),
        "  1. carry over + S curve",
        "  2. carry over + power curve",
        "  3. auto-selection between 1 and 2",
        "",
        "  0. no transformation", "", sep="\n")
      opt <- readline("| Please enter an option: ")
      cat("\n")
      if(!opt %in% as.character(0:3)){
        message("| Only the number between 0 and 3 is acceptable, dude!\n")
      }else{
        opt <- as.numeric(opt)
        break
      }
    }
  }
  
  # index = 4
  q2.2.1.3 <- function(){
    # Question 2.2.1.3 - "Are you satisfied with the recommended parameters?"
    repeat{
      cat('| Are you satisfied with the recommended parameters? ',
          '|   1. I want to change the parameters',
          '|   2. I want to change the transformation method',
          '|   3. I want to change a predictor',
          '|   4. Yes, I am OK with the transformation',
          '', '\n')
      satis <- readline("| Please enter an option: ")
      cat("\n")
      if(!toupper(satis) %in% as.character(1:4)){
        message("| Only a number between 1 and 4 is acceptable!\n")
      }else{
        satis <- as.numeric(satis)
        break
      }
    }
  }
  
  # index = 5
  q2.2.1.3_sub <- function(){
    # Question 2.2.1.3 - sub - "Which transformation method do you want to apply? "
    # when q2.2.1.1 opt == 3 and q2.2.1.3 == 1
    repeat{
      cat('| Which transformation method do you want to apply?',
          "|   1. carry over + S curve",
          "|   2. carry over + power curve",
          "", sep="\n")
      opt <- readline("| Please enter an option: ")
      cat("\n")
      if(!opt %in% as.character(1:2)){
        message("| Only 1 and 2 is acceptable, dude!\n")
      }else{
        opt <- as.numeric(opt)
        break
      }
    }
  }
  
  # index = 7
  q2.2.1.3.1_pc <- function(){
    # Question 2.2.1.3.1 - pc "Please enter the parameters (carry-over + power curve) you want to try"
    repeat{
      co.r <- readline("| Please enter alternative carry-over rate: ")
      cat("\n")
      if(!all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      pc.r <- readline("| Please suggest alternative power curve rate: ")
      cat("\n")
      if(!all(strsplit(pc.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(co.r, pc.r, NA, NA)))
  }
  
  # index = 8
  q2.2.1.3.1_sc <- function(){
    # Question 2.2.1.3.1 - sc "Please enter the parameters (carry-over + S curve) you want to try"
    repeat{
      co.r <- readline("| Please enter alternative carry-over rate: ")
      cat("\n")
      if(!all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      sc.1 <- readline("| Please suggest alternative power curve rate: ")
      cat("\n")
      if(!all(strsplit(pc.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      sc.2 <- readline("| Please suggest alternative power curve rate: ")
      cat("\n")
      if(!all(strsplit(pc.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(co.r, NA, sc.1, sc.2)))
  }
  
  # index = 9
  q2.2.2.2 <- function(){
    # Question 2.2.2.2 "Do you confirm removing this variable?"
    repeat{
      conf.remv <- readline("| Do you confirm removing this variable (Y/N)? ")
      cat("\n")
      if(!toupper(conf.remv) %in% c("Y","N")){
        message("| Only Y/y and N/n is acceptable!\n")
      }else break
    }
  }
  
  # index = 10
  q2.2.3.2 <- function(){
    # Question 2.2.3.2 "Do you want to perform stepwise regression check?"
    repeat{
      stepaic <- readline("| Do you to do stepwise regression check? ")
      cat("\n")
      if(!toupper(stepaic) %in% c("Y","N")){
        message("| Only Y/y and N/n is acceptable!\n")
      }else{
        aic <- ifelse(stepaic == "Y", T, F)
        break
      } 
    }
    return(aic)
  }
  
  L_index <- LETTERS[index]
  switch(L_index,
         A = q2.2(),
         B = q2.2.1.2(df, coef, opt),
         C = q2.2.1.1(pred),
         D = q2.2.1.3(),
         E = q2.2.1.3_sub(),
         G = q2.2.1.3.1_pc(),
         H = q2.2.1.3.1_sc(),
         I = q2.2.2.2(),
         J = q2.2.3.2())
}

#--------#
# warn() #
#--------#
warn <- function(fit1, fit2, p.cons = 0.2) {
  # 1. warn user when there is some coefficient which changes **sign** between two models
  # 2. warn user when there is big gap between **p-value** of one pred in two models 
  
  
  if(!is.null(names(coef(fit2)))) {
    
    coef2 <- coef(fit2)
    name2 <- names(coef2)
    p.value2 <- coef(summary(fit2))[, 4]
    
    if(sum(p.value2 > p.cons) > 0){
      message("P-value of the following predictor", 
              if(sum(p.value2 > p.cons) > 1) "s are" else " is", 
              " larger than ", as.character(p.cons), sep = "")
      cat(paste(names(p.value2)[p.value2 > p.cons], collapse = ", "), "", sep = "\n")
      cat(paste(rep("-", 40), collapse = ""), "", sep = "\n")
    }
    
    
    if(!is.null(names(coef(fit1)))){
      coef1 <- coef(fit1)    
      name1 <- names(coef1)
      p.value1 <- coef(summary(fit1))[, 4]
      
      name <- intersect(name1, name2)
      
      sign1 <- sign(coef1[which(name1 %in% name)])
      sign1 <- sign1[order(names(sign1))]
      
      sign2 <- sign(coef2[which(name2 %in% name)])
      sign2 <- sign2[order(names(sign2))]
      
      if(any(sign1 * sign2 == -1)) {
        message("Sign of the following predictor's coefficient has changed!", sep = "")
        cat(paste(names(sign1)[sign1 * sign2 == -1], collapse = ", "), "", sep = "\n")
        cat(paste(rep("-", 40), collapse = ""), "", sep = "\n")
      }
    } 
    
  } 
  
} # end function warn()

#-----------#
# rebuild() #
#-----------#



#---------------#
# loop.output() #
#---------------#





#----------------#
# final.output() #
#----------------#
# ??? adjustment of parameter input ???

final.output <- function(resp, data, fit, prmt, contri, aic = FALSE) {
  # prmt is a dataframe including 8 columns: 
  #       variable, trans.meth, co.r, sc.1, sc.2, pc.r, oth, status
  # data = df1 # Transformed Data
  
  # Needed Packages: "MASS" for stepAIC(), "car" for vif(), "xlsx" for export
  # these 3 packages will be loaded outside this function

  # 1.Transformation (trans.meth, co.r, sc.1, sc.2, pc.r, oth)
  # directly output as a csv file
  
  if(aic) aic.ind <- "aic." else aic.ind <- NULL
  
  # put 2, 3, 4 into one data frame, and then output into a csv file
  
  # 2. Residuals
  resid <- cbind(data[, resp], fit$fitted.values, summary(fit)$residuals)
  rownames(resid) <- NULL
  colnames(resid) <- c(resp, "Prediction", "Residuals")
  
  # 3. Coefficients
  coef <- coef(summary(fit))     # Estimate  Std. Error    t value   Pr(>|t|)
  
  # 4. VIF
  if(length(coef(fit)) > 2) {
    vif <- vif(fit)
  } else {
    vif <- NA
  }
  
  # Merge model information into one data frame
  # Output csv
  pos <- which(colnames(prmt) == "status")
  prmt.alive <- prmt[which(prmt[[pos]] == "alive"), -pos]
  
  if(rownames(coef)[1] == "(Intercept)"){
    prmt.alive <- rbind(NA, prmt.alive)
    prmt.alive$pred.i <- c("(Intercept)", as.vector(prmt.alive$pred.i)[-1]) 
    # column "pred" is a factor, need to be changed to a vector for further modification
    vif <- rbind(NA, as.matrix(vif))
  }
  model <- as.data.frame(cbind(prmt.alive, coef, contri, vif), stringsAsFactors = F)
  rownames(model) <- NULL
  
  # --------------------------------------------------------------------------
  
  if(suppressMessages(require(xlsx))){
    wb <- createWorkbook()
    
    sht1 <- createSheet(wb, sheetName = "Model_Results")
    sht2 <- createSheet(wb, sheetName = "Parameters")
    sht3 <- createSheet(wb, sheetName = "Residuals")
    sht4 <- createSheet(wb, sheetName = "Transformed_Data")
    
    addDataFrame(model, row.names = FALSE, sht1)
    addDataFrame(prmt, row.names = FALSE, sht2)
    addDataFrame(resid, row.names = FALSE, sht3)
    addDataFrame(data, row.names = FALSE, sht4)
    
    saveWorkbook(wb, paste("model_result", ifelse(aic, "_aic", ""), 
                           ".xlsx", sep = ""))
    message('The model results are saved as "model_result',ifelse(aic, "_aic", ""),
            '.xlsx" under the default working directory.\n')
    
  }else{
    # 1. residuals.csv
    write.csv(resid, paste(aic.ind, "residuals.csv", sep = ""))
    message(paste('Value of response variable, prediction and residuals are exported to',
                  paste('the file "',paste(aic.ind, "residuals.csv", sep = ""),
                        '" under default working directory', sep=""), sep="\n"))
    cat(paste(rep("-+-",20),collapse=""))
    
    # 2. prmt.csv
    write.csv(prmt, paste(aic.ind, "prmt.csv", sep = ""))
    message(paste('Variable parameters history is exported to "',
                  paste(aic.ind, "prmt.csv", sep = ""),
                  '" under default working directory',sep=""))
    cat(paste(rep("-+-",20),collapse=""))
    
    # 3. model.results.csv
    write.csv(model, paste(aic.ind, "model.results.csv", sep = ""))
    message(paste('The modeling result is exported to "', 
                  paste(aic.ind, "model.results.csv", sep = ""),'".',sep=""))
    message('You could find the file under default working directory.')
    cat(paste(rep("-+-",20),collapse=""),"\n\n")
    
    # 4. transformed.data.csv
    # output transformed data to local file
    # write.csv(data[, c(resp, as.character(prmt.alive[, 1]))], "transformed.data.csv")
    write.csv(data, "transformed.data.csv")
    
    # print(summary(stepAIC(fit, direction = "both", trace = 0)))  
  }
} # end of function final.output()



