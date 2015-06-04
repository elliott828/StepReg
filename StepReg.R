rm(list = ls())
# STEP 0.1
# Download all related packages
all.pcg <- c("rJava", "xlsxjars", "xts", "xlsx", 
             "car", "MASS","plyr","lmtest","zoo", "plyr", "tidyr", "dplyr",
             "lubridate", "ggplot2", "Rcpp", "colorspace")

req.pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, 
         quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)
}

req.pkg(all.pcg)


# STEP 0.2
# Initialization
e <- new.env()

# STEP 0.3
# Functions Definition


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

#=====================================================================

#-------------------#
# trial()           #
#-------------------#

trial <- function(data, resp, fit = NULL, action = 1, pred) {            
  # Build the model				
  # based on NULL or fit1, add/remove a predictor/the intercept, output summary				
  
  # action:  1 means add a predictor
  #         -1 means delete a predictor
  
  if(is.null(fit)){ # if(exists("fit", mode = "list"))
    if(action == 1) {
      fit.new <- lm(as.formula(sprintf('%s ~ %s', resp, pred)), data = data, na.action = na.exclude)
    } else if(action == -1) {
      message("There's no existed model to let you delete ", pred, " from!", sep = "")
      cat("\n")
      fit.new <- fit
    }
    
  } else { # if(!is.null(fit))
    if(pred %in% names(coef(fit))) {
      if(action == -1) {
        fit.new <- update(fit, as.formula(sprintf('~. - %s', pred)), data = data)
      } else {
        message("The ", pred, " is already in the model!", sep = "")
        cat("\n")
        fit.new <- fit
      }
    } else {
      if(action == 1){
        fit.new <- update(fit, as.formula(sprintf('~. + %s', pred)), data = data)
      } else {
        message("The ", pred, " isn't in the model!", sep = "")
        cat("\n")
        fit.new <- fit
      }
    }
  }
  return(fit.new)
} # end of function trial()

#=====================================================================

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
    smr.fit <- fit.update(resp, x, fit.coef, pred, df)
    smr <- summary(smr.fit)
    simulation <-  t(t(as.matrix(x)) * as.vector(coef(smr.fit)[[pred]]))
    contri.d <- sum(simulation)/sum(smr.fit$fitted.values)
    smr.coef <- subset(smr$coefficients, rownames(smr$coefficients)==pred)[c(1,4)]
    smr.key <- c(smr.coef, smr$r.squared, smr$adj.r.squared, contri.d)
    names(smr.key) <- c("Coefficient", "P.value", "R.squared", "Adj.R.squared", "Contribution")
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
      message("| ", e$pred, " cannot get into the model!\n")
      message("| Please switch another variable!\n")
    } else {
      curve.prmt <- if(len == 2) "pc.r" else c("sc.1","sc.2")
      colnames(prmt.all) <- c("co.r", curve.prmt, "coef",
                              "p-value", "r.squared", "adj.r.squared", "contribution")
      rownames(prmt.all) <- NULL
      prmt.all <- prmt.all[order(prmt.all[["adj.r.squared"]], decreasing = T),]
      
      # export all parameters and related model statistics to local working directory
      write.csv(prmt.all, paste("prmt", nam, pred, "csv",sep = "."), row.names = FALSE)
      message(paste("\nThe parameter reference: 'prmt", nam, 
                    pred, "csv' is exported!",sep = "."))
      
      # capture the best group of parameters
      # get the best adj. r squared first then allocate the position
      posi <- which.max(as.numeric(prmt.all[["adj.r.squared"]]))
      
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
            paste(" - The coefficient after transformation is ", 
                  if (round(best.stats[4],4)==0){
                    format(best.stats[4], scientific = T)
                  }else{
                    format(round(best.stats[4],4),nsmall = 4)
                  }, sep = ""),
            sep = "\n")
        curve.prmt <- c(best.stats[1], NA, best.stats[2], best.stats[3],
                        best.stats[ncol(prmt.all)-1])
      }else if(as.character(type)=="2"){
        cat(paste(" - The recommended power rate is ", best.stats[2], sep = ""),
            paste(" - The coefficient after transformation is ", 
                  if (round(best.stats[3],4)==0){
                    format(best.stats[3], scientific = T)
                  }else{
                    format(round(best.stats[3],4),nsmall = 4)
                  }, sep = ""),
            sep = "\n")
        curve.prmt <- c(best.stats[1], best.stats[2], NA, NA,
                        best.stats[ncol(prmt.all)-1])
      }
      
      cat(paste(" ", paste(rep("-",40), collapse = ""),sep = ""),"\n")
      
      if(is.na(best.stats[["p-value"]])){
        message("| ", e$pred, " is not advised to be added to the model!\n")
      }else if (best.stats[["p-value"]] > 0.2){
        message("| Please be aware that the p-value of predictor coefficient is larger than 0.2!")
        message("| The estimate of coefficient is not significant!\n")
      }
      
      names(curve.prmt) <- c("co.r","pc.r","sc.1","sc.2","Adj.r2")
    }
    
    return(curve.prmt)
  } # end of function testall()
  
  #-------------------------------------#
  # PART III - Application of functions #
  #-------------------------------------#
  
  if(as.character(type) == "1"){
    # carryover + s-curve only
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 1, df = df1)
  }else if(as.character(type) == "2"){
    # carryover + power curve only
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 2, df = df1)
  }else if(as.character(type) == "3"){
    # compary carryover + s-curve and carryover + power curve 
    prmt.cs <- testall(resp, x0, pred, fit.coef = fit, type = 1, df = df1)
    prmt.cp <- testall(resp, x0, pred, fit.coef = fit, type = 2, df = df1)
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
} # end of funciton recom()

#=====================================================================

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
  co.r <- tm.prmt[[1]]
  pc.r <- tm.prmt[[2]]
  sc.1 <- tm.prmt[[3]]
  sc.2 <- tm.prmt[[4]]
  
  if(is.na(co.r)){
    df[[pred]] <- data[[pred]]
  }else if(is.na(pc.r)){
    df[[pred]] <- cs(data[[pred]], co.r, sc.1, sc.2)
  }else{
    df[[pred]] <- cp(data[[pred]], co.r, pc.r)
  }
  return(df)
}

#=====================================================================

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
    return(opt)
  }
  
  # index = 2
  q2.2.1.2 <- function(df, coef, opt){
    # Question 2.2.1 / 2.2.2 "Indicate the predictor's name to be added to the model"
    # also applicable for question at step 2.2.2: 
    #   "Indicate the predictor's name to be removed from the model"
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
    return(pred.name)
  }
  
  # index = 3
  q2.2.1.1 <- function(pred){
    # Question 2.2.1.1 "Transformation method"
    repeat{
      cat(
        paste('| Which kind of transformation does the variable "', pred, '" need to be adapted?', sep=""),
              "|  1. carry over + S curve",
              "|  2. carry over + power curve",
              "|  3. auto-selection between 1 and 2",
              "",
              "|  0. no transformation", "", sep="\n")
      opt <- readline("| Please enter an option: ")
      cat("\n")
      if(!opt %in% as.character(0:3)){
        message("| Only the number between 0 and 3 is acceptable, dude!\n")
      }else{
        opt <- as.numeric(opt)
        break
      }
    }
    return(opt)
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
          '', sep = '\n')
      satis <- readline("| Please enter an option: ")
      cat("\n")
      if(!satis %in% as.character(1:4)){
        message("| Only a number between 1 and 4 is acceptable!\n")
      }else{
        satis <- as.numeric(satis)
        break
      }
    }
    return(satis)
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
    return(opt)
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
      sc.1 <- readline("| Please suggest alternative the 1st S curve rate: ")
      cat("\n")
      if(!all(strsplit(sc.1, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      sc.2 <- readline("| Please suggest alternative the 2nd S curve rate: ")
      cat("\n")
      if(!all(strsplit(sc.2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(co.r, NA, sc.1, sc.2)))
  }
  
  # index = 9
  q2.2.2.2 <- function(){
    # Question 2.2.2.2 "Do you confirm removing this variable?"
    repeat{
      conf.remv <- readline(paste("| Do you confirm removing", e$pred, "(Y/N)? ", sep = " "))
      cat("\n")
      if(!toupper(conf.remv) %in% c("Y","N")){
        message("| Only Y/y and N/n is acceptable!\n")
      }else break
    }
    return(toupper(conf.remv))
  }
  
  # index = 10
  q2.2.3.2 <- function(){
    # Question 2.2.3.2 "Do you want to perform stepwise regression check?"
    repeat{
      stepaic <- readline("| Do you want to do stepwise regression check (Y/N)? ")
      cat("\n")
      if(!toupper(stepaic) %in% c("Y","N")){
        message("| Only Y/y and N/n is acceptable!\n")
      }else{
        aic <- ifelse(toupper(stepaic) == "Y", T, F)
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

#=====================================================================

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

#=====================================================================

#-----------#
# rebuild() #
#-----------#

rebuild <- function(resp, data, st.row) {
  
  # resp and data is already in the global environment
  # data is raw without modification
  # source(modif)
  
  # need one step to confirm the model, then 
  # fit <- fit.temp; df <- df.temp
  
  message("| You're expected to input the Parameters file. ")
  message("| Make sure the full data file and Parameters file are RELATED! ")
  message("| Make sure the data structure is the same as StepReg TOOL OUTPUTS! \n")
  
  repeat{
    prmt.file <- readline('| Please enter the name of "prmt" file: ')
    cat("\n")
    endstr <- substr(prmt.file, nchar(prmt.file)-3, nchar(prmt.file))
    if (!endstr %in% c(".csv", "xlsx", ".xls")){
      message('| Only ".csv", "*.xlsx" or "*.xls" file is expected!\n')
    }else if(file.exists(prmt.file)){
      if(endstr == ".csv"){
        e$prmt <- read.csv(prmt.file, stringsAsFactors = FALSE)
        # names(e$prmt) <- c("variable", "co.r", "pc.r", "sc.1", "sc.2", "status")
      } else {
        sht.names <- names(getSheets(loadWorkbook(prmt.file)))
        repeat{
          sht.name2 <- readline("| Please enter the name of Parameters worksheet: ")
          cat("\n")
          if (!sht.name2 %in% sht.names){
            message('| The worksheet "', sht.name2, '" is not found!\n')
          }else break
        }
        e$prmt <- read.xlsx(prmt.file, sheetName = sht.name2, stringsAsFactors = F)
        # names(e$prmt) <- c("variable", "co.r", "pc.r", "sc.1", "sc.2", "status")
      }
      break
    }else if(!file.exists(prmt.file)){
      message('| "', prmt.file, '" does not exist in the work directory!')
      message('| Please re-check it in the file explorer!\n')
    }
  }
  
  e$prmt$status <- factor(e$prmt$status, levels = c("alive", "dead"))
  prmt.alive <- e$prmt[which(e$prmt$status == "alive"),]
  df0 <- data
  
  for(i in 1:nrow(prmt.alive)){
    pred <- as.character(prmt.alive[[1]][i])
    co.r <- prmt.alive[[2]][i]
    pc.r <- prmt.alive[[3]][i]
    sc.1 <- prmt.alive[[4]][i]
    sc.2 <- prmt.alive[[5]][i]
    
    df0 <- modif(pred, df0, c(co.r, pc.r, sc.1, sc.2))
  }
  e$df0 <- df0
  e$df1 <- df0[st.row:dim(df0)[1], ]
  e$fit1 <- lm(as.formula(paste(resp, paste(prmt.alive[[1]], collapse = " + "), sep = " ~ ")), 
               data = e$df1, na.action = na.exclude)
} # end of function rebuild()

#=====================================================================

#-------------------#
# arrange_ggplot2() #
#-------------------#

## Function for arranging ggplots. use png(); arrange(p1, p2, ncol=1); dev.off() to save.
suppressMessages(require(grid))
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row    
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
} # end of function arrange_ggplot2()

#=====================================================================

#---------------#
# loop.output() #
#---------------#

loop.output <- function(resp, data, fit, pred, tvar) {                  
  # Input: resp(name), data(modified, st.row:dim(df0)[1]), fit, tvar(name)  				
  # Output: only on memory and screen, nothing to files
  
  # Consists of 4 parts:  Part. I  Summary of Fit & MAPE
  #                       Part. II  Plots
  #                       Part. III  DW-test
  #                       Part. IV  Contribution Rates
  
  # Needed Packages: "zoo" (for library(lmtest)), "lmtest" (for dwtest())
  
  cat(paste(rep("-+-",20),collapse=""),"\n\n")
  readline('You can find the output of the model below.\nIn next 5 steps, please press <Enter> to continue...')
  cat("\n")
  
  #---------------------------------------------------------------
  # Part. I  Summary of Fit
  #---------------------------------------------------------------
  
  readline("Part. I  Summary of Fit")
  print(summary(fit))	
  cat("\n")
  
  #---------------------------------------------------------------
  # Part. II  MAPE
  #---------------------------------------------------------------
  
  readline("Part. II  MAPE")
  resp.temp <- data[[resp]]
  resp.temp[which(resp.temp == 0)] <- mean(data[[resp]]) 
  mape <- mean(abs(fit$residuals/resp.temp))
  e$mape <- mape
  cat("MAPE of the model is ", round(mape, 4),"\n", sep = "")
  cat("\n")
  
  
  #---------------------------------------------------------------
  # Part. III  Plots
  #---------------------------------------------------------------
  
  na <- readline("Part. III  Plots")
  message("Please look at the Plots area!\n")
  
  x_axis <- data[[tvar]]
  df_to_plot <- data.frame(x_axis, res = summary(fit)$residuals)
  if(!class(df_to_plot$x_axis)[1] %in% c("POSIXct", "POSIXt", "numeric", "integer")){
    df_to_plot$x_axis <- seq_along(1:nrow(df_to_plot))
  }
  # 1. scatterplot of residuals
  scat_res <- df_to_plot                                %>%
    ggplot(aes(x = x_axis, y = res))                     +
    geom_point(colour = "orange", size = 3)              +
    theme(legend.position = "none")                      +
    geom_hline(yintercept = 0, colour = "red")           +
    ggtitle("Plot 1. Scatterplot of Model Residuals")    +
    xlab("Observation Index")                            +
    ylab("Residuals")
  
  # 2. histogram of residuals
  max_res <- max(df_to_plot$res)
  min_res <- min(df_to_plot$res)
  mean_res <- mean(df_to_plot$res)
  binrange <- (max_res - min_res)/40
  hist_res <- df_to_plot                                           %>%
    ggplot(aes(x = res))                                            +
    geom_histogram(stats = "identity", fill = "orange", 
                   colour = "white", binwidth = binrange)           +
    geom_vline(xintercept = mean_res, colour = "red")               +
    ggtitle("Plot 2. Histogram of Model Residuals")                 +
    xlab("Residual Interval")                                       +
    ylab("Residuals")
  
  # 3. latest predictor
  if(!is.null(pred)){
    modif_pred <- data.frame(x_axis, predictor = data[[pred]])
    pred_line <- ggplot(modif_pred, aes(x_axis))                    +
      geom_line(aes(y = predictor), colour = "darkgrey", size = 1)  +
      ggtitle(paste("Plot 3. Modified Value of ", pred, sep = " ")) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank())
  }
  
  # 4. fit vs. actual
  fit.resp <- fit$fitted.values
  act.resp <- data[,resp]
  fit_to_plot <- data.frame(x_axis, fit.resp, act.resp)
  if(!class(fit_to_plot$x_axis)[1] %in% c("POSIXct", "POSIXt", "numeric", "integer")){
    fit_to_plot$x_axis <- seq_along(1:nrow(fit_to_plot))
  }
  
  fit_vs_act <- fit_to_plot                                       %>%
    ggplot(aes(x_axis))                                           +
    geom_line(aes(y = act.resp), colour = "darkgrey", size = 1)   +
    geom_line(aes(y = fit.resp), colour = "red", size = 1)        +
    ggtitle("Plot 4. Modeled vs. Actual Variable")                +
    ylab("Count / Volume")                                        +
    theme(legend.position = "right", 
          axis.title.x = element_blank())
  
  if(is.null(pred)){
    arrange_ggplot2(scat_res, hist_res, fit_vs_act, ncol = 1)
  } else{
    arrange_ggplot2(scat_res, hist_res, ncol = 1)
    arrange_ggplot2(pred_line, fit_vs_act, ncol = 1)
  }
  
  #---------------------------------------------------------------
  # Part. IV  DW-test
  #---------------------------------------------------------------
  
  na <- readline("Part. IV  DW-test")
  print(dwtest(fit))
  
  #---------------------------------------------------------------
  # Part. V  Contribution Rates
  #---------------------------------------------------------------    
  
  na <- readline("Part. V  Contribution Rates")
  cat("\n")
  
  # contri.d: draft    
  simulation <- cbind(coef(fit)[1], t(t(as.matrix(data[, names(coef(fit))[-1]])) * as.vector(coef(fit)[-1])))
  colnames(simulation) <- names(coef(fit))
  contri.d <- colSums(simulation)/sum(fit$fitted.values)
  
  # contri.p: positive
  contri.p <- contri.d[which(contri.d >= 0)]
  contri.p <- as.matrix(contri.p[order(contri.p, decreasing = T)])
  
  contri.top10 <- as.matrix(head(contri.p[which(rownames(contri.p) != "(Intercept)")], 10))
  colnames(contri.top10) <- "Top POSITIVE Contributors"
  rn.p <- rownames(contri.p)[which(rownames(contri.p) != "(Intercept)")]
  rownames(contri.top10) <- if (length(rn.p)>10) rn.p[1:10] else rn.p
  
  
  # contri.n: negative
  contri.n <- contri.d[which(contri.d < 0)]
  contri.n <- as.matrix(contri.n[order(contri.n, decreasing = F)])
  
  contri.bot5 <- as.matrix(head(contri.n[which(rownames(contri.n) != "(Intercept)")], 5))
  colnames(contri.bot5) <- "Top NEGATIVE Contributors"
  rn.n <- rownames(contri.n)[which(rownames(contri.n) != "(Intercept)")]
  rownames(contri.bot5) <- if (length(rn.n)>5) rn.n[1:5] else rn.n
  
  
  # message('Voila the top contributor(s) to the variable "', resp, '":', sep ="")
  if(nrow(contri.top10) > 0){
    print(contri.top10)
    cat("\n")
  }
  if(nrow(contri.bot5) > 0){
    print(contri.bot5)
    cat("\n")
  }
  
  
  #---------------------------------------------------------------
  # Part. VI  VIF (Updated on Friday 12/19/2014)
  #---------------------------------------------------------------  
  
  if(length(coef(fit)) > 2) {
    vif <- as.matrix(vif(fit))
    colnames(vif) <- "VIF"
    na <- readline("Part. VI  VIF")
    cat("\n")
    print(vif)
    cat("\n")
    
    if(any(vif >= 10)){
      message(paste("VIF of", paste(rownames(vif)[which(vif >= 10)], collapse = ", "), 
                    if(length(which(vif >= 10)) > 1) "are" else "is", 
                    "bigger than 10! ", sep = " "))
      cat("\n")
    }
  }
  
  cat(paste(rep("-+-",20),collapse=""),"\n\n")
  
} # end of function loop.output()

#=====================================================================

#----------------#
# final.output() #
#----------------#

final.output <- function(resp, data, tvar, fit, prmt, aic = FALSE) {
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
  resid <- cbind(data[, c(tvar, resp)], fit$fitted.values, summary(fit)$residuals)
  rownames(resid) <- NULL
  colnames(resid) <- c(tvar, resp, "Prediction", "Residuals")
  
  # 3. Coefficients
  coef <- coef(summary(fit))     # Estimate  Std. Error    t value   Pr(>|t|)
  
  # 4. VIF
  if(length(coef(fit)) > 2) {
    vif <- vif(fit)
  } else {
    vif <- NA
  }
  
  # 5. Contribution    
  simulation <- cbind(coef(fit)[1], t(t(as.matrix(data[, names(coef(fit))[-1]])) * as.vector(coef(fit)[-1])))
  colnames(simulation) <- names(coef(fit))
  contri.d <- colSums(simulation)/sum(fit$fitted.values)
  
  # Merge model information into one data frame
  # Output csv
  pos <- which(colnames(prmt) == "status")
  prmt.alive <- prmt[which(prmt[[pos]] == "alive"), -pos]
  
  if(rownames(coef)[1] == "(Intercept)"){
    prmt.alive <- rbind(NA, prmt.alive)
    prmt.alive[1, 1] <- "(Intercept)"
    vif <- rbind(NA, as.matrix(vif))
  }
  model <- data.frame(prmt.alive[, 1], coef[, 1], prmt.alive[, -1], coef[, -1], 
                      vif = vif, contribution = contri.d, stringsAsFactors = F)
  names(model) <- c("variable", "coefficient", "co.r", "pc.r",	"sc.1", "sc.2", 
                    "std.error", "t.value", "p.value", "vif", "contribution")
  
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
    message('| The model results are saved as "model_result',ifelse(aic, "_aic", ""),
            '.xlsx".')
    
    # prmt.csv: for convenience of importing model next time
    write.csv(prmt, paste(aic.ind, "prmt.csv", sep = ""), row.names = FALSE)
    message(paste('| The variable parameters history is exported to "',
                  paste(aic.ind, "prmt.csv", sep = ""),
                  '". ',sep=""))
    cat(paste(rep("-+-",20),collapse=""),"\n\n")
    
  }else{
    # 1. residuals.csv
    write.csv(resid, paste(aic.ind, "residuals.csv", sep = ""), row.names = FALSE)
    message(paste('| Value of response variable, prediction and residuals are exported to',
                  paste('the file "',paste(aic.ind, "residuals.csv", sep = ""),
                        '". ', sep=""), sep="\n"))
    cat(paste(rep("-+-",20),collapse=""))
    
    # 2. prmt.csv
    write.csv(prmt, paste(aic.ind, "prmt.csv", sep = ""), row.names = FALSE)
    message(paste('Variable parameters history is exported to "',
                  paste(aic.ind, "prmt.csv", sep = ""),
                  '"',sep=""))
    cat(paste(rep("-+-",20),collapse=""))
    
    # 3. model.results.csv
    write.csv(model, paste(aic.ind, "model.results.csv", sep = ""), row.names = FALSE)
    message(paste('The modeling result is exported to "', 
                  paste(aic.ind, "model.results.csv", sep = ""),'".',sep=""))
    cat(paste(rep("-+-",20),collapse=""),"\n\n")
    
    # 4. transformed.data.csv
    # output transformed data to local file
    # write.csv(data[, c(resp, as.character(prmt.alive[, 1]))], "transformed.data.csv")
    write.csv(data, "transformed.data.csv")
    
    # print(summary(stepAIC(fit, direction = "both", trace = 0)))  
  }
} # end of function final.output()

#=====================================================================

#-------------#
# check.cor() #
#-------------#
check.cor <- function(df){
  cor.df <- cor(df)
  col.no <- ncol(df)
  mat <- as.data.frame(matrix(ncol = 3))
  colnames(mat) <- c("Var1","Var2","Correlation")
  for (i in 2:col.no){
    for (j in 1:(i-1)){
      if (is.na(mat[1,1])){
        mat[1,1] <- rownames(cor.df)[i]
        mat[1,2] <- colnames(cor.df)[j]
        mat[1,3] <- cor.df[i,j]
      }else{
        row.no <- nrow(mat)
        mat[row.no+1, 1] <- rownames(cor.df)[i]
        mat[row.no+1,2] <- colnames(cor.df)[j]
        mat[row.no+1,3] <- cor.df[i,j]
      }
    }
  }
  
  mat <- mat[order(abs(mat[,3]), decreasing =T),]
  rownames(mat) <- NULL
  n.cor <- if(nrow(mat)>20) 20 else nrow(mat)
  message("| The top ", n.cor, " (absolute) correlation rates are listed below: \n")
  print(mat[1:n.cor,])
  write.csv(mat, "correlation_pairs.csv", row.names = F)
  message('\n| The correlation pairs and rates are exported to "correlation_pairs.csv".\n')
}

#=====================================================================

# Structure Function: StepReg()
StepReg <- function(){
  
  # STEP 0.5 
  # save original work directory for final recover
  e$wd_recover <- getwd()
  
  # STEP 0.6
  # set working directory
  if(!file.exists("StepReg_WD"))dir.create("StepReg_WD")
  setwd(paste(e$wd_recover, "StepReg_WD", sep = "/"))
  
  e$fit1 <- NULL
  e$prmt <- data.frame(variable = character(),
                       co.r = numeric(),
                       pc.r = numeric(),
                       sc.1 = numeric(),
                       sc.2 = numeric(),
                       status = character(),  #indicate the availability of variable for the model
                       stringsAsFactors = F)
  # e$prmt[[6]] <- factor(prmt[[6]], levels = c("alive", "dead"))
  
  # STEP 1.1
  # Welcome!
  cat("\n")
  message("| Welcome to StepReg system!")
  message("| Please follow instructions to finish the regression.\n")
  message("| Be aware that in StepReg system the working directory is set as: ")
  message("| ", getwd(), ".\n")
  message("| It will be recovered to your default directory after the system is closed. \n")

  # STEP 1.2
  # Read Data
  repeat{
    data.name <- readline("| Please enter the name of raw data file: ")
    endstr <- substr(data.name, nchar(data.name) - 3, nchar(data.name))
    cat("\n")
    if (!endstr %in% c(".csv", "xlsx", ".xls")){
      message('| Only "*.csv", "*.xlsx" or "*.xls" file is expected!\n')
    }else if(!file.exists(data.name)){
      message('| "', data.name, '" does not exist in StepReg system work directory!')
      message('| Please re-check it in the file explorer!\n')
    }else {
      # data can be input correctly
      if(endstr == ".csv"){
        e$data <- read.csv(data.name, stringsAsFactors = F)
      }else{ # if(endstr %in% c("xlsx", ".xls"))
        sht.names <- names(getSheets(loadWorkbook(data.name)))
          repeat{
            sht.name <- readline("| Please enter the name of the worksheet: ")
            cat("\n")
            if (!sht.name %in% sht.names){
              message('| The worksheet "', sht.name, '" is not found!\n')
            }else{
              break
            }
          }
        e$data <- read.xlsx(data.name, sheetName = sht.name, stringsAsFactors = F)
      }
  
      message('| "',data.name, '" is loaded.') 
      message('| There are ', dim(e$data)[1], ' observations and ', 
              dim(e$data)[2], ' variables in the raw dataset.\n')
      
      # Check NA in the input data frame
      check.na <- function(x)sum(is.na(x))
      num.na <- sapply(e$data, check.na)
      if(sum(num.na)!=0){
        message(paste('| There ', if(sum(num.na!=0)==1)'is ' else 'are ',
                      sum(num.na!=0),
                      if(sum(num.na!=0)==1)' variable' else ' variables',
                      ' with NAs!', sep = ""))
        cat("\n")
        message(paste('| The', if(sum(num.na!=0)==1)' variable is: ' else ' variables are: ', sep = ""))
        message("| ", paste(names(which(num.na != 0)), sep = ", "), ". \n")
        message("| Please double check your raw data!\n")
      } else {
        break
      }
    }
  }
    
  # STEP 1.3 
  # choose the response variable
  repeat{
    e$resp <- readline("| Please enter the name of response variable: ")
    cat("\n")
    if (e$resp %in% names(e$data)){
      break
    }else{
      message(paste('| The variable "', e$resp, 
                    '" does not exist in the database!', sep=""))
      cat("\n")
    }    
  }
  
  # STEP 1.4
  # choose the timeline variable and its format
  repeat{
    e$tvar <- readline("| Please enter the name of time variable: ")
    cat("\n")
    if (e$tvar %in% names(e$data)){
      break
    }else{
      message(paste('| The variable "', e$tvar, 
                    '" does not exist in the database!', sep=""))
      cat("\n")
    }    
  }
  
  if(class(e$data[[e$tvar]]) != "Date"){
    repeat{
      cat("| Which kind of date format is it in the raw data? \n")
      cat("|   a. m/d/y\n|   b. d/m/y\n|   c. y/m/d\n")
      t_f <- tolower(readline("\n| Please choose a format: "))
      cat("\n")
      if(!t_f %in% c("a","b","c")){
        message('| Only "a", "b" or "c" is acceptable!\n')
      }else break
    }
    
    e$data[[e$tvar]] <- switch(t_f, 
                               a = mdy(e$data[[e$tvar]]), 
                               b = dmy(e$data[[e$tvar]]), 
                               c = ymd(e$data[[e$tvar]]))
  } else {
    e$data[[e$tvar]] <- ymd(e$data[[e$tvar]])
  }
  e$df0 <- e$data
  
  # STEP 1.5
  # choose start row: e$st.row
  repeat{
    st.row <- readline("| Please enter the row number you want to start modeling: ")
    cat("\n")
    if(all(strsplit(st.row, split = "")[[1]] %in% as.character(0:9))) {
      e$st.row <- as.numeric(st.row)
      break
    }else{
      message("Please do enter an integer!\n")
    }    
  }
  e$df1 <- e$df0[e$st.row:nrow(e$df0), ]
  
  # Check the correlations
  check.cor(e$data[e$st.row:nrow(e$data),-which(colnames(e$data) == e$tvar)])
  
  # STEP 1.6 
  # rebuild the model if any model existed already
  repeat{
    rec <- readline("| Do you want to continue testing an existed model (Y/N)? ")
    cat("\n")
    if (!toupper(rec) %in% c("Y","N")){
      message("| Please only enter Y or N!\n")
    } else if (toupper(rec) == "Y"){
        rebuild(e$resp, e$data, e$st.row)
        loop.output(e$resp, e$df1, e$fit1, pred = NULL, e$tvar)
        warn(fit1 = NULL, fit2 = e$fit1, p.cons = 0.2)
        break
    } else break
  }
  
  # big loop start: What's the next move? 
  repeat{
    a_nm <- questions(1) # STEP 2.2 answer for the next move
    if(a_nm == 1){ # opt1. Add a new predictor
      repeat{
        # STEP 2.2.1 predictor name to add
        e$pred <- questions(2, df = e$df1, coef = e$fit1$coef, opt = 1) 
        repeat{
          # STEP 2.2.1.1 transformation method
          e$tm <- questions(3, pred = e$pred) 
          if(e$tm %in% 1:3){ # 0-no trans; 1-s curve; 2-power curve; 3-1 or 2
            e$tm.prmt <- recom(e$pred, e$resp, e$df0, e$tm, e$fit1, e$st.row)
            e$df.temp0 <- modif(e$pred, e$df0, e$tm.prmt)
          } else if(e$tm == 0){
            e$tm.prmt <- rep(NA, 4)
            e$df.temp0 <- e$df0
          }
          e$df.temp1 <- e$df.temp0[e$st.row:nrow(e$df.temp0), ]
          e$fit.temp <- trial(e$df.temp1, e$resp, e$fit1, action = 1, e$pred)
            
          repeat{
            loop.output(e$resp, e$df.temp1, e$fit.temp, e$pred, e$tvar)
            warn(fit1 = e$fit1, fit2 = e$fit.temp, p.cons = 0.2)
            
            # STEP 2.2.1.3 answer for Satisfactory Question
            e$a_stsf <- questions(4) 
            if(e$a_stsf == 1){ # opt1 change transformation prmt
              if(e$tm == 3){
                e$tm.prmt <- if(questions(5) == 1) questions(8) else questions(7)
              } else if(e$tm == 1){
                e$tm.prmt <- questions(8)
              } else if(e$tm == 2){
                e$tm.prmt <- questions(7)
              }
              
              e$df.temp0 <- modif(e$pred, e$df0, e$tm.prmt)
              e$df.temp1 <- e$df.temp0[e$st.row:nrow(e$df.temp0), ]
              e$fit.temp <- trial(e$df.temp1, e$resp, e$fit1, action = 1, e$pred)             
            } else break # if(e$a_stsf %in% 2:4){ 
          }
          # if(e$a_stsf == "2"){ # change transformation method
          if(e$a_stsf %in% 3:4){
            break
          }   
        }
        # if(e$a_stsf == 3){ # change a predictor
        if(e$a_stsf == 4){ # satisfied
          e$df0 <- e$df.temp0
          e$df1 <- e$df.temp1
          e$fit1 <- e$fit.temp
          prmt.line <- data.frame(variable = e$pred, 
                                  co.r = e$tm.prmt[1], pc.r = e$tm.prmt[2], 
                                  sc.1 = e$tm.prmt[3], sc.2 = e$tm.prmt[4], 
                                  status = "alive", stringsAsFactors = F)
          # names(e$prmt) <- c("variable", "co.r", "pc.r", "sc.1", "sc.2", "status")
          e$prmt <- rbind(e$prmt, prmt.line)
          break
        }
      }
    } else if(a_nm == 2){ # 2. Remove a predictor
      e$pred <- questions(2, df = e$df1, coef = e$fit1$coef, opt = 2) # predictor name to remove
      e$fit.temp <- trial(e$df1, e$resp, e$fit1, -1, e$pred)
      loop.output(e$resp, e$df1, e$fit.temp, e$pred, e$tvar)
      warn(fit1 = e$fit1, fit2 = e$fit.temp, p.cons = 0.2)
      e$a_conf <- questions(9) # STEP 2.2.2.2 confirm the removal or not
      if(e$a_conf == "Y"){
        e$fit1 <- e$fit.temp
        e$df0[[e$pred]] <- e$data[[e$pred]]
        e$df1 <- e$df0[e$st.row:nrow(e$df0), ]
        e$prmt[which((e$prmt[[1]] == e$pred) & (e$prmt[[6]] == "alive")), 6] <- "dead"
      } # else if(e$a_conf == "N") do nothing
    } else if(a_nm == 3){ # 3. Stop modeling
      final.output(e$resp, e$df1, e$tvar, e$fit1, e$prmt, questions(10))
      break
    }
  }

  message("| Thank you for using StepReg tool! Goodbye! \n")
  
  # reset wd to the default after StepReg finished
  setwd(e$wd_recover)
  
} # end of function StepReg()

# start StepReg system
StepReg()

