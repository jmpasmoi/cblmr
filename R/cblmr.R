#' Choice of Best Linear Model
#'
#'
#' This function allows you to choose the best model from among different "best" models.
#' Evaluating based on predictions and goodness of fit.
#'
#' @param dataframe only numerical values are required (explanatory and response variables)
#' @param response is the variable of interest in the experiment
#' @return a tab grouping predictions and goodness of fit per model
#' @author Jean Marie Cimula
#' @details
#' Evaluating based on predictions (PRESS : Prediction Sum of Squares) and
#' goodness of fit (AIC, BIC, R Squared, Adj. Squared).
#'@export

blm_choice <- function (dataframe, response){

  input_unit <- dataframe

  response  <- response

  #attach(input_unit)

  #(1) Number of colums of the data frame
  getLen <- dim(input_unit)[2]

  for (i in 1:getLen){

    #Removing the response variable in the temporary data frame
    if (names(input_unit)[i] == response){

      expLayer <- input_unit [,-i]

      break;

    }#if-condition

  }#Loop for

  #(2)Number of colums of the temporary data frame of explanatory variables
  getLenExpVar <- dim(expLayer)[2]

  #(3)Initializing a Matrix which will contain all combinations of predictor model
  ExpVarMatrix <- matrix( ncol = getLenExpVar)

  #Creating combination
  for (i in 1:getLenExpVar){
    #Combination function
    comb      <- t(combn(names(expLayer),i))
    numbRow   <- nrow(comb)
    numbCol   <- length(names(expLayer))
    numbRowNA <- numbRow
    numbColNA <- numbCol-ncol(comb)
    naMatr    <- matrix(rep(NA, numbRowNA*numbColNA), nrow = numbRowNA, ncol = numbColNA)
    result    <- cbind(comb, naMatr)
    ExpVarMatrix <- rbind(ExpVarMatrix, result)

  }#Loop for
  #Removing all NA
  ExpVarMatrix <- ExpVarMatrix[-1,]

  #Final result of combination between response and explanatory variables
  #Setting an empty data frame
  make_your_choice <- data.frame()

  for (i in 1:nrow(ExpVarMatrix)){

    getVal    <- na.omit (ExpVarMatrix[i, ])

    mdRegComb <- paste (response, " ~ ", paste (getVal, collapse = " + "), sep = "")

    #print(mdRegComb)
    mdLM <- lm(as.formula(mdRegComb),data=input_unit)

    #Evaluating based on goodness a fit
    #Diagnostic parameters
    SMry     <- summary(mdLM)
    RSqrt    <- SMry[8]   #R-Squared
    AdjRSqrt <- SMry[9] #adj R-Squared
    AIC      <- AIC(mdLM)#AIC
    BIC      <- BIC(mdLM)#BIC

    #Evaluating based on prediction
    PL  <- DAAG::press(mdLM)

    #Ridge Regression
    nbResp <- 0
    nbExp  <- 0
    Ridge  <- 0
    Lasso  <- 0
    ElasticNet <- 0
    Cp <- 0

    if (stringr::str_detect(mdRegComb,'[+]') == TRUE){

      #Checking the tilde in the model
      delim   <- unlist(gregexpr(pattern ='[~]',mdRegComb)) - 1

      #Retrieving the part before the tilde with the response variable
      stPart  <- substr(mdRegComb,1,delim)

      #Trim the result retrieved
      stPart  <- stringr::str_trim(stPart)

      #Retrieving the second part with the exploratory variables
      sndPart <- substr(mdRegComb,delim + 2, stringr::str_length(mdRegComb))

      nbResp  <- getNumberResponse (input_unit,stPart) #Column of Response variable
      nbExp   <- getNumberExploratory (input_unit,sndPart)#Columns of Exploratory variables
      Ridge   <- getRidgeValue(input_unit,nbResp,nbExp)
      Lasso   <- getLassoValue(input_unit,nbResp,nbExp)
      ElasticNet <- getElasticNetValue(input_unit,nbResp,nbExp)
      Cp         <- getMallowCp(input_unit,nbResp,nbExp)

      }

    #Assembling diagnostic parameters per model predictor in Matrix of all combinations
    dFrame <- data.frame(modelReg = mdRegComb,RSquared = RSqrt,AdjustedRSquared = AdjRSqrt,AIC = AIC,BIC = BIC,PRESS = PL,Ridge = Ridge,Lasso = Lasso, ElasticNet = ElasticNet,Cp=Cp)#, nbResp = nbResp, nbExp = nbExp)#, Accuracy = RidgeRegression)

    #Loading data frame
    make_your_choice <- rbind(make_your_choice,dFrame)
  }

    make_your_choice <- as.data.frame(make_your_choice)

     #getBestRModel(make_your_choice)  #Call the function getBestRModel
     #return (View(make_your_choice))#Return
     knitr::kable(make_your_choice)

}#End function

getNumberResponse <- function (dataframe, model){

  #Retrieving the number of the column of the response variable

  colData <- 0

  for (j in 1:dim(dataframe)[2]){
    #Take one by one chosen exploratory variables
    if (names(dataframe)[j] == stringr::str_trim(model)){

         colData <- j
         break;
    }

    colData <- colData

  }#End-for

  respVarColumn <- colData #Response variable number in the dataframe

  #print(respVarColumn)
  return(respVarColumn)

  }#End function

getNumberExploratory <- function (dataframe, model){

  ChPlusLength <- length(unlist(gregexpr(pattern = "[+]",model))) + 1 #The Length of the character + in the string augmented of 1

  chPlus       <- stringr::str_split_fixed(model,"[+]",n = ChPlusLength) #Creating a table of string

  getColData   <- NULL

  #Retrieving the number of columns for all explanatory variables
  for (i in 1 : length(chPlus)){

    H <- stringr::str_trim(chPlus[i]) #Decomposing the exploratory variable one by one from Linear Model presentation

    for (j in 1:dim(dataframe)[2]){

      #Take one by one chosen exploratory variables
      if (names(dataframe)[j] == H){

             colData <- j
             break;
      }

    }#End-for

    getColData <- paste(getColData,colData,sep=",") #Concat the number of exp variable column in the dataframe

}#End-for

  expVarColumns <- getColData

  return(expVarColumns)#Return

}#End function

getRidgeValue <- function(dataframe,nbResp,nbExp){

  #Putting the exploratory in the vector
  nbExp <- substr(nbExp,2,stringr::str_length(nbExp))

  #Taking the element of the pair one by one
  nbExp <- stringr::str_split_fixed (nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )

  dFrame <- data.frame(row.names=1:nrow(dataframe))

  n <- 0

  for (i in 1 : length(nbExp)){

    n <- as.integer(nbExp[i])

    dataCol <- dataframe[n]

    dFrame  <- cbind(dFrame,dataCol)

  }#End-for

  dFrame <- as.data.frame(dFrame)

  x <- as.matrix(dFrame)

  y <- as.matrix(dataframe[nbResp])

  #fit model
  fit <- glmnet::glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)

  #summarize the fit
  summary(fit)

  #make predictions
  predictions <- predict(fit, x, type="link")

  #summarize accuracy
  rmse <- mean((y - predictions)^2)

  #return
  return (rmse)

}#End-function

getLassoValue <- function(dataframe,nbResp,nbExp){

  #Putting the exploratory in the vector
  nbExp <- substr(nbExp,2,stringr::str_length(nbExp))

  #Taking the element of the pair one by one
  nbExp <- stringr::str_split_fixed (nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )

  dFrame <- data.frame(row.names=1:nrow(dataframe))

  n <- 0

  for (i in 1 : length(nbExp)){

    n <- as.integer(nbExp[i])

    dataCol <- dataframe[n]

    dFrame  <- cbind(dFrame,dataCol)

  }#End-for

  dFrame <- as.data.frame(dFrame)

  x <- as.matrix(dFrame)

  y <- as.matrix(dataframe[nbResp])

  #fit model
  fit <- lars::lars(x, y, type="lasso")

  #summarize the fit
  summary(fit)

  #select a step with a minimum error
  best_step <- fit$df[which.min(fit$RSS)]

  # make predictions
  predictions <- predict(fit, x, s=best_step, type="fit")$fit

  # summarize accuracy
  rmse <- mean((y - predictions)^2)

  #return
  return (rmse)

}#End-function

getElasticNetValue <- function(dataframe,nbResp,nbExp){

  #Putting the exploratory in the vector
  nbExp <- substr(nbExp,2,stringr::str_length(nbExp))

  #Taking the element of the pair one by one
  nbExp <- stringr::str_split_fixed (nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )

  dFrame <- data.frame(row.names=1:nrow(dataframe))

  n <- 0

  for (i in 1 : length(nbExp)){

    n <- as.integer(nbExp[i])

    dataCol <- dataframe[n]

    dFrame  <- cbind(dFrame,dataCol)

  }#End-for

  dFrame <- as.data.frame(dFrame)
  x <- as.matrix(dFrame)
  y <- as.matrix(dataframe[nbResp])

  #fit model
  fit <- glmnet::glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)

  #summarize the fit
  summary(fit)

  #make predictions
  predictions <- predict(fit, x, type = "link")

  #summarize accuracy
  rmse <- mean((y - predictions)^2)

  #return
  return (rmse)

}#End-function

getMallowCp <- function (dataframe,nbResp,nbExp){

  #Putting the exploratory in the vector
  nbExp <- substr(nbExp,2,stringr::str_length(nbExp))

  #Taking the element of the pair one by one
  nbExp <- stringr::str_split_fixed (nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )

  dFrame <- data.frame(row.names=1:nrow(dataframe))

  n <- 0

  for (i in 1 : length(nbExp)){

    n <- as.integer(nbExp[i])

    dataCol <- dataframe[n]

    dFrame  <- cbind(dFrame,dataCol)

  }#End-for

  dFrame <- as.data.frame(dFrame)

  x <- as.matrix(dFrame)

  y <- as.matrix(dataframe[nbResp])

  #Mallows Cp

  Cp <- leaps::leaps(x, y,names = names(dataframe)[nbExp],method = "Cp")$Cp

  Cp <- Cp[which.min(Cp)]

  #return
  return(Cp)

}#End-function
