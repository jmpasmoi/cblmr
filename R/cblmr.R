#' Choice of Best Linear Model
#'
#'
#' This function allows you to choose the best model from among different "best" models.
#' Evaluating based on predictions and goodness of fit.
#'
#' @param dataframe only numerical values are required(explanatory and response variables)
#' @param response  dependent variable
#' @param exp.comb  number of expected combinations
#' @param choice when choice=TRUE it return the best model based on exp.comb. Default choice=FALSE
#'
#' @return a tab grouping predictions and goodness of fit per model
#' @author Jean Marie Cimula
#' @details
#' Evaluating based on predictions(PRESS : Prediction Sum of Squares) and
#' goodness of fit(AIC, BIC, R Squared, Adj. Squared).
#'
#' @export
#'


blm_choice <- function(dataframe, response, exp.comb, ..., choice = FALSE){




  f <- formals(blm_choice)  # Capture formal arguments

  formalNames <- names(f)

  exc <- do.call(missing, list(formalNames[3]))
  chc <- do.call(missing, list(formalNames[5]))

  if(chc == FALSE && exc == TRUE){

    stop("Missing exp.comb as integer", call. = FALSE)

  }

  if(exc == TRUE){ ec <- "abs"}
  else{

    if(exp.comb > 0 && is.numeric(exp.comb) == TRUE){ec <- exp.comb}
    else{stop("Unexpected value. Please type positive integer")}
  }


  input_unit <- dataframe

  response  <- response

  #attach(input_unit)

  #(1) Number of colums of the data frame
  get_len <- dim(input_unit)[2]

  for(i in 1:get_len){

    #Removing the response variable in the temporary data frame
    if(names(input_unit)[i] == response){

      exp_layer <- input_unit [,-i]

      break;

    }#if-condition

  }#Loop for

  #(2)Number of colums of the temporary data frame of explanatory variables
  get_len_expvar <- dim(exp_layer)[2]

  #(3)Initializing a Matrix which will contain all combinations of predictor model
  exp_var_mat <- matrix( ncol = get_len_expvar)

  #Creating combination
  for(i in 1:get_len_expvar){
    #Combination function
    comb      <- t(combn(names(exp_layer),i))
    nb_row   <- nrow(comb)
    nb_col   <- length(names(exp_layer))
    nb_row_na <- nb_row
    nb_col_na <- nb_col-ncol(comb)
    na_matr    <- matrix(rep(NA, nb_row_na*nb_col_na), nrow = nb_row_na, ncol = nb_col_na)
    result    <- cbind(comb, na_matr)
    exp_var_mat <- rbind(exp_var_mat, result)

  }#Loop for
  #Removing all NA
  exp_var_mat <- exp_var_mat[-1,]

  #Final result of combination between response and explanatory variables
  #Setting an empty data frame
  make_your_choice <- data.frame()

  for(i in 1:nrow(exp_var_mat)){

    getval    <- na.omit(exp_var_mat[i, ])



    mdRegComb <- paste(response, " ~ ", paste(getval, collapse = " + "), sep = "")

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

    if(stringr::str_detect(mdRegComb,'[+]') == TRUE){

      #Checking the tilde in the model
      delim   <- unlist(gregexpr(pattern ='[~]',mdRegComb)) - 1

      #Retrieving the part before the tilde with the response variable
      stPart  <- substr(mdRegComb,1,delim)

      #Trim the result retrieved
      stPart  <- stringr::str_trim(stPart)

      #Retrieving the second part with the exploratory variables
      sndPart <- substr(mdRegComb,delim + 2, stringr::str_length(mdRegComb))

      nbResp  <- getNumberResponse(input_unit,stPart) #Column of Response variable
      nbExp   <- getNumberExploratory(input_unit,sndPart)#Columns of Exploratory variables
      Ridge   <- getRidgeValue(input_unit,nbResp,nbExp)
      Lasso   <- getLassoValue(input_unit,nbResp,nbExp)
      ElasticNet <- getElasticNetValue(input_unit,nbResp,nbExp)
      Cp         <- getMallowCp(input_unit,nbResp,nbExp)

      }

    #Assembling diagnostic parameters per model predictor in Matrix of all combinations
    dFrame <- data.frame(reg_model = mdRegComb,RSquared = RSqrt,AdjustedRSquared = AdjRSqrt,AIC = AIC,BIC = BIC,PRESS = PL,Ridge = Ridge,Lasso = Lasso, ElasticNet = ElasticNet,Cp=Cp, nexp = length(getval))#, nbResp = nbResp, nbExp = nbExp)#, Accuracy = RidgeRegression)

    #Loading data frame
    make_your_choice <- rbind(make_your_choice,dFrame)
  }

    make_your_choice <- as.data.frame(make_your_choice)

    if(is.numeric(ec)==TRUE){

      df <- make_your_choice[make_your_choice$nexp==ec,]

    }else {

      df <- make_your_choice
    }

    rownames(df) <-  NULL
    colnames(df)[2] <- "r_squared"
    colnames(df)[3] <- "adj_r_squared"

    if(choice){

      df <- sqldf::sqldf("
             select * from df
                      where r_squared = (select max(r_squared) from df)
                            and adj_r_squared = (select max(adj_r_squared) from df)
                            and Cp = (select max(Cp) from df)
                            and AIC = (select min(AIC) from df)
                            and BIC = (select min(BIC) from df)
                            and PRESS = (select min(PRESS) from df)
                            and Ridge = (select min(Ridge) from df)
                            and Lasso = (select min(Lasso) from df)
                            and ElasticNet = (select min(ElasticNet) from df)
            ")

    }

    knitr::kable(df)

}#End function

getNumberResponse <- function(dataframe, model){

  #Retrieving the number of the column of the response variable

  colData <- 0

  for(j in 1:dim(dataframe)[2]){
    #Take one by one chosen exploratory variables
    if(names(dataframe)[j] == stringr::str_trim(model)){

         colData <- j
         break;
    }

    colData <- colData

  }#End-for

  respVarColumn <- colData #Response variable number in the dataframe

  #print(respVarColumn)
  return(respVarColumn)

  }#End function

getNumberExploratory <- function(dataframe, model){

  ChPlusLength <- length(unlist(gregexpr(pattern = "[+]",model))) + 1 #The Length of the character + in the string augmented of 1

  chPlus       <- stringr::str_split_fixed(model,"[+]",n = ChPlusLength) #Creating a table of string

  getColData   <- NULL

  #Retrieving the number of columns for all explanatory variables
  for(i in 1 : length(chPlus)){

    H <- stringr::str_trim(chPlus[i]) #Decomposing the exploratory variable one by one from Linear Model presentation

    for(j in 1:dim(dataframe)[2]){

      #Take one by one chosen exploratory variables
      if(names(dataframe)[j] == H){

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
  nbExp <- stringr::str_split_fixed(nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )

  dFrame <- data.frame(row.names=1:nrow(dataframe))

  n <- 0

  for(i in 1 : length(nbExp)){

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
  return(rmse)

}#End-function

getLassoValue <- function(dataframe,nbResp,nbExp){

  #Putting the exploratory in the vector
  nbExp <- substr(nbExp,2,stringr::str_length(nbExp))

  #Taking the element of the pair one by one
  nbExp <- stringr::str_split_fixed(nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )

  dFrame <- data.frame(row.names=1:nrow(dataframe))

  n <- 0

  for(i in 1 : length(nbExp)){

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
  return(rmse)

}#End-function

getElasticNetValue <- function(dataframe,nbResp,nbExp){

  #Putting the exploratory in the vector
  nbExp <- substr(nbExp,2,stringr::str_length(nbExp))

  #Taking the element of the pair one by one
  nbExp <- stringr::str_split_fixed(nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )

  dFrame <- data.frame(row.names=1:nrow(dataframe))

  n <- 0

  for(i in 1 : length(nbExp)){

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
  return(rmse)

}#End-function

getMallowCp <- function(dataframe,nbResp,nbExp){

  #Putting the exploratory in the vector
  nbExp <- substr(nbExp,2,stringr::str_length(nbExp))

  #Taking the element of the pair one by one
  nbExp <- stringr::str_split_fixed(nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )

  dFrame <- data.frame(row.names=1:nrow(dataframe))

  n <- 0

  for(i in 1 : length(nbExp)){

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
