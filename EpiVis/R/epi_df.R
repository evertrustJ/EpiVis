#' Check whether a data frame is a proper EpiVis data frame.
#' 
#' Given a data frame, check if it is consistent with the requirements of a proper EpiVis data frame.
#'       
#' @details Note that a proper EpiVis data frame can be a proper superset of in terms of 
#'  variables (x, y, rate, map), but all these variables must have same length after
#'  removing NA's. 
#' 
#' @importFrom stats na.omit
#' 
#' @param data a data frame.
#' @return Returns true if and only if the data frame is a proper EpiVis data frame.
#' @export
epi_check <- function(data=NULL){
  
  if (is.null(data)){
    warning("epi_check failed: Please enter something to begin with.")
    warning(paste0("Expected EpiVis data frame, but received: NOTHING"))
    return(FALSE)
  }
  
  if (!is.data.frame(data)) {
    warning("check.epi failed: A valid EpiVis data frame has to be a data frame.")
    warning(paste0("Expected EpiVis data frame, but received: ", typeof(data)))
    return(FALSE)
  }
  
  pars <- c("x", "y", "rate", "map")
  parsMissing <- NULL
  for (par in pars){
    if (!par %in% colnames(data))
      parsMissing<-c(par, parsMissing)
  }
  
  if (!is.null(parsMissing)){
    warning(paste0("epi_check failed: Following parameters are required 
           for a proper EpiVis Dataframe: ", parsMissing))
    return(FALSE)
  }
  
  x <- data$x
  y <- data$y
  rate <- data$rate
  map <- data$map
  map1 <- data$map[1]

  if (anyNA(x)){
    warning("epi_check failed: NA found in the x coordinate.")
    return(FALSE)
  }
  
  if (anyNA(y)){
    warning("epi_check failed: NA found in the y coordinate.")
    return(FALSE)
  }
  
  if (anyNA(rate)){
    warning("epi_check failed: NA found in rate.")
    return(FALSE)
  }
  
  if (anyNA(map)){
    warning("epi_check failed: NA found in map variable.")
    return(FALSE)
  }
  
  if (any(map!=map1)){
    warning("epi_check failed: map variable is not unique; which one do you want to plot?")
    return(FALSE)
  }
  
  ## x, y, rate have to be numerical.
  if (!(is.numeric(x)||is.numeric(y))){
    warning("epi_check failed: Coordinates entered are non-numeric thus invalid.")
    return(FALSE)
  }
  if(!is.numeric(rate)){
    warning("Rate parameter entered are non-numeric thus invalid.")
    warning(paste0("Expected numerical rate, but received: ", typeof(rate)))
    return(FALSE)
  }
  
  ## Check if parameters are valid.
  if (length(na.omit(x))!=
      length(na.omit(y))){ 
    warning("epi_check failed: Please enter coordinates of equal length.")
    return(FALSE)
  }
  
  if (!all(rate<=1)){
    warning("The rate of the disease cannot exceed one. 
            That is, there cannot be more cases than population.")
    return(FALSE)
  }
  
  if (length(na.omit(rate))!=
      length(na.omit(y))){ 
    warning("epi_check failed: rate parameter has to have the 
            same length as number of coordinates")
    return(FALSE)
  }
  
  return(TRUE)
}  

#' Converge coordinates, rate and map into a proper EpiVis data frame.
#' 
#' Requires all vectors to have the same length, map should be a just string (or a sequence of characters.)
#'        
#' Requires: 1. rate or 2. case and population; will converge to rate eventually.
#'        
#' x, y, rate, case, population are allowed to have NA's, but observations with any NA's will be thrown out.
#'
#' @importFrom stats na.omit
#' 
#' @param x A vector of `n` coordinates of x
#' @param y A vector of `n` coordinates of y
#' @param rate A vector of `n` numerics denoting the rate of the cases (case/population)
#' @param case A vector of `n` numerics denoting the total number of cases
#' @param population A vector of `n` numerics denoting the number of population
#' @param map A string of name of the map of interest
#' 
#' @return A proper EpiVis data frame that is compatible with EpiVis functions.
#' @export
epi_converge <- function(x=NULL, y=NULL, rate=NULL, 
                         case=NULL, population=NULL, map=NULL){
  
  
  ## Asserting the string is the fastest hence is done first.
  ## Map has to be non-empty and has to be supported.
  if (is.null(map)) {
    warning("You probably need to entered a map!")
    return("epi_converge failed.")}

  ## No longer need to check if map is missing.
  pars <- c("x", "y", "rate", "case", "population")
  parsMissing <- NULL
  for (par in pars){
    if (is.null(par)) parsMissing <- c(par, parsMissing)
  }
  
  ## Both rate && cases and population are missing.
  if ("rate" %in% parsMissing &&   
     ("case" %in% parsMissing||"population" %in% parsMissing)){
      warning("Please enter the number of either: 1. cases and population 2. rate.")
      stop()
  ## Cases and population are not missing, we can calculate rate 
  ##   hence it is implicitly not missing.
  } else if ("rate" %in% parsMissing){
    parsMissing<- parsMissing[-which(parsMissing=="rate")]
  } ## We don't care about the else case; if rate is not missing we can just use it.
    
  ## There are missing necessary parameters!
  if (!is.null(parsMissing)){
    warning(paste0("Following parameters are required 
          for a proper EpiVis Dataframe: ", parsMissing))
    stop()
  }

  ## coordinates have to be numeric.
  if (!is.numeric(x)||!is.numeric(y)){
    warning("Expected numerical coordinates [x, y], but received: [", typeof(x),", ", typeof(y),"]")
    stop()
  }
  
  ## coordinates have to have the same length.
  if (length(na.omit(x))!= length(na.omit(y))) {
    warning("Expected coordinates of same length, 
            but received: length of x = ", length(x)," length of y = ", length(y))
    stop()
  }
  
  if (is.null(na.omit(rate))){ 
    ## If we don't have rate, we have to calculate it.
    ##    We have moved this part down here purposefully to improve readability.
    
    ## We have previously asserted that both case and population is non-empty
    
    ## First have to assert that both length of case & population are of the
    ##   same length as coordinates.
    ## No need to assert that length(case) == length(population) as it is done so
    ##   implicitly.
    if (length(na.omit(case))!=length(na.omit(na.omit(x)))) {
      warning(paste0("Expected vector CASE to have length ", 
                     length(na.omit(x)), ", but received a vector CASE with length ",
                     length(na.omit(case))))
      return("epi_converge failed.")
    }
    if (length(na.omit(population))!=length(na.omit(x))){ 
      warning(paste0("Expected vector POPULATION to have length ", 
                     length(na.omit(x)), ", but received a vector POPULATION with length ",
                     length(na.omit(population))))
      stop()
    }
    
    ## Asserting both cases and population are numeric so division is possible.
    if (!is.numeric(case)){
      warning(paste0("Expected numeric CASE, but received: ", typeof(case)))
      stop()
    } 
    if (!is.numeric(population)){
      warning(paste0("Expected numeric POPULATION, but received: ", typeof(population)))
      stop()
    }
    
    if (any(which(population < case))){
      warning(paste0("Expected POPULATION to be greater than or equal to CASE, but received: 
                     POPULATION = ",  population[which(population < case)[1]], 
                     " and CASE = ", case[which(population < case)[1]],
                     " at index ", which(population < case)[1], "."))
      stop()
    }
    if (any(population<0)){
      warning(paste0("Expected Non-negative POPULATION, 
                     but received negative POPULATION at these locations: ",
                     which(population<0)))
      stop()
    }
    if (any(case<0)){
      warning(paste0("Expected Non-negative CASE, 
                     but received negative CASE at these locations: ",
                     which(case<0)))
      stop()
    }
    
    ## Only then can we calculate rate.
    rate <- case/population
  } 
  ## Else, rate exists
  else {
    ## Assert that there are as many rates as x's.
    if (length(na.omit(rate) != length(na.omit(x)))){
      warning(paste0("Expected vector RATE to have length ", 
                     length(na.omit(x)), ", but received a vector RATE with length ",
                     length(na.omit(population))))
      stop()
    }
  }
  ## Have to assert that rate is valid.
  if (!(all(na.omit(rate)<=1)&&all(na.omit(rate)>=0))){ 
    warning("The rate of the disease cannot exceed one. 
            That is, there cannot be more cases than population.")
    stop()
  }
  
  ## Warn the clients that there will be a more observations tossed out.
  if (!identical(which(is.na(x)), which(is.na(y))))
    warning("The indices of missing x and the indices of missing y are not identical.
            As a result, these observations are going to be tossed out.")
  if (!(identical(which(is.na(rate)), which(is.na(y)))||
        identical(which(is.na(rate)), which(is.na(y)))))
    warning("The indices of missing coordinate and the indices of missing rate might not be identical.
            As a result, these observations are going to be tossed out.")
  
  
  epi_df <- data.frame(x=x, y=y, rate=rate)
  epi_df <- na.omit(epi_df) # Tossing out rows with any NA in them
  # We do not want to toss out rows where only map variable is missing
  epi_df <- cbind(epi_df, map=as.character(rep(map[1], nrow(epi_df)))) 
}
