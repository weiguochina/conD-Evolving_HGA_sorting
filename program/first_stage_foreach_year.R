#' Estimates the first stage of a sorting model
#'
#' @details
#' Optimization occurs via the maxLik package for an obtimization of a conditional logit model
#'
#' @param code_name Indicates (with name or column number) the vector with alternative chosen
#' @param Z_names Indicates (with names or column numbers) the vectors with individual data
#' @param X_names Indicates (with names or column numbers) the vectors with alternative data
#' @param data Dataset to be used
#' @param print_detail Controls the output detail of maxLik (default is set at 3)
#'
#' @return A (maxLik) object with the estimates (maxLik) together with the indicators of the alternative chosen,
#'  the vectors with individual data, the vectors with city/regional data and the name of the reference alternative
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by_ summarise_all funs
#' @importFrom maxLik parallel foeach
#'
#' @export
#'
#' @examples
#' code_name <- c("mun_code")
#' Z_names <- c("age","income")
#' X_names <- c("lnprice","nature","monuments")
#' data <- municipality
#' s1.results <- first_stage(code_name, Z_names, X_names, data, print_detail = 1)
first_stage_foreach_year <- function(code_name, Z_names, X_names, initials, data, chunk_size, print_detail = 3){
  
  n.cores <- parallel::detectCores() - 1
  # my.cluster <- parallel::makeCluster(
  #   n.cores, 
  #   type = "PSOCK"
  # )
  # doParallel::registerDoParallel(cl = my.cluster)

  
  bpparam <- SnowParam(workers=n.cores, type="SOCK")
  
  
  code  <- data[code_name]
  Z     <- data[Z_names]
  X     <- data[X_names]
  
  Z <- data.frame(code,Z)
  X <- data.frame(code,X)
  group <- names(X)[1]
  
  # First, create the city specific data matrices
  data_alt <- X %>%
    group_by_at(group) %>%
    summarise_all( ~ mean(., na.rm =TRUE))
  data_alt <- data.matrix(data_alt)
  rownames(data_alt) <- data_alt[,1]
  
  # Second, create the individual specific data matrices (select here your variables)
  datamat <- data.matrix(Z)
  nid <- as.numeric(as.factor(datamat[,1]))

  # demean the invididual data, not where dummies
  dummies.ind <- grep(TRUE, apply(datamat,2, function(x)  all(x %in% c(0,1)))) # identify dummy variables
  datamat[,-c(1,dummies.ind)] <- scale(datamat[,-c(1,dummies.ind)], scale = FALSE, center = TRUE)
  
  # data[,2:ncol(data)]<-scale(data[,2:ncol(data)])
  # data_alt <- scale(data_alt)
  
  # make sure each variable has a name
  varNames <- c(outer(colnames(datamat[,2:ncol(datamat)]), colnames(data_alt[,2:ncol(data_alt)]), FUN= paste, sep=":"))
  # There should be one base catetory! Here the first
  varNames <- c(rownames(data_alt)[2:nrow(data_alt)],varNames)
  
  startValues <- initials
  # gradi <- Matrix(0L, nrow(datamat), length(varNames))
  names(startValues) <- varNames
  
  
  # # define the log likelihood function for a single choice i
  # LogLiki <- function(i, param, mat, datamat, data_alt, varNames, nid) {
  #   xij <- cbind(mat,kronecker(t(datamat[i,2:ncol(datamat)]),(data_alt[,2:ncol(data_alt)]), make.dimnames=TRUE))
  #   col.order <- c(seq(1:(nrow(data_alt)-1)), match(varNames, colnames(xij))[!is.na(match(varNames, colnames(xij)))])
  #   row.order <- c(1,match(paste0(":",varNames), rownames(xij))[!is.na(match(paste0(":",varNames), rownames(xij)))])
  #   xij <- xij[row.order, col.order]
  #   xbij <- xij[,nrow(data_alt):length(varNames)] %*% param[nrow(data_alt):length(varNames)] + c(0,param[1:nrow(data_alt)-1])
  #   pij <- exp(xbij)/sum(exp(xbij))
  #   Prob_i <- log(pij[nid[i]])
  #   Yvec <- rep(0,nrow(data_alt))
  #   Yvec[nid[i]] <- 1
  #   Yvec <- t(Yvec - pij)%*%xij
  #   return(cbind(Prob_i,Yvec))
  # }
  
  index <- split(seq_len(nrow(datamat)), 
                 ceiling(seq_along(seq_len(nrow(datamat)))/chunk_size))
  
  if(length(index[[length(index)]])==1) {
    index[[length(index)-1]] <- c(index[[length(index)-1]], index[[length(index)]])
    index[[length(index)]] <- NULL
  }

  mat <- Diagonal(nrow(data_alt))[,-1]
  
  
  # # define the log likelihood function for a single chunk of choices i
  LogLiki <- function(i, param, mat, datamat, data_alt, varNames, nid, index) {
    
    id <- nid[index[[i]]]
    Jij <- rep(seq_len(length(index[[i]])), each = nrow(data_alt))
    Iij <- (seq_len(length(index[[i]]))-1) * nrow(data_alt) + id # the J matrix will follow the sequence of factor(J)
    JMat <- Matrix(0L, length(Jij), max(Jij), sparse = TRUE)
    JMat[cbind(seq_len(length(Jij)), Jij)] <- 1L
    

    Xij <- cbind(do.call("rbind",  replicate(length(index[[i]]), mat) ),
                 kronecker(datamat[index[[i]],2:ncol(datamat)],(data_alt[,2:ncol(data_alt)]), 
                     make.dimnames=TRUE) 
    )
    
    Yvec <- rep(0,length(Jij))
    Yvec[Iij] <- 1
    
    Xbij <- Xij[,(nrow(data_alt)):(length(varNames))] %*% param[nrow(data_alt):length(varNames)] +
      do.call("rbind",  replicate(length(index[[i]]), matrix(c(0,param[1:nrow(data_alt)-1])), simplify=FALSE))
    Xb <- data.table(as.vector(exp(Xbij)), Jij)
    names(Xb) <- c("expXb", "Ji")
    Xb[, sum_expXb := sum(expXb), by = Ji]
    Xb[, pij := expXb/sum_expXb]
    Prob <- Xb$pij[Iij]
    gradi <<- t((Yvec - c(Xb$pij)) * JMat) %*% Xij
    
    # xij <- cbind(mat,kronecker(t(datamat[i,2:ncol(datamat)]),(data_alt[,2:ncol(data_alt)]), make.dimnames=TRUE))
    # col.order <- c(seq(1:(nrow(data_alt)-1)), match(varNames, colnames(xij))[!is.na(match(varNames, colnames(xij)))])
    # row.order <- c(1,match(paste0(":",varNames), rownames(xij))[!is.na(match(paste0(":",varNames), rownames(xij)))])
    # xij <- xij[row.order, col.order]
    # xbij <- xij[,nrow(data_alt):length(varNames)] %*% param[nrow(data_alt):length(varNames)] + c(0,param[1:nrow(data_alt)-1])
    # pij <- exp(xbij)/sum(exp(xbij))
    # Prob_i <- log(pij[nid[i]])
    # Yvec <- rep(0,nrow(data_alt))
    # Yvec[nid[i]] <- 1
    # Yvec <- t(Yvec - pij)%*%xij
    return(cbind(Prob,gradi))
  }
  
  # define the log likelihood function over all choices
  LogLikFun <- function(param) {

    datamat = datamat 
    data_alt = data_alt
    varNames = varNames 
    nid = nid
    
    # Y <- foreach(
    #   i = 1:nrow(datamat)
    # ) %dopar% {
    #   xij <- cbind(mat,kronecker(t(datamat[i,2:ncol(datamat)]),(data_alt[,2:ncol(data_alt)]), make.dimnames=TRUE))
    #   col.order <- c(seq(1:(nrow(data_alt)-1)), match(varNames, colnames(xij))[!is.na(match(varNames, colnames(xij)))])
    #   row.order <- c(1,match(paste0(":",varNames), rownames(xij))[!is.na(match(paste0(":",varNames), rownames(xij)))])
    #   xij <- xij[row.order, col.order]
    #   xbij <- xij[,nrow(data_alt):length(varNames)] %*% param[nrow(data_alt):length(varNames)] + c(0,param[1:nrow(data_alt)-1])
    #   pij <- exp(xbij)/sum(exp(xbij))
    #   Prob_i <- log(pij[nid[i]])
    #   Yvec <- rep(0,nrow(data_alt))
    #   Yvec[nid[i]] <- 1
    #   Yvec <- t(Yvec - pij)%*%xij
    #   return(cbind(Prob_i,Yvec))
    # }
    
    Y <- bplapply(1:length(index), LogLiki, param = param, mat = mat, datamat = datamat, data_alt = data_alt,
                  varNames = varNames, nid = nid, index = index, BPPARAM=bpparam)
    
    Y <- do.call(rbind, Y)
    

    Prob <- c(Y[,1])
    gradi <<- Y[,2:ncol(Y)]
    
    return(Prob)
  }
  
  gradlik <- function(param) gradi
  
  estimates <- maxLik(LogLikFun, gradlik, start=startValues, 
                      print.level=print_detail, method = "BHHH",
                      control = list(tol = 1e-5))
  
  # parallel::stopCluster(my.cluster)
  # doParallel::stopImplicitCluster()
  
  estimates$code_name <- code_name
  estimates$Z_names <- Z_names
  estimates$X_names <- X_names
  estimates$base_alt <- rownames(data_alt)[1]
  
  return(estimates)
}
