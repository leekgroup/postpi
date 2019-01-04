#' @import InterVA4
#'
#' @title Translate alphebatic matrix into numeric order matrix
#' @description TTranslate alphebatic matrix into numeric order matrix
#' @param x alphabetic matrix
#' @param problevel customized probbase level
#' @return
#'  \item{indic}{provide indicator}
#'  \item{Yt}{provide Yt}
#'  \item{cond.prob}{provide conditional probability matrix}
#' @keywords provide numeric order matrix
#' @examples
#' \dontrun{
#' Trans_matrix = LevelTrans(x, problevel)
#' }
#' @export
LevelTrans <- function(x, problevel){
  # Translate alphebatic matrix into numeric order matrix
  # Args:
  # 	x: alphabetic matrix
  #   y: customized probbase level
  # Return:
  #	order matrix, order increase as prob decreases
  a <- dim(x)[1]
  b <- dim(x)[2]
  if(is.null(a)){
    y <- rep(0,length(x))
  }else{
    y <- matrix(0, a, b)
  }
  y[x == "I"] <- problevel[1]
  y[x == "A+"] <- problevel[2]
  y[x == "A"] <- problevel[3]
  y[x == "A-"] <- problevel[4]
  y[x == "B+"] <- problevel[5]
  y[x == "B"] <- problevel[6]
  y[x == "B-"] <- problevel[7]
  y[x == "B -"] <- problevel[7]
  y[x == "C+"] <- problevel[8]
  y[x == "C"] <- problevel[9]
  y[x == "C-"] <- problevel[10]
  y[x == "D+"] <- problevel[11]
  y[x == "D"] <- problevel[12]
  y[x == "D-"] <- problevel[13]
  y[x == "E"] <- problevel[14]
  y[x == "N"] <- problevel[15]
  y[x == ""] <- problevel[15]
  return(y)
}


sim <- function(N, cond.prob, shuffle = FALSE,
                csmf, seed, missing.all = NULL,
                withID = TRUE,
                missing.comp = 0,
                missing.pos = NULL,
                missing.neg = NULL,
                false.pos = NULL,
                false.neg = NULL,
                group){
  # Simulate data and true cod given probbase and csmf
  # Args:
  # 	N: number of data to simulate
  # 	cond.prob: 60 * 245 matrix
  #   csmf: true csmf to simulate from
  #   seed: set seed
  #	InterVA: True if using interVA probbase
  #   problevel: if InterVA is false, then provide a vector of probs
  #   shuffle: shuffle the probbase matrix
  #	withID: add first column of ID if true
  #	is.Numeric: whether matrix of 0/1's or "Y"s
  #   missing.comp: number of symptoms completely missing
  #   missing.pos: proportion of positive response missing
  #	missing.neg: proportion of negative response missing
  #   group: subpopulation information, to avoid replicated ID
  # Return:
  #	indic and Yt

  set.seed(seed)
  S <- dim(cond.prob)[1]
  C <- dim(cond.prob)[2]
  indic <- matrix(0, N, S)
  if(shuffle){
    nr <-  dim(cond.prob)[1]
    nc <-  dim(cond.prob)[2]
    cond.prob = matrix(cond.prob[sample(seq(1, nr * nc), nr * nc)],
                       nr, nc)
  }
  # if(!is.null(missing.all)){
  # 	cond.prob[missing.all, ] <- 0
  # }

  Yt <- sample(seq(1:C), N, replace = TRUE, prob=csmf)

  for(i in 1 : N){
    temp <- cond.prob[, Yt[i]]
    age.prob <- temp[1:7]
    age.prob <- age.prob/ sum(age.prob)
    age.indic <- sample(seq(1:7), size=1, prob = age.prob)
    sex.prob <- temp[8:9]
    sex.prob <- sex.prob/sum(sex.prob)
    sex.indic <- sample(seq(1:2), size = 1, prob = sex.prob)
    indic[i, age.indic] <- 1
    indic[i, (sex.indic+7)] <- 1
    indic[i, 10:S] <- rbinom((S-9), 1, temp[10:S] )
  }

  if(!is.null(missing.all)){
    indic[, missing.all] <- "."
  }

  # back up age and sex, they assume not missing
  indic.sexage <- indic[, 1:9]
  indic <- indic[,-c(1:9)]
  if(missing.comp > 0){
    tomiss <- sample(seq(10, dim(indic)[2]), size = missing.comp)
    indic[,tomiss] <- "."
  }
  if(!is.null(missing.pos)){
    tomiss <- sample(which(indic == 1),
                     round(length(which(indic == 1)) * missing.pos),
                     replace = FALSE)
    indic[tomiss] <- "."
  }

  if(!is.null(missing.neg)){
    tomiss <- sample(which(indic == 0),
                     round(length(which(indic == 0)) * missing.neg),
                     replace = FALSE)
    indic[tomiss] <- "."
  }
  indic <- cbind(indic.sexage, indic)
  indic[which(indic == 1)] <- "Y"
  indic[which(indic == 0)] <- ""

  if(!is.null(false.pos)){
    topos <- sample(which(indic ==  ""),
                    round(length(which(indic == "")) * false.pos),
                    replace = FALSE)
    indic[topos] <- "Y"
  }

  if(!is.null(false.neg)){
    toneg <- sample(which(indic ==  "Y"),
                    round(length(which(indic == "Y")) * false.pos),
                    replace = FALSE)
    indic[toneg] <- ""
  }

  if(withID){
    ID <- paste(group, seq(1,N))
    indic <- cbind(ID, indic)
  }
  data(SampleInput)
  colnames(indic) <- colnames(SampleInput)
  return(list(indic = indic, Yt = Yt, cond.prob = cond.prob))
}
