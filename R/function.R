#' Solve a two-person zero-sum game.
#' @name solveGame
#' @param matrix A matrix.
#' @return The value of the game \code{matrix}, the saddlepoints of the \code{matrix} if existing and the strategies of the players if the strategies are not pure.
#' @examples
#' solveGame(matrix(c(10,2,2,10,4,8,1,12), ncol = 2, byrow = TRUE))
#' solveGame(matrix(c(-3,1,2,0,5,2,3,2,2,-3,1,-3,1,0,-2,2), nrow = 4, byrow = TRUE))

solveGame <- function(matrix) {
  if(!is.matrix(matrix)){
    stop("Please enter a matrix!")
  }
  out <- list()
  minRow = getMaxOfRowMin(matrix)
  maxCol = getMinOfColMax(matrix)
  if(minRow == maxCol){
    out['saddlePoints'] <- list(saddlePoints = generateMatrixFromSaddleVector(getSaddlePointsOfGame(matrix, maxCol)))
    out['value'] <- list(value = minRow)
  } else {
    solution <- list()
    solutionA = solveLinearProgram(matrix, "max");
    solutionB = solveLinearProgram(matrix, "min");
    if(length(solutionA[-1]) > length(solutionB[-1])) {
      strategieMatrix <- matrix(NA, nrow=2, ncol=length(solutionA[-1]))
      strategieMatrix[1,] <- solutionA[-1]
      strategieMatrix[2,] <- solutionB[-1]
      out['strategies'] <- list(stragedies = strategieMatrix)
    } else {
      strategieMatrix <- matrix(NA, nrow=2, ncol=length(solutionB[-1]))
      strategieMatrix[1,] <- solutionA[-1]
      strategieMatrix[2,] <- solutionB[-1]
      out['strategies'] <- list(stragedies = strategieMatrix)
    }
    out['value'] <- list(value = solutionA[1])
    out['saddlePoints'] <- list(saddlePoints = NULL)
  }
  return(out)
}

#' Find the maximum of the rows minima.
#' @name getMaxOfRowMin
#' @param matrix A matrix
#' @return The maximum of the rows minima as numeric.

getMaxOfRowMin <- function(matrix) {
  numRow = nrow(matrix)
  minRowV = c();
  for(i in 1:numRow){
    minRowV[i] = min(matrix[i,]);
  }
  return(max(minRowV))
}

#' Find the minimum of the columns maxima.
#' @name getMinOfColMax
#' @param matrix A matrix
#' @return The minimum of the columns maxima as numeric.

getMinOfColMax <- function(matrix) {
  numCol = ncol(matrix)
  maxColV = c();
  for(i in 1:numCol){
    maxColV[i] = max(matrix[,i]);
  }
  return(min(maxColV))
}

#' Find the saddlepoints of the game.
#' @name getSaddlePointsOfGame
#' @param matrix A matrix
#' @param maxCol A numeric
#' @return The matrix of saddlepoints.

getSaddlePointsOfGame <- function(matrix, maxCol) {
  numCol = ncol(matrix)
  numRow = nrow(matrix)
  index = 1;
  saddle <- c()
  for(i in 1:numRow){
    for(j in 1:numCol){
      if(maxCol == matrix[i,j]) {
        if(min(matrix[i,]) == max(matrix[,j])) {
          saddle[index] <- i
          index = index + 1
          saddle[index] <- j
          index = index + 1
        }
      }
    }
  }
  return(generateMatrixFromSaddleVector(saddle))
}

#' Generates a mx2 matrix from the saddlepoints vector.
#' @name generateMatrixFromSaddleVector
#' @param vector A vector.
#' @return The matrix of saddlepoints formated mx2 from \code{vector}.

generateMatrixFromSaddleVector <- function(vector){
  return(matrix(vector, ncol = 2, byrow = TRUE))
}

#' Solve the linear program of the gamematrix
#' @name solveLinearProgram
#' @param matrix A matrix.
#' @param minmax A string.
#' @return The max or min value of the game and the probabilities of the players strategies.

solveLinearProgram <- function(matrix, minmax){
  ncol = ncol(matrix)
  nrow = nrow(matrix)

  if(minmax == "max") {
    operator = "<="
    matrix = t(-matrix)
  } else {
    operator = ">="
    matrix = -matrix
  }
  print(matrix)
  matrix <- rbind(c(rep(1, ncol(matrix))), matrix)
  print(matrix)
  matrix <- cbind(c(0,rep(1, nrow(matrix)-1 )), matrix)
  print(matrix)
  f.obj <- c(1,rep(0, ncol(matrix)-1 ))
  print(f.obj)
  f.con <- matrix
  f.dir <- c("==",rep(operator, nrow(matrix)-1 ))
  f.rhs <- c(1,rep(0, nrow(matrix)-1 ))
  print(f.rhs)
  print(lp(minmax, f.obj, f.con, f.dir, f.rhs)$duals)
  #return(lp(minmax, f.obj, f.con, f.dir, f.rhs)$solution)
}

#' Plot the graphical solution of a 2xn or mx2 matrixgame
#' @name plotSolution
#' @param matrix A matrix.
#' @param rowOrCol A string.
#' @return The plot of the solution in a cartesian coordinate system with a legend.

plotSolution <- function(matrix, rowOrCol='row'){
  print(matrix)
  if(isMatrixOfRightDimention(matrix)){
    stop("Enter 2xn or mx2 matrix!")
  }

  if(ncol(matrix) > nrow(matrix)){
    minOrMax <- 'max'
    solveLP = solveLinearProgram(matrix, minOrMax)
    matrix = t(matrix)
  } else if(ncol(matrix) == nrow(matrix)){
    if(rowOrCol == 'row'){
      minOrMax <- 'max'
      solveLP = solveLinearProgram(matrix, minOrMax)
      matrix = t(matrix)
    } else if(rowOrCol == 'col') {
      minOrMax <- 'min'
      solveLP = solveLinearProgram(matrix, minOrMax)
    } else {
      stop("Enter row or col for param!")
    }
  } else {
    minOrMax <- 'min'
    solveLP = solveLinearProgram(matrix, minOrMax)
  }
  p <- ggplot2::ggplot(data.frame(x=c(0,1)), mapping = aes(x = x))
  p <- p + ggplot2::theme_classic()
  p <- p + ggplot2::scale_y_continuous(sec.axis = dup_axis())
  p <- p + ggplot2::scale_colour_manual("Strategies", values=generateColors(nrow(matrix)))
  for(i in 1:nrow(matrix)){
    label <- generateFunctionName(i, matrix)
    p <- p + ggplot2::stat_function(
                    fun=computeLinearFunction,
                    args = list(matrix[i,1], matrix[i,2]),
                    geom="line",
                    aes_(color=label)
                    )
  }
  if(minOrMax == 'min') {
    p <- p + ggplot2::geom_segment(
                    data=drawMinOrMaxLine(matrix, minOrMax),
                    aes(x = x, y = y, xend = xend, yend = yend, colour = "max payment col player")
                    )
  } else {
    p <- p + ggplot2::geom_segment(
                    data=drawMinOrMaxLine(matrix, minOrMax),
                    aes(x = x, y = y, xend = xend, yend = yend, colour = "min payoff row player")
                    )
  }
  p <- p + ggplot2::geom_hline(
                    aes(yintercept=solveLP[1], linetype = paste("v =",format(round(solveLP[1], 5), nsmall = 5))),
                    colour= 'gray'
                    )
  p <- p + ggplot2::geom_vline(
                    aes(xintercept=solveLP[2],linetype = paste("x* = p* =",format(round(solveLP[2], 5), nsmall = 5))),
                    colour= 'gray'
                    )
  p <- p + ggplot2::scale_linetype_manual(
                    name = "Game Solution",
                    values = c(3, 3),
                    guide = guide_legend(override.aes = list(color = c("gray", "gray")))
                    )
  p <- p + ggplot2::labs(y = "v", x = "x")
  return(p)
}

#' Check if the matrix has the right format of 2xn or mx2
#' @name isMatrixOfRightDimention
#' @param matrix A matrix.
#' @return The TRUE if the matrix is of the right format or FALSE if not.

isMatrixOfRightDimention <- function(matrix) {
  return(!is.matrix(matrix) || (dim(matrix)[1] > 2 & dim(matrix)[2] > 2))
}

#' Generates random colors for the linear functions
#' @name generateColors
#' @param numOfRows A integer.
#' @return The colors for the linear functions.

generateColors <- function(numOfRows) {
  hues = seq(15, 375, length = numOfRows + 1)
  colors <- hcl(h = hues, l = 65, c = 100)[1:numOfRows]
  colors[numOfRows+1] <- "#000000"
  colors <- unlist(strsplit(colors, " "))
}

#' Generates name for the linear function
#' @name generateFunctionName
#' @param i A integer.
#' @param matrix A matrix.
#' @return The name of the linear function as a string.

generateFunctionName <- function(i, matrix){
  return(paste("e",i,"=",matrix[i,1],"*x +",matrix[i,2],"*(1-x)" ))
}

#' Find the intersections of two functions
#' @name computeIntersection
#' @param a A numeric.
#' @param b A numeric.
#' @param c A numeric.
#' @param d A numeric.
#' @return The intersection of the two functions or NA if there is not one.

computeIntersection <- function(a, b, c, d){
  if((a-c-b+d) == 0)
    return(NA)
  return((d-b)/(a-c-b+d))
}

#' Compute the function value f(x) of a function
#' @name computeLinearFunction
#' @param x A numeric.
#' @param a A numeric.
#' @param b A numeric.
#' @return The function value f(x) of a function.

computeLinearFunction <- function(x, a, b) {
  return(a*x + b*(1-x))
}

#' Draw the line of the max or min of all functions
#' @name drawMinOrMaxLine
#' @param matrix A matrix.
#' @param minmax A string.
#' @return The data frame of the segments of the min or max function.

drawMinOrMaxLine <- function(matrix, minmax){
  intersections <- getAllIntersections(matrix)
  values = getMinOrMaxFunctionsValueForIntersections(matrix, intersections, minmax)
  offsetDataFrame <- data.frame(x=values[,1], y=values[,2], xend=values[,3], yend=values[,4])
  return(offsetDataFrame)
}

#' Find all intersections of the functions
#' @name getAllIntersections
#' @param matrix A matrix.
#' @return The function values of the functions intersections in sorted vector.

getAllIntersections <- function(matrix){
  intersections <- c()
  index <- 1
  for(i in 1:(nrow(matrix))){
    for(j in 1:(nrow(matrix))){
      intersection <- computeIntersection(matrix[i,1], matrix[i,2], matrix[j,1], matrix[j,2])
      if(!(is.na(intersection) || is.element(intersection, intersections))) {
        intersections[index] <- intersection
        index <- index + 1
      }
    }
  }
  intersections[index] = 0
  intersections[index+1] = 1
  return(sort(intersections))
}

#' Find the min or max functions value for each intersection
#' @name getMinOrMaxFunctionsValueForIntersections
#' @param matrix A matrix.
#' @param intersections A vector.
#' @param minmax A string.
#' @return The matrix of the intersection min/max function value tupel.

getMinOrMaxFunctionsValueForIntersections <- function(matrix, intersections, minmax){
  functionValuesForIntersection <- c()
  minOrMaxFunctionsValue <- matrix(NA, ncol = 4, nrow = length(intersections))
  for(i in 1:length(intersections)){
    for(j in 1:nrow(matrix)){
      functionValuesForIntersection[j] <- computeLinearFunction(
                      intersections[i],
                      matrix[j,1],
                      matrix[j,2])
    }
    minOrMaxFunctionsValue[i,1] <- intersections[i]
    if(minmax == 'max'){
      minOrMaxFunctionsValue[i,2] <- min(functionValuesForIntersection)
    } else {
      minOrMaxFunctionsValue[i,2] <- max(functionValuesForIntersection)
    }
  }
  for (i in 1:nrow(minOrMaxFunctionsValue)-1) {
    minOrMaxFunctionsValue[i,3] <-  minOrMaxFunctionsValue[i+1,1]
    minOrMaxFunctionsValue[i,4] <-  minOrMaxFunctionsValue[i+1,2]
  }
  return(minOrMaxFunctionsValue[-nrow(minOrMaxFunctionsValue),])
}
