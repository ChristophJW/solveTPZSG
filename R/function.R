game_function <- function(matrix) {
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

generateMatrixFromSaddleVector <- function(vector){
  return(matrix(vector, ncol = 2, byrow = TRUE))
}

getMaxOfRowMin <- function(matrix) {
  numRow = nrow(matrix)
  minRowV = c();
  for(i in 1:numRow){
    minRowV[i] = min(matrix[i,]);
  }
  return(max(minRowV))
}

getMinOfColMax <- function(matrix) {
  numCol = ncol(matrix)
  maxColV = c();
  for(i in 1:numCol){
    maxColV[i] = max(matrix[,i]);
  }
  return(min(maxColV))
}

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
  return(saddle)
}

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

  matrix <- rbind(c(rep(1, ncol(matrix))), matrix)
  matrix <- cbind(c(0,rep(1, nrow(matrix)-1 )), matrix)
  f.obj <- c(1,rep(0, ncol(matrix)-1 ))
  f.con <- matrix
  f.dir <- c("==",rep(operator, nrow(matrix)-1 ))
  f.rhs <- c(1,rep(0, nrow(matrix)-1 ))

  return(lp(minmax, f.obj, f.con, f.dir, f.rhs)$solution)
}

computeIntersection <- function(a, b, c, d){
  if((a-c-b+d) == 0)
    return(NA)
  return((d-b)/(a-c-b+d))
}

drawLine <- function(x, a, b) {
  return(a*x + b*(1-x))
}

drawMinOrMaxLine <- function(matrix, minmax){
  intersections <- getAllIntersections(matrix)
  values = getMinOrMaxFunctionsValueForIntersections(matrix, intersections, minmax)
  offsetDataFrame <- data.frame(x=values[,1], y=values[,2], xend=values[,3], yend=values[,4])
  return(offsetDataFrame)
}

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

getMinOrMaxFunctionsValueForIntersections <- function(matrix, intersections, minmax){
  functionValuesForIntersection <- c()
  minOrMaxFunctionsValue <- matrix(NA, ncol = 4, nrow = length(intersections))
  for(i in 1:length(intersections)){
    for(j in 1:nrow(matrix)){
      functionValuesForIntersection[j] <- drawLine(intersections[i], matrix[j,1], matrix[j,2])
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

plotSolution <- function(matrix){
  print(matrix)
  if(isMatrixOfRightDimention(matrix)){
    stop("Enter 2xn or mx2 matrix!")
  }

  if(ncol(matrix) > nrow(matrix)){
    minOrMax <- 'max'
    solveLP = solveLinearProgram(matrix, minOrMax)
    matrix = t(matrix)
  } else {
    minOrMax <- 'min'
    solveLP = solveLinearProgram(matrix, minOrMax)
  }
  p <- ggplot(data.frame(x=c(0,1)), mapping = aes(x = x)) + theme_classic()
  p <- p + scale_y_continuous(sec.axis = dup_axis())
  p <- p + scale_colour_manual("Strategies", values=generateColors(nrow(matrix)))
  for(i in 1:nrow(matrix)){
    label <- generateFunctionName(i, matrix)
    p <- p + stat_function(
                    fun=drawLine,
                    args = list(matrix[i,1], matrix[i,2]),
                    geom="line",
                    aes_(color=label)
                    )
  }
  if(minOrMax == 'min') {
    p <- p + ggplot2::geom_segment(
                    data=drawMinOrMaxLine(matrix, minOrMax),
                    aes(x = x, y = y, xend = xend, yend = yend, colour = "max payment")
                    )
  } else {
    p <- p + ggplot2::geom_segment(
                    data=drawMinOrMaxLine(matrix, minOrMax),
                    aes(x = x, y = y, xend = xend, yend = yend, colour = "min payoff")
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

isMatrixOfRightDimention <- function(matrix) {
  return(!is.matrix(matrix) || (dim(matrix)[1] > 2 & dim(matrix)[2] > 2))
}

generateColors <- function(numOfRows) {
  hues = seq(15, 375, length = numOfRows + 1)
  colors <- hcl(h = hues, l = 65, c = 100)[1:numOfRows]
  colors[numOfRows+1] <- "#000000"
  colors <- unlist(strsplit(colors, " "))
}

generateFunctionName <- function(i, matrix){
  return(paste("e",i,"=",matrix[i,1],"*x +",matrix[i,2],"*(1-x)" ))
}
