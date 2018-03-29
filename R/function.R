game_function <- function() {
  A = matrix(c(-3,1,2,0,5,2,3,2,2,-3,1,-3,1,0,-2,2), nrow = 4, byrow = TRUE);
  #A = matrix(c(2,3,4,-3,4,-5,3,-5,6), nrow = 3, byrow = TRUE);
  #A = matrix(c(3,3,5,1,-1,7,0,-2,4), nrow = 3, byrow = TRUE);
  #A = matrix(c(4,3,5,1,2,-1,0,-2,0,-1,-4,1,4,3,5,-3,0,-1,0,-2,3,2,-7,3,8), nrow = 5, byrow = TRUE);
  #A = matrix(c(4,1,-3,3,2,5,0,1,6), nrow = 3, byrow = TRUE);
  #A = matrix(c(3,1,1,0,0,1,2,0,1,0,2,1,3,1,2,2), nrow = 4, byrow = TRUE);
  #A = matrix(c(0,-1,1,1,0,-1,-1,1,0), nrow = 3, byrow = TRUE);#Rock/Paper/Scissors
  #A = matrix(c(2,1,3,3,2,1,1,3,2), nrow = 3, byrow = TRUE);#Rock/Paper/Scissors
  #A = matrix(c(-2,1,2,2,-1,0,1,0,-2), nrow = 3, byrow = TRUE);#https://www.usna.edu/Users/math/dphillip/sa305.s13/uhan-game-theory.pdf
  #A = matrix(c(-1,1,1,-1), nrow = 2, byrow = TRUE);
  #A = matrix(c(2,3,4,-3,4,-5,3,-5,6), nrow = 3, byrow = TRUE);
  #A = matrix(c(0,1,1,1,1,-1,1,-2,1,1,-1,1,1,2,-1,1), nrow = 4, byrow = TRUE);
  #A = matrix(c(-1,1,-2,1,-1,1,2,-1,1), nrow = 3, byrow = TRUE);
  A = matrix(c(1,-1,-2,-1,1,1,2,-1,-1), nrow = 3, byrow = TRUE);
  #A = "asdfsdf"
  #A = matrix(c(10,2,4,1,2,10,8,12), ncol = 4, byrow = TRUE);
  #A = matrix(c(10,2,2,10,4,8,1,12), ncol = 2, byrow = TRUE);
  #A = matrix(c(1,2,3,3,6,2,6,1,3,3,3,1,3,6,2,3,3,6,2,1,6,3,2,1,3), nrow = 5, byrow = TRUE);
  #A = matrix(c(0,5,-2,-3,0,4,6,-4,0), nrow = 3, byrow = TRUE);

  A = matrix(c(5,3,5,3,2,1,-1,-2,4,3,5,3), nrow = 3, byrow = TRUE); #'4 Sattelpunkte'
  #A = matrix(c(5,3,5,3,2,1,-1,-2,4,3,5,3), nrow = 3, byrow = TRUE); #'4 Sattelpunkte'

  #A = matrix(c(5,3,5,3,2,1,-1,-2,4,3,5,3), nrow = 1, byrow = TRUE);

  #A = matrix(c(10,2,4,1,2,10,8,12), ncol = 4, byrow = TRUE);
  A = matrix(c(10,2,2,10,4,8,1,12), ncol = 2, byrow = TRUE);

  print(A)
  if(!is.matrix(A)){
    stop("Please enter a matrix!")
    #Vektoren auch behandeln
  }
  out <- list()



  minRow = getMaxOfRowMin(A)
  maxCol = getMinOfColMax(A)
  if(minRow == maxCol){
    out['saddlePoints'] <- list(saddlePoints = generateMatrixFromSaddleVector(getSaddlePointsOfGame(A, maxCol)))
    out['value'] <- list(value = minRow)
  } else {
    solutionA = solveLinearProgram(A, "max");
    out['value'] <- list(value = solutionA[1])
    out['saddlePoints'] <- list(saddlePoints = NULL)
    out['strategyA'] <- list(strategyA = solutionA[-1])
    solutionB = solveLinearProgram(A, "min");
    out['strategyB'] <- list(strategyB = solutionB[-1])
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

solveLinearProgram <- function(gameMatrix, minmax){
  ncol = ncol(gameMatrix)
  nrow = nrow(gameMatrix)

  if(minmax == "max") {
    operator = "<="
    gameMatrix = t(-gameMatrix)
  } else {
    operator = ">="
    gameMatrix = -gameMatrix
  }

  gameMatrix <- rbind(c(rep(1, ncol(gameMatrix))), gameMatrix)
  gameMatrix <- cbind(c(0,rep(1, nrow(gameMatrix)-1 )), gameMatrix)
  f.obj <- c(1,rep(0, ncol(gameMatrix)-1 ))
  f.con <- gameMatrix
  f.dir <- c("==",rep(operator, nrow(gameMatrix)-1 ))
  f.rhs <- c(1,rep(0, nrow(gameMatrix)-1 ))

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

drawResLine <- function(matrix){
  resData <- c()
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
  intersections <- sort(intersections)
  functionValue <- c()
  minFunctionValue <- c()
  for(i in 1:length(intersections)){
    for(j in 1:nrow(matrix)){
      functionValue[j] <- drawLine(intersections[i], matrix[j,1], matrix[j,2])
    }
    minFunctionValue[i] <- min(functionValue)
  }
  return(minFunctionValue)
}



gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



plotSolution <- function(matrix){
  if(!is.matrix(matrix) || (dim(matrix)[1] > 2 & dim(matrix)[2] > 2)){
    stop("Enter 2xn or mx2 matrix!")
  }



  if(ncol(matrix) > nrow(matrix)){
    solveLP = solveLinearProgram(matrix, 'max')
    matrix = t(matrix)
    minOrMax <- 'min'
  } else {
    solveLP = solveLinearProgram(matrix, 'min')
    minOrMax <- 'max'
  }

  colorPalette = unlist(strsplit(gg_color_hue(nrow(matrix)), " "))
  colorPalette[nrow(matrix)+1] <- "#000000"

  p <- ggplot(data.frame(x=c(0,1)), mapping = aes(x = x)) + theme_classic()

  p <- p + scale_y_continuous(sec.axis = dup_axis())
  for(i in 1:nrow(matrix)){
    label <- paste("e",i,"=",matrix[i,1],"*x +",matrix[i,2],"*(1-x)" )
    p <- p + stat_function(fun=drawLine, args = list(matrix[i,1], matrix[i,2]), geom="line", aes_(color=label))
  }
  if(ncol(matrix) > nrow(matrix)){
    for (y in drawResLine(matrix)) {
      print(y)
      #p <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df)
    }

    #p <- p + ggplot2::stat_function(fun = drawResLine, args = list(matrix, minOrMax), geom="line", aes_(color="max payments"))
  } else {
    redrawValues = drawResLine(matrix)
    for (i in 1:length(redrawValues)) {
      p <- p + geom_segment(aes(x = 0, y = 0, xend = 1, yend = redrawValues[i], colour = "segment"))
    }
    # p <- p + ggplot2::stat_function(fun = drawResLine, args = list(matrix, minOrMax), geom="line", aes_(color="min payoff"))
  }

  p <- p + scale_colour_manual("Strategies", values=colorPalette)
  p <- p + ggplot2::geom_hline(yintercept=solveLP[1], linetype="dashed", aes(test="Game value2"))
  p <- p + ggplot2::geom_vline(xintercept=solveLP[2], linetype="dashed", aes(test="Game value2"))
  p <- p + ggplot2::labs(y = "", x = "")
  return(p)
}

text <- function() {
  return('asdffs')
}

plot <- function() {
  matrix = matrix(c(10,2,4,1,2,10,8,12), ncol = 4, byrow = TRUE);
  #matrix = matrix(c(10,2,2,10,4,8,1,12), ncol = 2, byrow = TRUE);
  #matrix = matrix(c(1,4,3,0), ncol = 2, byrow = TRUE);
  plotSolution(matrix)
}
