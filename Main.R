source("Utils.R")


g <- ReadBNFFile("grammar.bnf")
expression <- (g[[1]][1])


rules_applications <- 0
solution <- FALSE
repeat {
  resultado <- iterate_rules(expression, g)
  rules_applications <- rules_applications + 1 
  expression <- resultado[[1]]
  if ((resultado[[2]] == TRUE) || rules_applications > 100) {
    solution <- resultado[[3]]
    break
  }
} 



cat("Result ", solution , " ", unlist(expression))
