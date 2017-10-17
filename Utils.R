ReadBNFFile <- function(filename) {
  # reads a bnf grammar file and returns a list structure
  
  # read the file line by line
  con=file(filename, open="r")
  lines=readLines(con) 
  close(con)
  
  # parse the lines
  rule_list = list()
  for (l in lines) {
    l = trim_space(l)
    gram_line = strsplit(l, "::=")[[1]]
    if (length(gram_line) > 1) {
      # split and trim rules
      rules = strsplit(trim_space(gram_line[2]), "|", fixed=TRUE)
      for (j in seq_along(rules[[1]])) {
        rules[[1]][[j]] = trim_space(rules[[1]][[j]])
      }

      # add rules to list
      i = length(rule_list) + 1
      rule_list[[i]] = list()
      rule_list[[i]][[1]] = trim_space(gram_line[[1]])
      rule_list[[i]][[2]] = as.list(rules[[1]])
      print(rule_list)
    }
  }
  return (rule_list)
}


trim_brackets <- function (x) gsub("^<+|>+$", "", x)
trim_space <- function (x) gsub("^\\s+|\\s+$", "", x)


findRule <- function(grammar, non_terminal) {
  
  idx <- grep(non_terminal,lapply(grammar, function(x) x[[1]]))
  if (length(idx) == 0) return (NULL)
  grammar[idx][[1]][[2]]
}


apply_rule <- function (expression, rule, consequent, first_NT) {
  expr <- list()
  idx <- grep(first_NT, expression)[1]
  
  res <- as.list(strsplit(trim_space(rule[[consequent]]), " ", fixed=TRUE)[[1]])
  
  if (idx > 1)
    expr <- append(expr,expression[1:(idx-1)])
  expr <- append(expr, res)
  if (idx < length(expression))
    expr <- append(expr,expression[(idx+1):length(expression)])
  
  ## cat("Original expression ", unlist(expression), "\n")
  ## cat(paste("Applying rule ", first_NT, " ::= ", unlist(rule[[consequent]])), "\n")
  ## cat("Obtained expression ", unlist(expr), "\n\n")
  expr
}


iterate_rules <- function(expression, g) {
  stop <- TRUE
  solution <- FALSE
  first_NT <- expression[grep ("<[[:alnum:]]+>", expression)[1]]
  if (is.null(first_NT[[1]])) {
    ## cat(" ---- non_terminal NOT found ---- \n")
    solution <- TRUE
  } else {
    rule <- findRule(g, first_NT)
    if (is.null(rule)) {
      ## cat(paste(" ---- not rule found for expression ",unlist(expression),"\n"))
    } else {
      expression <- apply_rule(expression, rule, sample(1:length(rule), 1), first_NT)
      stop = FALSE
    }
  }
  list(expression, stop, solution)
}

