options <- list()
options[[1]] <- var_one
options[[2]] <- var_two
options[[3]] <- var_three

Reduce(intersect, options)
length(Reduce(intersect, options))