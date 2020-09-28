estimate_local_dependencies = function(f, wrt){
  i0 = list(); p0 = list(); loc_dep = c()
  for (y in names(f$inputs)) i0[[y]] = runif(1)
  for (y in names(f$params)) p0[[y]] = runif(1)
  if(wrt == 'output'){
    v0 = f$rule.output(inputs = i0, params = p0)
  } else {
    v0 = try(f$rule.gradient(inputs = i0, params = p0, wrt = terms[2]), silent = T)
    if(inherits(v0, 'try-error') | is.empty(v0)){v0 = 1} # Something greater that 0 to add a dependancy if gradient is not known!
  }
  i1 = i0; p1 = p0
  for (y in names(f$inputs)){
    test = runif(5)
    tval = numeric(5)
    for (i in 1:5){
      i1[[y]] = test[i]
      if (wrt == 'output'){
        tval[i] = f$rule.output(inputs = i1, params = p0)
      } else {
        tvali   = try(f$rule.gradient(inputs = i1, params = p0, wrt = terms[2]), silent = T)
        if(inherits(tvali, 'try-error') | is.empty(tvali)){tvali = 1}
        tval[i] = tvali
      }
    }
    if(sum(tval != v0) > 0) {
      loc_dep = c(loc_dep, y)
    }  
    i1[[y]] = i0[[y]]
  }

  for (y in names(f$params)){
    test = runif(5)
    tval = numeric(5)
    for (i in 1:5){
      p1[[y]] = test[i]
      if (wrt == 'output'){
        tval[i] = f$rule.output(inputs = i0, params = p1)
      } else {
        tvali   = try(f$rule.gradient(inputs = i0, params = p1, wrt = terms[2]), silent = T)
        if(inherits(tvali, 'try-error') | is.empty(tvali)) {tvali = 1}
        tval[i] = tvali
      }
    }
    if(sum(tval != v0) > 0) {
      loc_dep = c(loc_dep, y)
    }  
    p1[[y]] = p0[[y]]
  }
  return(loc_dep)  
}

find_faulty_gradient = function(f, wrt, data = NULL){
  f$reset()
  g1 = f$get.gradients(wrt = wrt, data = data)
  g1 = sum(g1[[1]])
  f$reset()
  g2 = extract.gradient(f, wrt = wrt, data = data) %>% sum
  failures = c()
  if(!equal(g1, g2, tolerance = 0.01)){
    for(inp in names(f$inputs)){
      inpc = paste(f$name, inp, sep = '.')
      if(inherits(f$inputs[[inp]], 'FUNCTION')){
        if(inpc %in% f$get.dependencies(wrt = wrt)){
          failures = c(failures, find_faulty_gradient(f$inputs[[inp]], wrt = wrt, data = data))
        }
      }
    }
    if(length(failures) == 0){
      failures = c(failures, f$name)
    }
  }
  return(failures)
}