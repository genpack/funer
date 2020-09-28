# one dimensional equation solver using simple newtown algorithm:
eqSolver1d = function(fun, grad, x0, maxit = 10, epsilon = 0.0001){
  x    = x0
  i    = 2
  fval = fun(x)
  fpmx = grad(x)
  if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  while((i < maxit) & (abs(fval) > epsilon) & (abs(fpmx) > epsilon)){
    x = x - fval/fpmx
    fval = fun(x)
    fpmx = grad(x)
    if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  }
  return(x)
}


# finds k to minimize ||m*x^k + h - y|| given x,y as vectors and m, h as scalars
findk = function(x, y, m, h, k0 = 1){
  logx  = log(x)
  keep  = !is.na(logx)
  x     = x[keep]
  y     = y[keep]
  logx  = logx[keep]
  log2x = logx^2
  f = function(k) {
    xk = x^k
    sum(logx*xk*(m*xk + h - y))
  }
  g = function(k) {
    xk  = x^k
    x2k = xk^2
    sum(2*m*log2x*x2k + (h - y)*log2x*xk)
  }
  eqSolver1d(f, g, k0, maxit = 20)
}


funsolve.single = function(f, parameter, epochs = 10, epsilon = 0.0001, data = NULL){
  i    = 2
  fval = f$get.output.agg(data = data)
  fpmx = f$get.gradients.agg(wrt = parameter) %>% unlist
  if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  while((i < epochs) & (abs(fval) > epsilon) & (abs(fpmx) > epsilon)){
    x = f$get.param(parameter) %>% unlist
    x = x - fval/fpmx
    f$set.param(x)
    fval = f$get.output.agg(data = data)
    fpmx = f$get.gradients.agg(wrt = parameter) %>% unlist
    if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  }
  return(list(parameter = f$get.param(parameter), fval = f$get.output.agg(data = data), gradient = f$get.gradients.agg(wrt = parameter) %>% unlist))
}


funsolve = function(f, parameters, epochs = 10, epsilon = 0.0001, data = NULL){
  i    = 2
  fval = f$get.output.agg(data = data)
  fpmx = f$get.gradients.agg(wrt = parameters) %>% unlist
  if ((sum(is.na(fpmx)) > 0) | is.na(fval)){stop('Out of domain!')}
  while((i < epochs) & (abs(fval) > epsilon) & (sum(abs(fpmx)) > epsilon)){
    x = f$get.param(parameters) %>% unlist
    x = x - fval*fpmx/sum(fpmx^2)
    f$set.param(x)
    fval = f$get.output.agg(data = data)
    fpmx = f$get.gradients.agg(wrt = parameters) %>% unlist
    if (is.na(fval) | (sum(is.na(fpmx)) > 0)){stop('Out of domain!')}
  }
  return(list(parameters = f$get.param(parameters), fval = f$get.output.agg(data = data), gradients = f$get.gradients.agg(wrt = parameters) %>% unlist))
}

minimize.walk = function(f, parameters = f$list.parameters(), data = NULL,
                         gain = 2, initial_step_size = 0.1, min_step_size = 0.001, min_reduction_ratio = 0.001, silent = F){
  if(!silent){
    cat('\n', 'WALKER is working:', '\n')
  }
  maxit = as.integer(-2*log(min_reduction_ratio)*length(parameters)/log(gain))
  pvec  = f$get.param(parameters) %>% unlist
  fval  = f$get.output.agg(data = data)
  bmark = fval; keep = pvec
  if(is.na(fval)){return(NULL)}
  dirs = rep(1, length(parameters)) %>% {names(.) <- parameters;.}
  step = rep(initial_step_size, length(parameters)) %>% {names(.) <- parameters;.}
  cnt = 0
  while((sum(step > min_step_size) > 0) & (cnt < maxit)){
    for(pm in parameters){
      pvec[pm] = pvec[pm] + dirs[pm]*step[pm]
      f$set.param(pvec[pm]); cnt = cnt + 1
      fval_fwd = f$get.output.agg(data = data)
      pass_fwd = !is.na(fval_fwd) & (fval_fwd < Inf) & (fval_fwd > -Inf)
      if(pass_fwd) pass_fwd = (fval - fval_fwd > min_reduction_ratio*fval)
      if (pass_fwd){
        # change is successful and remains. step size is doubled:
        if(!silent){
          cat('\n', 'Loss reduced from ', fval, ' to ', fval_fwd)
        }
        step[pm] = gain*step[pm]
        fval     = fval_fwd
      } else {
        if(!silent){cat('!')}
        pvec[pm] = pvec[pm] - gain*dirs[pm]*step[pm]
        f$set.param(pvec[pm]); cnt = cnt + 1
        fval_inv = f$get.output.agg(data = data)
        pass_inv = !is.na(fval_inv) & (fval_inv < Inf) & (fval_inv > -Inf)
        if(pass_inv) pass_inv = (fval - fval_inv > min_reduction_ratio*fval)
        if(pass_inv){
          # change is successful and remains. direction must change and step size doubled:
          if(!silent){
            cat('\n', 'Loss reduced from ', fval, ' to ', fval_inv)
          }
          step[pm] = gain*step[pm]
          dirs[pm] = - dirs[pm]
          fval     = fval_inv
        } else {
          if(!silent){cat('!')}
          pass_fwd = !is.na(fval_fwd) & (fval_fwd < Inf) & (fval_fwd > -Inf)
          pass_inv = !is.na(fval_inv) & (fval_inv < Inf) & (fval_inv > -Inf)
          if(pass_fwd) pass_fwd = (abs(fval_fwd - fval) < min_reduction_ratio*fval)
          if(pass_inv) pass_inv = (abs(fval_inv - fval) < min_reduction_ratio*fval)
          if(pass_fwd & pass_inv){
            step[pm] = step[pm]/gain
          } else {
            # Both directions increase the output or don't change it. Return to original value. Reduce step size.
            pvec[pm] = pvec[pm] + dirs[pm]*step[pm]
            f$set.param(pvec[pm]); cnt = cnt + 1
            step[pm] = step[pm]/gain
            pass_fwd = !is.na(fval_fwd) & (fval_fwd < Inf) & (fval_fwd > -Inf)
            pass_inv = !is.na(fval_inv) & (fval_inv < Inf) & (fval_inv > -Inf)
            if(pass_fwd & pass_inv) pass_fwd = (fval_fwd > fval_inv)
            if(pass_fwd){
              # The increasing impact of change of parameter in inverse direction is less than forward, Change direction.
              dirs[pm] = - dirs[pm]
            }
          }
        }
      }
    }
  }
  if(f$get.output.agg(data = data) > bmark){
    f$set.param(keep)
    # Temporary
    assert(f$get.output.agg(data = data) == bmark)
  }
}

minimize.ccd = function(f, parameters = f$list.parameters(), data = NULL, search_range = c(-10, 10), breaks = 5, min_reduction_ratio = 0.001, silent = F){
  keep  = f$get.param(parameters)
  bmark = f$get.output.agg(data = data)
  if(!silent){
    cat('\n', 'CCD is working:', '\n')
  }
  for (pm in parameters){
    val  = f$get.param(pm) %>% unlist
    fval = f$get.output.agg(data = data)
    minimize.search(f, parameter = pm, breaks = breaks, min = search_range[1], max = search_range[2], data = data)
    if((!silent) & (fval - f$get.output.agg(data = data) > min_reduction_ratio*fval)){
      cat('\n', 'Loss reduced to ', f$get.output.agg(data = data), ' by changing ', pm, ' from ', val, ' to ', f$get.param(pm) %>% unlist)
    } else {if(!silent){cat('!')}}
  }
  if(bmark < f$get.output.agg(data = data)){
    f$set.param(keep)
  }
}


minimize.search = function(f, parameter, min = 0, max = 1, breaks = 10, data = NULL, benchmark = NULL){
  if(is.null(benchmark)){
    benchmark = list(x = f$get.param(parameter), y = f$get.output.agg(data = data))
  }
  h = max - min; v = numeric();
  u = min + (sequence(breaks + 1) - 1)*h/breaks
  for(i in u){
    ui = i; names(ui) <- parameter
    f$set.param(ui)
    v = c(v, f$get.output.agg(data = data))
  }
  minind = order(v)[1]
  if(h < 0.01){
    if(v[minind] < benchmark$y - 0.01){
      ui = u[minind]; names(ui) <- parameter
      f$set.param(ui)
    } else {
      if(!identical(benchmark$x, f$get.param(parameter))){
        f$set.param(benchmark$x)
      }
    }
  } else {
  minimize.search(f, parameter, u[max(1, minind - 1)], u[min(breaks + 1, minind + 1)], breaks = breaks, data = data, benchmark = benchmark)
  }
}


step_forward = function(f, parameters, direction, data = NULL, initial_step_size = 0.1, min_step_size = 100000*.Machine$double.eps, gain_success = 2, gain_fail = 10, min_reduction_ratio = .Machine$double.eps, silent = T){
  f$reset()
  success = F
  fval = f$get.output.agg(data = data)
  pval = f$get.param(parameters) %>% unlist
  step = initial_step_size
  while(step > min_step_size){
    pval = pval + step*direction
    f$set.param(pval)
    fval_fwd = f$get.output.agg(data = data)
    pass_fwd = !is.na(fval_fwd) & (fval_fwd < Inf)
    if(pass_fwd) pass_fwd = (fval_fwd/fval) < (1.0 - min_reduction_ratio)

    if(pass_fwd){
      # change is successful and remains. step size is doubled:
      if(!silent){
        cat('\n', 'Loss reduced from ', fval, ' to ', fval_fwd, '. Step size: ', step)
      }
      step    = gain_success*step
      fval    = fval_fwd
      success = T
    } else {
      if(!silent) cat('!')
      pval = pval - step*direction
      f$set.param(pval)
      step = step/gain_fail
    }

  }
  return(success)
}

minimize.sgd_walk = function(f, parameters, data = NULL, ...){
  success = T
  while(success){
    stepdir = f$get.gradients.agg(wrt = parameters, data = data) %>% unlist %>% vect.normalize %>% {-1.0*.}
    success = step_forward(f, parameters = parameters, direction = stepdir, data = data, ...)
  }
}


