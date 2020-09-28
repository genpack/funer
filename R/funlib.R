
# log(abs(x))
logabs = FUNCTION(
  name = 'logabs',
  type = 'natural logarithm of absolute value of the input',
  inputs = list(x = 1.0),
  rule.output = function(inputs, params, objects){
    inputs$x %<>% abs     
    mask = inputs$x < .Machine$double.eps
    inputs$x[mask] <- .Machine$double.eps
    log(inputs$x)
  },
  
  rule.gradient = function(inputs, params, objects, wrt){
    sgnx = sign(inputs$x)
    inputs$x %<>% abs
    mask = inputs$x < .Machine$double.eps
    switch(wrt, 
             'x' = {out = sgnx/inputs$x; out[mask] <- 0})
    return(out)
  },
  local_dependencies = list(output = 'x', x = 'x')
)

# logit
# f(x) =  log(x/(1-x))
logit = FUNCTION(
  name = 'logit',
  type = 'logit of the input',
  inputs = list(x = 1.0),
  rule.output = function(inputs, params, objects){
    inputs$x[inputs$x < .Machine$double.eps] <- .Machine$double.eps
    inputs$x[inputs$x > 1.0 - .Machine$double.eps] <- 1.0 - .Machine$double.eps
    
    log(inputs$x/(1.0 - inputs$x))
  },
  rule.gradient = function(inputs, params, objects, wrt){
    mask = (inputs$x < .Machine$double.eps) | (inputs$x > 1.0 - .Machine$double.eps)
    switch(wrt, 
           'x' = {out = 1.0/(inputs$x*(1.0 - inputs$x)); out[mask] <- 0})
  },
  local_dependencies = list(output = 'x', x = 'x')
)

# x^a:
pwr = FUNCTION(
  name   = 'pwr',
  type   = 'pwr',
  inputs = list(x = 'x'),
  params = list(a = 1),
  rule.output   = function(inputs, params, objects){inputs$x^params$a},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'a' = {log(inputs$x)*(inputs$x^params$a)},
           'x' = {params$a*inputs$x^(params$a - 1)})
  },
  local_dependencies = list(output = c('x', 'a'), x = c('x', 'a'))
)

# (x+b)^a:
pwradd = FUNCTION(
  name   = 'pwradd',
  type   = 'pwradd',
  inputs = list(x = 'x'),
  params = list(a = 1, b = 0),
  rule.output   = function(inputs, params, objects){(inputs$x + params$b)^params$a},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'a' = {log(inputs$x + params$b)*((inputs$x + params$b)^params$a)},
           'b' = {params$a*(inputs$x + params$b)^(params$a - 1)},
           'x' = {params$a*(inputs$x + params$b)^(params$a - 1)})
  },
  local_dependencies = list(output = c('x', 'a', 'b'), x = c('x', 'a', 'b'), a = c('x', 'a', 'b'), b = c('x', 'a', 'b'))
)

# poly: a*x^2 + b*x + c
poly_d2 = FUNCTION(
  name   = 'poly_d2',
  type   = 'polynomial of degree 2',
  inputs = list(x = 'x'),
  params = list(a = 0.0, b = 0.0, c = 0.0),
  rule.output   = function(inputs, params, objects){params$a*inputs$x^2 + params$b*inputs$x + params$c},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'x' = {2*params$a*inputs$x + params$b},
           'a' = {inputs$x^2},
           'b' = {inputs$x},
           'c' = {1.0}
    )
  },
  local_dependencies = list(output = c('x', 'a', 'b', 'c'),  x = c('x', 'a', 'b'), a = 'x', b = 'x', c = c())
)



# 1/x:
inv = FUNCTION(
  name   = 'inv',
  type   = 'inv',
  inputs = list(x = 'x'),
  rule.output   = function(inputs, params, objects){1.0/inputs$x},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'x' = {- 1.0/(inputs$x^2)})
  },
  local_dependencies = list(output = 'x', x = 'x')
)

# 1/(x + b):
invadd = FUNCTION(
  name   = 'invadd',
  type   = 'invadd',
  inputs = list(x = 'x'),
  params = list(b = 0),
  rule.output   = function(inputs, params, objects){1.0/(inputs$x+ params$b)},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'x' = {- 1.0/((inputs$x+ params$b)^2)},
           'b' = {- 1.0/((inputs$x+ params$b)^2)})
  },
  local_dependencies = list(output = c('x', 'b'), x = c('x', 'b'), b = c('x', 'b'))
)

# add parameter:
# f(x) = x + b
add = FUNCTION(
  name   = 'add',
  type   = 'add',
  inputs = list(x = 'x'),
  params = list(b = 0),
  rule.output   = function(inputs, params, objects){inputs$x + params$b},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'x' = {1.0},
           'b' = {1.0})
  },
  local_dependencies = list(output = c('x', 'b'), x = c(), b = c())
)

#logadd
# f(x) = log(x + b)
logadd = FUNCTION(
  name   = 'logadd',
  type   = 'logadd',
  inputs = list(x = 'x'),
  params = list(b = 1),
  rule.output   = function(inputs, params, objects){log(inputs$x + params$b)},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'x' = {1.0/(inputs$x + params$b)},
           'b' = {1.0/(inputs$x + params$b)})
  },
  local_dependencies = list(output = c('x', 'b'), x = c('x', 'b'), b = c('x', 'b'))
)

# hyperbolic fraction of polynomials degree 1
# f(x) = (a + b*x)/(1 + d*x)
hyperpoly_d1 = FUNCTION(
  name   = 'hyperpoly_d1',
  type   = 'hyperpoly_d1',
  inputs = list(x = 'x'),
  params = list(a = 0, b = 1, d = 0),
  rule.output   = function(inputs, params, objects){(params$b*inputs$x + params$a)/(params$d*inputs$x + 1.0)},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'x' = {params$b*(1.0 + params$d*inputs$x) - params$d*(params$a + params$b*inputs$x) / (1.0 + params$d*inputs$x)^2},
           'a' = {1.0/(1.0 + params$d*inputs$x)},
           'b' = {inputs$x/(1.0 + params$d*inputs$x)},
           'd' = {-params$d*(params$a + params$b*inputs$x)/((1.0 + params$d*inputs$x)^2)})
  },
  local_dependencies = list(output = c('x', 'a', 'b', 'd'), 
                            'x' = c('x', 'a', 'b', 'd'), 
                            'a' = c('x', 'd'),
                            'b' = c('x', 'd'),
                            'd' = c('x', 'a', 'b', 'd'))
)

exp = FUNCTION(
  name   = 'exp',
  type   = 'exp',
  inputs = list(x = 'x'),
  rule.output   = function(inputs, params, objects){exp(inputs$x)},
  rule.gradient = function(inputs, params, objects, wrt){ 
    switch(wrt,
           'x' = {exp(inputs$x)})
  },
  local_dependencies = list(output = 'x', x = 'x')
)


binbin = FUNCTION(
  name = 'binbin',
  type = 'binary binner of one input',
  inputs = list(x = rnorm(100)), 
  params = list(c = 0, a0 = 0, a1 = 0),
  rule.output = function(inputs, params, objects){
    q = inputs$x >= params$c
    ifelse(q, params$a1, params$a0)
  },
  rule.gradient = function(inputs, params, objects, wrt){
    q = inputs$x >= params$c
    switch(wrt,
           'a0' = {as.numeric(!q)},
           'a1' = {as.numeric(q)})
  },
  local_dependencies = list(
    output = c('x', 'c', 'a0', 'a1'), 
    x  = c('x', 'c', 'a0', 'a1'),
    a0 = c('c', 'x'), 
    a1 = c('c', 'x')))

######################################################################
