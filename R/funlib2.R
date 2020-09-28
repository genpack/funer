# x1*x2
mul2 = FUNCTION(
  name = 'mul2',
  type = 'multiplier of two inputs',
  inputs = list(x1 = 0, x2 = 0),
  rule.output = function(inputs, params, objects){inputs$x1*inputs$x2} ,
  rule.gradient = function(inputs, params, objects, wrt){
    switch(wrt, 
           'x1' = {inputs$x2},
           'x2' = {inputs$x1})
  },
  local_dependencies = list(
    'output' = c('x1', 'x2'),
    'x1'     = 'x2',
    'x2'     = 'x1'
  )
)

# x1 + x2
add2 = FUNCTION(
  name = 'add2',
  type = 'adder of two inputs',
  inputs = list(x1 = 0, x2 = 0),
  rule.output = function(inputs, params, objects){inputs$x1 + inputs$x2} ,
  rule.gradient = function(inputs, params, objects, wrt){
    switch(wrt, 
           'x1' = {1},
           'x2' = {1})
  },
  local_dependencies = list(
    'output' = c('x1', 'x2'),
    'x1'     = c(),
    'x2'     = c()
  )
)

# a0 + a1*x1 + a2*x2
lincomb2 = FUNCTION(
  name = 'lincomb2',
  type = 'linear combination of two inputs',
  inputs = list(x1 = 0, x2 = 0),
  params = list(a0 = 0, a1 = 0, a2 = 0),
  rule.output = function(inputs, params, objects){params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2} ,
  rule.gradient = function(inputs, params, objects, wrt){
    switch(wrt, 
           'x1' = {params$a1},
           'x2' = {params$a2},
           'a0' = {1.0}, 'a1' = {inputs$x1}, 'a2' = {inputs$x2})
  },
  local_dependencies = list(
    output = c('x1', 'x2', 'a0', 'a1', 'a2'),
    x1 = 'a1', x2 = 'a2', a0 = c(), a1 = 'x1', a2 = 'x2'
  )
)

# hyperbola2
# f(x1, x2) = a1*x1^2 + b1*x1 + c0 + a2*x2^2 + b2*x2 + d0*x1*x2 : version 2
# f(x1, x2) = (a0 + a1*x1 + a2*x2)*(b0 + b1*x1 + b2*x2): version 1
hyperbola2 = FUNCTION(
  name = 'hyperbola2',
  type = 'hyperbola with two inputs',
  inputs = list(x1 = 0, x2 = 0),
  params = list(a0 = 0, a1 = 0, a2 = 0, b0 = 0, b1 = 0, b2 = 0),
  rule.output = function(inputs, params, objects){(params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2)*(params$b0 + params$b1*inputs$x1 + params$b2*inputs$x2)},
  rule.gradient = function(inputs, params, objects, wrt){
    switch(wrt, 
           'x1' = {params$a1*(params$b0 + params$b1*inputs$x1 + params$b2*inputs$x2) + params$b1*(params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2)},
           'x2' = {params$a2*(params$b0 + params$b1*inputs$x1 + params$b2*inputs$x2) + params$b2*(params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2)},
           'a0' = {params$b0 + params$b1*inputs$x1 + params$b2*inputs$x2}, 
           'a1' = {inputs$x1*(params$b0 + params$b1*inputs$x1 + params$b2*inputs$x2)}, 
           'a2' = {inputs$x2*(params$b0 + params$b1*inputs$x1 + params$b2*inputs$x2)},
           'b0' = {params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2}, 
           'b1' = {inputs$x1*(params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2)}, 
           'b2' = {inputs$x2*(params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2)}
    )
  },
  local_dependencies = list(
    output = c('x1', 'x2', 'a0', 'a1', 'a2', 'b0', 'b1', 'b2'),
    x1     = c('x1', 'x2', 'a0', 'a1', 'a2', 'b0', 'b1', 'b2'), x2 = c('x1', 'x2', 'a0', 'a1', 'a2', 'b0', 'b1', 'b2'), 
    a0     = c('x1', 'x2', 'b0', 'b1', 'b2'), a1 = c('x1', 'x2', 'b0', 'b1', 'b2'), a2 = c('x1', 'x2', 'b0', 'b1', 'b2'),
    b0     = c('x1', 'x2', 'a0', 'a1', 'a2'), b1 = c('x1', 'x2', 'a0', 'a1', 'a2'), b2 = c('x1', 'x2', 'a0', 'a1', 'a2')
  )
)



# log denotes logistic not Iogarlthw
logloss = FUNCTION(
  name = 'logloss',
  type = 'logloss',
  inputs = list(y = 'y_true', x = 'y_pred'),
                rule.output = function(inputs, params, objects){inputs$y*log(1 + exp(- inputs$x)) + (1 + inputs$y)*log(1 + exp(inputs$x))} ,
                rule.gradient = function(inputs, params, objects, wrt){
                  switch(wrt, 
                         'y' = {log(1 + exp(- inputs$x)) - log(1 + exp(inputs$x))},
                         'x' = {- inputs$y/(1 + exp(inputs$x)) + (1 - inputs$y)/(1 + exp(- inputs$x))})
                }
)

binbin2 = FUNCTION(
  name = 'binbin2',
  type = 'binary binner of two inputs',
  inputs = list(x1 = rnorm(100), x2 = rnorm(100)), 
  params = list(c1 = 0, c2 = 0, a00 = 0, a01 = 0, a10 = 0, a11 = 0),
  rule.output = function(inputs, params, objects){
    q1 = inputs$x1 >= params$c1
    q2 = inputs$x2 >= params$c2
    ifelse(q1, ifelse(q2, params$a11, params$a10), ifelse(q2, params$a01, params$a00))
    # (!q1 & !q2)*params$a00 + (!q1 & q2)*params$a01 + (q1 & !q2)*params$a10 + (q1 & q2)*params$a11
  },
  rule.gradient = function(inputs, params, objects, wrt){
    q1 = inputs$x1 >= params$c1
    q2 = inputs$x2 >= params$c2
    switch(wrt,
          'a00' = {as.numeric(!q1 & !q2)},
          'a01' = {as.numeric(!q1 & q2)},
          'a10' = {as.numeric(q1 & !q2)},
          'a11' = {as.numeric(q1 & q2)})
  },
  local_dependencies = list(
    output = c('x1', 'x2', 'c1', 'c2', 'a00', 'a01', 'a10', 'a11'), 
    x1  = c('x1', 'x2', 'c1', 'c2', 'a00', 'a01', 'a10', 'a11'),
    a00 = c('c1', 'c2', 'x1', 'x2'), 
    a01 = c('c1', 'c2', 'x1', 'x2'), 
    a10 = c('c1', 'c2', 'x1', 'x2'), 
    a11 = c('c1', 'c2', 'x1', 'x2')))

######################################################################
logistic = FUNCTION.AGG(
  name = 'logistic',
  type = 'logistic',
  inputs = list(y = 'y_true', x = 'y_pred'),
  rule.output = function(inputs, params, objects){
    ifelse(inputs$y, log(1 + exp(- inputs$x)), log(1 + exp(inputs$x)))
    # inputs$y*log(1 + exp(- inputs$x)) + (1 - inputs$y)*log(1 + exp(inputs$x))
  },
  rule.gradient = function(inputs, params, objects, wrt){
    expx  = exp(inputs$x)
    exp_x = exp(- inputs$x)
    switch(wrt, 
           'x' = {
             ifelse(inputs$y, 1.0/(1 + exp_x) - 1.0, 1.0 - 1.0/(1 + expx))
             
             # ifelse(inputs$y, - exp_x/(1 + exp_x), expx/(1 + expx))
             # - inputs$y*exp_x/(1 + exp_x) + (1 - inputs$y)*expx/(1 + expx)
            },
           'y' = {log(1 + exp_x) - log(1 + expx)}
    )},
  local_dependencies = list(
    'output' = c('x', 'y'),
    'x'      = c('x', 'y'),
    'y'      = 'x'
  )
)

# sum of squared errors
loss_sse = FUNCTION.AGG(
  name = 'loss_sse',
  type = 'loss_sse',
  inputs = list(y = 'y_true', x = 'y_pred'),
  rule.output = function(inputs, params, objects){
    (inputs$x - inputs$y)^2
  },
  rule.gradient = function(inputs, params, objects, wrt){
    switch(wrt, 
           'x' = {2*(inputs$x - inputs$y)},
           'y' = {2*(inputs$y - inputs$x)}
    )},
  local_dependencies = list(
    'output' = c('x', 'y'),
    'x'      = c('x', 'y'),
    'y'      = c('x', 'y')
  )
)

# (x - y - z)^2
loss_sse_gb = FUNCTION.AGG(
  name = 'loss_sse_gb',
  type = 'loss_sse_gb',
  inputs = list(y = 'y_true', x = 'y_pred', z = 'y_hat'),
  rule.output = function(inputs, params, objects){
    (inputs$x + inputs$z - inputs$y)^2
  },
  rule.gradient = function(inputs, params, objects, wrt){
    switch(wrt, 
           'x' = {2*(inputs$x + inputs$z - inputs$y)},
           'y' = {2*(inputs$y - inputs$x - inputs$z)},
           'z' = {2*(inputs$x + inputs$z - inputs$y)}
    )},
  local_dependencies = list(
    'output' = c('x', 'y', 'z'),
    'x'      = c('x', 'y', 'z'),
    'y'      = c('x', 'y', 'z'),
    'z'      = c('x', 'y', 'z')
  )
)


logistic_gb = FUNCTION.AGG(
  name = 'logistic_gb',
  type = 'logistic_gb',
  inputs = list(y = 'y_true', x = 'y_pred', z = 'y_hat'),
  rule.output = function(inputs, params, objects){
    ifelse(inputs$y, log(1 + exp(- inputs$x - inputs$z)), log(1 + exp(inputs$x + inputs$z)))
    # inputs$y*log(1 + exp(- inputs$x - inputs$z)) + (1 - inputs$y)*log(1 + exp(inputs$x + inputs$z))
  },
  rule.gradient = function(inputs, params, objects, wrt){
    expx  = exp(inputs$x + inputs$z)
    exp_x = exp(- inputs$x - inputs$z)
    switch(wrt, 
           'x' = {
             ifelse(inputs$y, 1.0/(1 + exp_x) - 1.0, 1.0 - 1.0/(1 + expx))
             # - inputs$y*exp_x/(1 + exp_x) + (1 - inputs$y)*expx/(1 + expx)
            },
           'z' = {
             ifelse(inputs$y, 1.0/(1 + exp_x) - 1.0, 1.0 - 1.0/(1 + expx))
             # ifelse(inputs$y, - exp_x/(1 + exp_x), expx/(1 + expx))
             # - inputs$y*exp_x/(1 + exp_x) + (1 - inputs$y)*expx/(1 + expx)
            },
           'y' = {log(1 + exp_x) - log(1 + expx)}
    )},
  local_dependencies = list(
    'output' = c('x', 'y', 'z'),
    'x'      = c('x', 'y', 'z'),
    'z'      = c('x', 'y', 'z'),
    'y'      = c('x', 'z')
  )
)

aurc_neg = FUNCTION.AGG(
  name = 'aurc_neg',
  type = 'aurc_neg',
  inputs = list(y = 'y_true', x = 'prob'),
  rule.output = function(inputs, params, objects){
    return(1 - AUC::auc(AUC::roc(inputs$x, inputs$y %>% as.factor)))
  }, 
  local_dependencies = list(
    'output' = c('x', 'y'),
    'x'      = c('x', 'y'),
    'y'      = c('x', 'y')
  )
)

lift_2 = FUNCTION.AGG(
  name = 'lift_2',
  type = 'lift_2',
  inputs = list(y = 'y_true', x = 'prob'),
  rule.output = function(inputs, params, objects){
    return(correlation(x = inputs$x, y = inputs$y, metric = 'lift', lift_ratio = 0.02))
  }, 
  local_dependencies = list(
    'output' = c('x', 'y'),
    'x'      = c('x', 'y'),
    'y'      = c('x', 'y')
  )
)





