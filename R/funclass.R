# valid.optimizers = c('stats.optim', 'funer.walker', 'funer.ccd')

#' @export
FUNCTION = setRefClass('FUNCTION',
  fields = list(
    name            = "character",
    type            = "character",
    objects         = "list",
    values          = "list",
    inputs          = "list",
    params          = "list",
    dependencies    = "list",
    local_dependencies  = "list",
    gradients       = "list",
    rule.output     = "function",
    rule.gradient   = "function"
  ),

  methods = list(
    # argument 'data' is used only if inputs are of character type
    get.inputs = function(labels = names(inputs), data = NULL){
      labels %<>% intersect(names(inputs))
      new_labels = labels %>% setdiff(names(values))
      for (inp in new_labels){
        if(inherits(inputs[[inp]], c('numeric', 'integer'))){
          values[[inp]] <<- inputs[[inp]]
        } else if (inherits(inputs[[inp]], 'FUNCTION')){
          values[[inp]] <<- inputs[[inp]]$get.output(data = data)
        } else if (inherits(inputs[[inp]], 'character')){
          assert(inputs[[inp]] %in% colnames(data), 'data does not exist or input refers to non-existing column!')
          values[[inp]] <<- data[, inputs[[inp]]]
        } else {
          stop("inputs[[" %++% inp %++% "]] must be either a numeric or an object of class FUNCTION!")
        }
      }

      values %>% list.extract(labels)
    },

    get.output = function(data = NULL){
      if(is.null(values$output)){
        # find values of all inputs
        # calculate output value of the function
        values$output <<- rule.output(inputs = get.inputs(data = data), params = params, objects = objects)
      }
      return(values$output)
    },

    # if wrt == 'output' returns list of inputs and parameters to which output depends on
    # oterwise, returns list of variables(inputs or params) to which, the gradient of the function with respect to argument wrt, depends
    get.dependencies = function(wrt = 'output'){
      pass = F
      if (wrt!= 'output'){
        if(grep(pattern = '.', x = wrt, fixed = T) %>% is.empty) {wrt <- name %>% paste(wrt, sep = '.')}
        terms = wrt %>% strsplit(".", fixed = T) %>% unlist
        pass  = terms[[1]] != name
        local_wrt = terms[2]
      } else {local_wrt = wrt}

      if (is.null(dependencies[[wrt]])){
        if(pass){
          for(y in names(inputs)){
            dep = get.dependencies(y)
            if(wrt %in% dep){
              if(inherits(inputs[[y]], 'FUNCTION')){dependencies[[wrt]] <<- c(dependencies[[wrt]], dep, inputs[[y]]$get.dependencies()) %>% unique}
            }
          }
        } else {
            if (is.null(local_dependencies[[local_wrt]])){
              i0 = list(); p0 = list()
              for (y in names(inputs)) i0[[y]] = runif(1)
              for (y in names(params)) p0[[y]] = runif(1)
              if(wrt == 'output'){
                v0 = rule.output(inputs = i0, params = p0, objects = objects)
              } else {
                v0 = try(rule.gradient(inputs = i0, params = p0, objects = objects, wrt = local_wrt), silent = T)
                if(inherits(v0, 'try-error') | is.empty(v0)){v0 = 1} # Something greater that 0 to add a dependancy if gradient is not known!
              }
              i1 = i0; p1 = p0
            }

            for (y in names(inputs)){
              if(is.null(local_dependencies[[local_wrt]])){
                test = runif(5)
                tval = numeric(5)
                for (i in 1:5){
                  i1[[y]] = test[i]
                  if (wrt == 'output'){
                    tval[i] = rule.output(inputs = i1, params = p0, objects = objects)
                  } else {
                    tvali   = try(rule.gradient(inputs = i1, params = p0, objects = objects, wrt = local_wrt), silent = T)
                    if(inherits(tvali, 'try-error') | is.empty(tvali)){tvali = 1}
                    tval[i] = tvali
                  }
                }
                i1[[y]] = i0[[y]]
                permit = sum(tval != v0) > 0
              } else {permit = y %in% local_dependencies[[local_wrt]]}
              if(permit) {
                dependencies[[wrt]] <<- c(dependencies[[wrt]], name %>% paste(y, sep = '.'))
                if(inherits(inputs[[y]], 'FUNCTION')){dependencies[[wrt]] <<- c(dependencies[[wrt]], inputs[[y]]$get.dependencies())}
              }
            }

            for (y in names(params)){
              if(is.null(local_dependencies[[local_wrt]])){
                test = runif(5)
                tval = numeric(5)
                for (i in 1:5){
                  p1[[y]] = test[i]
                  if (wrt == 'output'){
                    tval[i] = rule.output(inputs = i0, params = p1, objects = objects)
                  } else {
                    tvali   = try(rule.gradient(inputs = i0, params = p1, objects = objects, wrt = local_wrt), silent = T)
                    if(inherits(tvali, 'try-error') | is.empty(tvali)) {tvali = 1}
                    tval[i] = tvali
                  }
                }
                p1[[y]] = p0[[y]]
                permit = sum(tval != v0) > 0
              } else {permit = y %in% local_dependencies[[local_wrt]]}
              if(permit) {
                dependencies[[wrt]] <<- c(dependencies[[wrt]], name %>% paste(y, sep = '.'))
              }
            }
          }
      }
      return(dependencies[[wrt]])
    },

    list.parameters = function(){
      if(is.null(objects$list_parameters)){
        lst = character()
        for(inp in names(inputs)){
          if(inherits(inputs[[inp]], 'FUNCTION')){
            lst %<>% c(inputs[[inp]]$list.parameters())
          }
        }
        for(pm in names(params)){
          lst %<>% c(name %>% paste(pm, sep = '.'))
        }
        objects$list_parameters <<- lst
      }
      return(objects$list_parameters)
    },

    list.inputs = function(){
      if(is.null(objects$list_inputs)){
        lst = character()
        for(inp in names(inputs)){
          lst %<>% c(name %>% paste(inp, sep = '.'))
          if(inherits(inputs[[inp]], 'FUNCTION')){
            lst %<>% c(inputs[[inp]]$list.inputs())
          }
        }
        objects$list_inputs <<- lst
      }
      return(objects$list_inputs)
    },

    # Computes gradient of the function with respect to a single input or paramter
    # get.gradient = function(wrt){
    #   terms = wrt %>% strsplit(".", fixed = T)
    #
    #   vars = terms %>% lapply(function(x){if(length(x) > 1){x[[2]]} else {x[[1]]}}) %>% unlist
    #   funs = terms %>% lapply(function(x){if(length(x) > 1){x[[1]]} else {'@'}}) %>% unlist
    #   funs[vars %in% c(names(inputs), names(params)) & (funs == '@')] = name
    #
    #   vars[funs == name & (vars %in% c(names(inputs), names(params)))] -> locals
    #
    #   terms = terms[[1]]
    #   if((length(terms) == 1) & (wrt %in% c(names(inputs), names(params)))){
    #     terms = c(name, terms)
    #     wrt   = terms %>% collapse(sep = ".")
    #   }
    #   if(is.null(gradients[[wrt]])){
    #     if(terms[1] == name & length(terms) > 1){
    #       assert(local_wrt %in% c(names(inputs), names(params)), "Function " %++% terms[1] %++% " has no input or parameter named as " %++% local_wrt )
    #       gradients[[wrt]] <<- rule.gradient(inputs = get.inputs(), params = params, wrt = local_wrt)}
    #     else {
    #       get.gradients(names(inputs))
    #       gradients[[wrt]] <<- gradients[[inp]]*inputs[[inp]]$get.gradients(wrt)
    #     }
    #   }
    # },
    #

    is.param = function(var){
      var %in% names(params)
    },
    is.input = function(var){
      var %in% names(inputs)
    },

    varsplit = function(var){
      #N  = sequence(length(var))
      #ss = N %in% grep(pattern = '.', x = parameters, fixed = T)
      tbc = sequence(length(var)) %>% setdiff(grep(pattern = '.', x = var, fixed = T))
      var[tbc] <- 'UNK' %>% paste(var[tbc], sep = '.')

      terms = var %>% strsplit(".", fixed = T)

      vars = terms %>% lapply(function(x){x[[2]]}) %>% unlist
      funs = terms %>% lapply(function(x){x[[1]]}) %>% unlist

      #
      local = vars %in% c(names(inputs), names(params))
      match = funs == name
      spcfy = funs != 'UNK'

      error = spcfy & !local & match
      if(sum(error) > 0){
        stop(vars[error] %>% paste('is not a parameter or input of', name, '!') %>% paste(collapse = '\n'))
      }

      return(list(locals = vars[local & (!spcfy | match)], nonlocals = var[(!spcfy & !local) | (spcfy & !match)]))
    },

    get.gradients = function(wrt, data = NULL){
      tbc = sequence(length(wrt)) %>% setdiff(grep(pattern = '.', x = wrt, fixed = T))
      wrt[tbc] <- name %>% paste(wrt[tbc], sep = '.')
      newwrt = wrt %>% setdiff(names(gradients))

      terms = newwrt %>% strsplit(".", fixed = T)

      vars = terms %>% lapply(function(x){x[[2]]}) %>% unlist
      funs = terms %>% lapply(function(x){x[[1]]}) %>% unlist


      locals = vars[funs == name]
      valids = locals %^% (names(inputs) %U% names(params))
      locals = name %>% paste(locals, sep = '.')

      for(i in valids){
        grd_i = try(rule.gradient(inputs = get.inputs(data = data), params =  params, objects = objects, wrt = i), silent = T)
        if(inherits(grd_i, 'try-error') | is.empty(grd_i)){
           grd_i = extract.gradient.local(.self, wrt = i, data = data)
        }
        grd_i = as.numeric(grd_i)
        gradients[[name %>% paste(i, sep ='.')]] <<- grd_i
      }

      nonlocals <- newwrt %>% setdiff(locals)

      if(length(nonlocals) > 0){
        for(i in nonlocals){gradients[[i]] <<- 0}
        Gri = get.gradients(names(inputs), data = data)
        for (j in names(inputs)){
          if(inherits(inputs[[j]], 'FUNCTION')){
            Grj = inputs[[j]]$get.gradients(nonlocals, data = data)
            for (i in nonlocals){
              gradients[[i]] <<- gradients[[i]] + Gri[[name %>% paste(j, sep = '.')]]*Grj[[i]]
            }
          }
        }
      }
      gradients %>% list.extract(wrt)
    },

    deep_copy = function(){
      # What about ref class objects other than those stored in list 'inputs'?!
      self_copy = .self$copy()
      for(inp in names(inputs)){
        if(inherits(inputs[[inp]], 'FUNCTION')){
          self_copy$inputs[[inp]] <- inputs[[inp]]$deep_copy()
        }
      }
      return(self_copy)
    },

    get.param = function(...){
      vals   = c(...) %>% verify('character')
      out    = list()
      split  = varsplit(vals)
      locpam = split$locals %^% names(params)
      for(pm in locpam){out[[paste(name, pm, sep = '.')]] <- params[[pm]]}

      for(inp in names(inputs)){
        if(inherits(inputs[[inp]],'FUNCTION')){
          out = c(out, inputs[[inp]]$get.param(split$nonlocals))
        }
      }
      return(out)
    },

    reset = function(){
      values       <<- list()
      gradients    <<- list()
      dependencies <<- list()
      objects$list_parameters <<- NULL

      for(inp in names(inputs)){
        if(inherits(inputs[[inp]], 'FUNCTION')){
          inputs[[inp]]$reset()
        }
      }
    },

    reset.var = function(var){
      tbc = var %>% length %>% sequence %>% setdiff(grep(pattern = '.', x = var, fixed = T))
      var[tbc] <- name %>% paste(var[tbc], sep = '.')
      deps = get.dependencies()
      if(sum(var %in% deps) > 0){values[['output']] <<- NULL}
      for(inp in names(inputs)){
        inpcmp = name %>% paste(inp, sep = '.')
        if((inpcmp) %in% deps){
          if(inpcmp %in% var){
            values[[inp]]      <<- NULL
            values[['output']] <<- NULL
          }

          if(inherits(inputs[[inp]], 'FUNCTION')){
            if(sum(var %in% inputs[[inp]]$get.dependencies()) > 0){
              values[[inp]]      <<- NULL
              values[['output']] <<- NULL
              inputs[[inp]]$reset.var(var)
            }
          }
        }
      }

      for(wrt in names(gradients)){
        if(sum(var %in% get.dependencies(wrt)) > 0){
          gradients[[wrt]] <<- NULL
        }
      }
    },

    set.param = function(...){
      vals = list(...)
      if(length(vals) == 1){
        if(inherits(vals[[1]], 'list')){vals = vals[[1]]}
        else if(inherits(vals[[1]], c('numeric', 'integer'))){
          nss = names(vals[[1]])
          if(length(nss) > 0) {

            vals = as.list(vals[[1]])
          }
        }
      }
      parameters = names(vals)
      assert(parameters %<% list.parameters(), 'Unknown parameter(s): ' %++% paste(parameters, collapse = ', '))
      reset.var(parameters)
      #reset()
      split  = varsplit(parameters)
      locpam = split$locals %^% names(params)
      for(pm in locpam){params[[pm]] <<- vals[[name %>% paste(pm, sep = '.')]]}
      vals %>% list.extract(split$nonlocals) -> pm
      if(length(pm) > 0){
        for(inp in names(inputs)){
          if(inherits(inputs[[inp]], 'FUNCTION')){
            if(names(pm) %<% inputs[[inp]]$list.parameters()){
              inputs[[inp]]$set.param(pm)
            }
          }
        }
      }
    }
  )
)

#' @export
FUNCTION.AGG = setRefClass('FUNCTION.AGG', contains = 'FUNCTION', methods = list(
  get.output.agg = function(...){
    get.output(...) %>% sum
  },

  get.gradients.agg = function(...){
    out = list()
    for(gr in names(get.gradients(...))){
      out[[gr]] = sum(gradients[[gr]])
    }
    return(out)
  },

  plot.var = function(wrt, data = NULL, ...){
    if(wrt %in% list.parameters()){
      plot.parameter(wrt, data = data, ...)
    } else stop('TODO: Not supported yet!')
  },

  plot.parameter = function(parameter, data = NULL, min = 0, max = 1, breaks = 1000, ...){
    h = max - min; v = numeric(); keep = get.param(parameter)
    u = min + (sequence(breaks + 1) - 1)*h/breaks
    for(i in u){
      ui = i; names(ui) <- parameter
      set.param(ui)
      v = c(v, get.output.agg(data = data))
    }
    plot(u, v, ...)
    set.param(keep)
  },

  # minimize = function(parameters = list.parameters(), optimizer = 'stats.optim', data = NULL, silent = F, min_reduction_ratio = 10*.Machine$double.eps, ...){
  #   assert(optimizer %in% valid.optimizers)
  #   if(!silent){
  #     cat('\n', optimizer, ' is working:', '\n')
  #   }
  #   initial_fvalue = get.output.agg(data = data); keep = get.param(parameters)
  #   switch(optimizer,
  #          'stats.optim' = {
  #            arg_par  = get.param(parameters) %>% unlist
  #            arg_fn   = function(x){
  #              set.param(x)
  #              # reset()
  #              get.output.agg(data = data)
  #            }
  #            arg_gr   = function(x){
  #              set.param(x)
  #              # reset()
  #              get.gradients.agg(parameters, data = data) %>% unlist
  #            }
  #            components = strsplit(optimizer, '.', fixed = T)
  #            package = components[[1]][1]
  #            optfunc = components[[1]][2]
  #            rscript = paste(package, optfunc, sep ='::')
  #            res     = parse(text = (rscript %>% paste('(par = arg_par, fn = arg_fn, gr = arg_gr, ...)'))) %>% eval
  #            if(res$value < get.output.agg(data = data)){
  #              set.param(res$par)
  #            }
  #          },
  #          'funer.walker' = {
  #            minimize.walk(.self, parameters = parameters, data = data, min_reduction_ratio = min_reduction_ratio, silent = silent, ...)
  #          },
  #          'funer.ccd' = {minimize.ccd(.self, parameters = parameters, data = data, min_reduction_ratio = min_reduction_ratio, silent = silent, ...)})
  #   if(initial_fvalue - get.output.agg(data = data) > min_reduction_ratio*initial_fvalue){
  #     if(!silent){
  #       cat('\n', 'Loss reduced from ', initial_fvalue, ' to', get.output.agg(data = data))
  #     }
  #   } else {
  #     set.param(keep)
  #     get.output.agg(data = data) %>% equal(initial_fvalue) %>% assert('This should not happen. Something is wrong!')
  #     if(!silent){
  #       cat('\n', 'Failed to reduce loss from ', initial_fvalue)
  #     }
  #   }
  # }

  minimize = function(parameters = list.parameters(), package = 'stats', optimizer = 'optim', data = NULL, silent = F, min_reduction_ratio = 100*.Machine$double.eps, ...){
    scr = paste(package, optimizer, sep = '::')
    if(!silent){
      cat('\n', scr, ' is working:', '\n')
    }

    initial_fvalue = get.output.agg(data = data); keep = get.param(parameters)
    arg_par  = get.param(parameters) %>% unlist
    arg_fn   = function(x){
      set.param(x)
      # reset()
      get.output.agg(data = data)
    }
    arg_gr   = function(x){
      set.param(x)
      # reset()
      get.gradients.agg(parameters, data = data) %>% unlist
    }

    res      = parse(text = (scr %>% paste('(par = arg_par, fn = arg_fn, gr = arg_gr, ...)'))) %>% eval

    if(initial_fvalue - get.output.agg(data = data) > min_reduction_ratio*initial_fvalue){
      if(!silent){
        cat('\n', 'Loss reduced from ', initial_fvalue, ' to', get.output.agg(data = data))
      }
    } else {
      set.param(keep)
      get.output.agg(data = data) %>% equal(initial_fvalue) %>% assert('This should not happen. Something is wrong!')
      if(!silent){
        cat('\n', 'Failed to reduce loss from ', initial_fvalue)
      }
    }
    return(res)
  }
))

# wrt is the local input or parameter name
extract.gradient.local = function(f, wrt, data = NULL, h = 10000*.Machine$double.eps){
  # Verify:
  wrt %>% verify('character', lengths = 1, null_allowed = F, domain = c(names(f$inputs), names(f$params)))
  y1 = f$get.output(data = data)
  if(wrt %in% names(f$params)){
    xx = f$params
    xx[[wrt]] = xx[[wrt]] + h
    y2 = f$rule.output(inputs = f$get.inputs(data = data), params = xx, objects = f$objects)
    grad <- (y2 - y1)/h
  } else if (wrt %in% names(f$inputs)){
    xx = f$get.inputs(data = data)
    xx[[wrt]] = xx[[wrt]] + h
    y2 = f$rule.output(inputs = xx, params = f$params, objects = f$objects)
    grad <- (y2 - y1)/h
  } else (stop('Given variable ' %++% wrt %++% ' is not a local input or parameter!'))

  # f$values$output
  return(grad)
}

# wrt should refer to a parameter (completeparameter name is required: example: f1.a)
extract.gradient = function(f, wrt, data = NULL, h = 10000*.Machine$double.eps){
  # verify:
  wrt %<>% verify('character', lengths = 1, null_allowed = F)
  fc = f$deep_copy()

  y1 = fc$get.output(data = data)
  xx = fc$get.param(wrt)[[wrt]]

  fc$set.param(list(wrt = xx + h) %>% {names(.) <- wrt; .})

  y2 = fc$get.output(data = data)

  return((y2-y1)/h)
}





