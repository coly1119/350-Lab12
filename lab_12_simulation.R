generate_data = function(n, p)
{
  covariates = matrix(data = rnorm(n*p, 0, 1), nrows = n, ncols = p)
  responses = rnorm(n, 0, 1)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff)
{
  lm1 = lm(responses~covariates)
  pval = (summary(lm1)$coefficients)[-1, 4]
  if(cutoff < 1) 
  {
    return(c())
  }
  else{
    withinCut = which(pval <= cutoff)
    regress = lm(responses ~ covariates[, withinCut])
  }
  pval2 = (summary(resgress)$coefficients)[-1, 4]
  return(pval2)
}

run_simulation = function(n_trials, n, p, cutoff) 
{
  pval = vector(n_trials)
  for (i in 1:n_trials) {
    lst = generate_data(n, p)
    covariates = lst$covariates
    responses = lst$responses
    model = model_select(covariates, responses, cutoff)
    pval = c(p.value, model)
  }
  save(pval, file = "pvalue.R")
}

run_simulation(100, 100, 10, 0.05)
run_simulation(1000, 1000, 20, 0.05)
run_simulation(10000, 10000, 50, 0.05)



