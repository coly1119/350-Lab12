generate_data = function(n, p)
{
  covariates = matrix(data = rnorm(n*p, 0, 1), nrows = n, ncols = p)
  responses = rnorm(n, 0, 1)
  return(list(covariates, responses))
}

model_select = function(covariates, responses, cutoff)
{
  lm1 = lm(responses~covariates)
  if(cutoff < 1) 
  {
    lm2 = lm(responses~c())
  }
  else{
    lm2 = lm(responses~covraiates[,1:cutoff])
  }
}