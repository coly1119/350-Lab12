generate_data = function(n, p)
{
  covariates = matrix(data = rnorm(n*p, 0, 1), nrows = n, ncols = p)
  responses = rnorm(n, 0, 1)
  return(list(covariates = covariates, responses = responses))
}