install.packages('GA')
library(GA)
dataForSvmGa = dataForSvm
dataForSvmGa$LM = ifelse(dataForSvmGa$LM == "human", 0, 1)
dataForSvmGa$LM =as.numeric(dataForSvmGa$LM)
## 5-fold cross validacija
K = 5 
fold_inds <- sample( 1 : K, nrow( dataForSvmGa ), replace = TRUE )

cv_data <- lapply( 
  1 : K, 
  function( index ) 
    list( 
      train_data = dataForSvmGa[ fold_inds != index, , drop = FALSE ], 
      test_data = dataForSvmGa[ fold_inds == index, , drop = FALSE ] 
    )
)
rmsd <- function( train_data, test_data, c, gamma ) 
{
  ## Treniranje SVM modela 
  model <- svm( 
    train_data,
    y=NULL,
    type='one-classification',
    gama=gama,
    nu=c,
    kernel = "radial",
    scale=TRUE
  )
  ## testiraj i kalkuliši RMSD
  rmsd <- mean( 
    ( predict( model, newdata = test_data ) - test_data$LM ) ^ 2 
  )
  
  return ( rmsd )
}
fitness_func <- function( x, cv_data ) 
{
  
  ## Dohvati SVM paramete
  gamma_val <- x[ 1 ]
  c_val <- x[ 2 ]
  
  ## Koristiti cross validaciju za procjenu RMSD za svaku porciju seta podataka
  rmsd_vals <- sapply(
    cv_data, 
    function( input_data ) with( 
      input_data, 
      rmsd( train_data, test_data, c_val, gamma_val ) 
    )
  )
  
  ## Vrati negativni RMSD 
  return ( -mean( rmsd_vals ) )
}

## Setuj opseg Gamma & C parametara
para_value_min <- c( gamma = 1e-3, c = 0 )
para_value_max <- c( gamma = 2, c = 1 )

## Pokreni genetički algoritam
results <- ga( type = "real-valued", 
               fitness = fitness_func, 
               cv_data, 
               names = names( para_value_min ), 
               min = para_value_min, 
               max = para_value_max,
               popSize = 50, 
               maxiter = 100
)
results
