# Functional code base for masteRmind game in preparation for 
# an August 24, workshop.
#
# Updated: August 9, 2018 
# Author: James Henderson (jbhender@umich.edu)

# Generate the secret code: ---------------------------------------------------
# n - the number of words in the secret code
# dict - a dictionary from which to choose the code. 
# If NULL the standard dictionary is used.
# repeats = FALSE, - should repeats be allowed?
# based on the R function "sample"
# sep - how to separate words in the code

std_dict = c( R='Red', Gr='Green', Bu='Blue', Y='Yellow',
   Go='Gold', O='Orange', Ba='Black', W='White' )


gen_code = function(n = 4, dict=NULL, repeats=FALSE, sep=', ') {
  if( is.null(dict) ) {
    dict = c( R='Red', Gr='Green', Bu='Blue', Y='Yellow',
              Go='Gold', O='Orange', Ba='Black', W='White' )
  }
  
  paste( sample(dict, n, replace = repeats), collapse = sep)
}

# Compare a guessed code to the master code: ----------------------------------
check_code = function(guess, secret, n = 4, sep = ', ') {
  
  # Split guess and secret into vectors of length n
  guess = stringr::str_split_fixed(guess, pattern = sep, n = n)
  secret = stringr::str_split_fixed(secret, pattern = sep, n = n)
  
  n_exact = sum( guess == secret )
  n_color = length( intersect(guess, secret) )
  
  list( n_exact = n_exact, n_color = n_color )
}

# Translate and verify user input: --------------------------------------------
clean_input = function(guess, n = 4, sep = ', ', dict = std_dict) {
  
  # Split guess into a vector
  guess = stringr::str_split_fixed(guess, pattern = sep, n = n)
  
  # Do some error checking
  too_few = FALSE
  if( length(guess) < n) {
    too_few = TRUE
  }
  
  too_many = FALSE
  if( length(guess) > n) {
    too_many = TRUE
  }
  
  # Could standardize case first, check for quotes, etc ..
  # Check against dictionary
  not_in_dict = ! { guess %in% dict }
  
  # Check if user provided accepted abbreviations
  for( i in 1:length(guess) ) {
    
    if( not_in_dict[i] ) {
      vals = names(dict) %in% guess[i] 

      if( sum(vals) == 1 ) {
        guess[i] = unname( dict[which(vals)]  )
        not_in_dict[i] = FALSE
      } 
    }
  }
  
  list( guess = guess, #cleaned guess
        too_few = too_few, 
        too_many = too_many,
        not_in_dict = not_in_dict,
        error_free = ! any(too_few, too_many, not_in_dict)
  )
}

# Recieve user input: ---------------------------------------------------------
request_input = function(num_guess = 1) {
  request_str = sprintf('Plase enter guess #%i:\n', num_guess)
  cat(request_str)
  
  guess = readline()
  
  guess
}
  
# Response to input error: ----------------------------------------------------

# Standard feedback: 
