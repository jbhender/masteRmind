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
print_error = function(input, n){
  if( input$error_free ){
    cat('Why did this get called! Programmer error.\n')
  } else {
    cat('User error - provide constructive feedback!\n')
    
    # Too few
    if( input$too_few ) {
      err_few = sprintf(
        "Too many elements in guess. Please try again with %i elements separated by '%s'!\n", n, sep )
      cat(err_few)
    }
    
    # Too many
    if( input$too_many ) {
      err_many = sprintf(
        "Too many elements in guess. Please try again with %i elements separated by '%s'!\n", 
        n, sep )
      cat(err_many)
    }
    
  }
}

# Standard feedback: ---------------------------------------------------------
feedback = function( guesses, results, secret, n) {
  turn = length(guesses)

  if( results[[turn]]$n_exact == n ) {
    win_msg = sprintf('Congratulations! You guessed the secret code: %s.\n', secret)
    cat(win_msg)
    return(TRUE)
  } else {
    for(i in 1:turn) {
      msg = sprintf('%i - %s: exact = %i, colors = %i.\n', i, guesses[[i]], 
                    results[[i]]$n_exact, results[[i]]$n_color )
      cat(msg)
    }
    return(FALSE)
  }
}

# Start up message: ----------------------------------------------------------

# Game skeleton: --------------------------------------------------------------
play_mastermind = function(n = 4, dict = NULL, max_turns = 10, repeats = FALSE) 
{
  # Initialize
  turn = 0
  win = FALSE
  secret = gen_code(n, dict, repeats, sep = ', ')
  
  while( turn <= max_turns && !win ) {
     input = request_input()
    
     input = clean_input(input, n, sep, dict)
     
     if( input$error_free ) {
       result = check_code(input$guess)
       win = feedback(result)
       
       turn = turn + 1
       if(turn <= max_turns) {
         request_input(turn)
       }
       
     } else {
       print_error(input)
     }
  }
  
  if( win ) return()
  if( turn > max_turns ){
    lose_msg = sprintf('Mastermind wins! The secret code was:\n %s.\n\n', secret)
    cat(lose_msg)
  }
}

