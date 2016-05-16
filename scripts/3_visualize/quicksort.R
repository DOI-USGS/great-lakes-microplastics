# quicksort from http://www.geeksforgeeks.org/iterative-quick-sort/

# A utility function to swap two elements
swap <- function(a, b)
{
  # Don't swap if the elements are the same
  if(a==b) return()
  
  # Do the swapping. This has 'side-effects' (i.e., the main effect) on sorted
  t <- sorted[a]
  sorted[a] <<- sorted[b]
  sorted[b] <<- t
  
  # Keep a record of what has been swapped
  swaps <<- rbind(swaps, matrix(c(a, b), ncol=2, nrow=1))
  
  # Keep a record of results of swaps
  steps <<- rbind(steps, matrix(sorted, ncol=length(sorted), nrow=1))
}

# Identify a pivot index that partitions an array around the value of element h.
# The pivot p is the element originally at h; all elements with values less than
# p's are moved to the left, then p is placed at the first position to the right
# of those values. 
# sorted --> Array to be sorted, 
# l  --> Starting index (low), 
# h --> Ending index (high)
partition <- function(l, h) {
  x = sorted[h]
  i = l - 1
  
  for (j in l:(h-1)) {
    if (sorted[j] <= x) {
      i <- i + 1
      swap(i, j)
    }
  }
  swap(i+1, h)
  return(i + 1)
}

# tosort --> Array to be sorted, 
# l  --> Starting index (low), 
# h  --> Ending index (high)
quickSortIterative <- function(tosort, l=1, h=length(tosort)) {
  # Prepare to document all the swaps we make. swaps gets edited by swap()
  swaps <<- matrix(ncol=2, nrow=0)
  steps <<- matrix(tosort, ncol=length(tosort), nrow=1)
  
  # Make a copy of arr that we can manipulate
  sorted <<- tosort
  
  # Create an auxiliary stack about as long as tosort; we'll use less than that.
  # the stack will contain partition bounds
  stack <- integer(h-l+1);
  
  # initialize top of stack
  top <- 0
  
  # push initial values of l and h to stack
  stack[top <- top + 1] = l
  stack[top <- top + 1] = h
  
  # Keep popping from stack while is not empty
  while ( top > 0 ) {
    # Pop h and l (i.e., consider the next partition on the stack)
    h <- stack[top]
    l <- stack[top - 1]
    top <- top - 2
    
    # Set pivot element at its correct position in sorted array. The "correct
    # position" is the position at which there are no elements with values
    # greater than p's to the left of it. This does some sorting along the way.
    p <- partition(l, h)
    
    # If there are elements on left side of pivot, push left side to stack
    if ( p-1 > l ) {
      stack[top <- top + 1] = l
      stack[top <- top + 1] = p - 1
    }
    
    # If there are elements on right side of pivot, push right side to stack
    if ( p+1 < h ) {
      stack[top <- top + 1] = p + 1
      stack[top <- top + 1] = h
    }
  }
  
  out <- list(sorted=sorted, swaps=swaps, steps=steps)
  rm(sorted, swaps, steps, envir=.GlobalEnv)
  return(out)
}

# quickSortIterative(c(4,2,5,1,3))
