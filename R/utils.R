# Keeping logger options
my_options <- new.env(parent = emptyenv())

get_options <- function(k, v) {
  my_options[[k]]
}

set_options <- function(k, v) {
  my_options[[k]] <- v
}

list_options <- function() {
  names(my_options)
}
