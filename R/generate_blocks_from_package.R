# package to list functions from
# make sure the package is loaded in the environment


argument_to_field <- function(x){ # nolint
  l <- length(x) 
  
  if(inherits(x, "character") && l == 1){
    return(
      sprintf("blockr::new_string_field('%s')", x)
    )
  }
  
  if(inherits(x, "character") || inherits(x, "factor") && l > 1){
    return(
      sprintf("blockr::new_select_field('%s')", x)
    )
  }
  
  if(inherits(x, "numeric") && l > 1){
    return(
      sprintf("blockr::new_numeric_field(%s, min = %s, max = %s)", x, min(x), max(x))
    )
  }
  
  if(inherits(x, "logical")){
    return(
      sprintf("blockr::new_switch_field(%s)", x)
    )
  }
  
  if(inherits(x, "numeric")){
    return(
      sprintf("blockr::new_numeric_field(%s)", x)
    )
  }
  
  blockr::new_string_field()
}

make_expression <- function(fn, args){
  args <- names(args)
  args <- paste0(args, " = .(", args, ")")
  
  sprintf("%s(%s)", fn, paste(args, collapse = ",\n\t\t\t\t\t\t"))
}

generate_blocks_from_package<- function(package_name){
  
  if (!require(package_name, character.only = TRUE)) {
 
    library(package_name, character.only = TRUE)
  }
  # list all functions
  fns <- ls(sprintf("package:%s", package_name), all.names = TRUE)
  
  output_file <- paste0(package_name, "-blocks.R")
  con <- file(output_file, "a")
  on.exit(close(con))
  
  blocks <- fns |> 
    lapply(\(fn){
      args <- formals(get(fn))
      fields <- args |>
        lapply(argument_to_field)
      
      fields <- paste0(names(fields), " = ", fields, collapse = ",\n\t\t\t\t\t")
      
      sprintf(
 
      "
# %s Block Start ======================================
new_%s_block <- function(data, ...){
      
      all_cols <- colnames(data)
      
      blockr::new_block(
        name = '%s_block',
        expr = quote({\n\t%s\n}),
        fields = list(\n\t%s\n),
        class = c('data_block', '%s_block')
      )
}
      
%s_block <- function(data, ...) {
    initialize_block(new_%s_block(data, ...), data)
}
      
blockr::register_block(
    %s_block,
    '%s',
    'A block',
    input = 'data.frame',
    output = 'data.frame',
    classes = c('data_block', '%s_block')
)
# Block End ======================================
      
      ",
      fn,
        fn,
        fn,
        make_expression(fn, args),
        fields,
        fn,
        fn,
        fn,
      fn,
      fn,
        fn
      )
    }) |>
    lapply(\(block) writeLines(block, con))
  
}
