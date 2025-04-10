

#' @title Print [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [print.groupedHyperframe()] does not have a returned value.
#' 
#' @seealso `?nlme:::print.groupedData`
#' 
#' @keywords internal
#' @importFrom cli col_blue
#' @importFrom spatstat.geom as.data.frame.hyperframe as.list.hyperframe
#' @importFrom utils head
#' @export print.groupedHyperframe
#' @export
print.groupedHyperframe <- function(x, ...) {
  'Grouped Hyperframe: ' |> cat()
  grp <- attr(x, which = 'group', exact = TRUE)
  #if (identical(emptyenv(), environment(grp))) {
  #  environment(grp) <- globalenv()
  #} # not sure how this is useful in ?nlme:::print.groupedData
  print(grp, ...)
  
  g <- all.vars(grp)
  ns <- g |> 
    seq_along() |> 
    vapply(FUN = \(i) { # (i = 1L)
      f <- do.call(what = interaction, args = c(
        as.list.hyperframe(x[j = g[seq_len(i)], drop = FALSE]),
        list(drop = TRUE, lex.order = TRUE)
      ))
      length(levels(f))
    }, FUN.VALUE = NA_integer_)
  
  cat('\n')
  mapply(FUN = \(n, g) {
    paste(n, col_blue(g))
  }, n = ns, g = g, SIMPLIFY = TRUE) |> 
    rev.default() |> 
    cat(sep = ' nested in\n')
  cat('\n')
  
  # see inside ?spatstat.geom::print.hyperframe
  x |>
    as.data.frame.hyperframe(discard = FALSE) |> 
    head(n = 10L) |>
    print(...) 
}








#' @title Extract Subset of [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param ... additional parameters of \link[spatstat.geom]{[.hyperframe}
#' 
#' @returns
#' Function \link{[.groupedHyperframe} returns a [groupedHyperframe] or a \link[spatstat.geom]{hyperframe}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom [.hyperframe
#' @export [.groupedHyperframe
#' @export
`[.groupedHyperframe` <- function(x, ...) {
  ret <- `[.hyperframe`(x, ...)
  # a bandage fix hahaha
  group <- attr(x, which = 'group', exact = TRUE)
  if (!all(all.vars(group) %in% names(ret))) return(ret) # just 'hyperframe'
  attr(ret, which = 'group') <- group
  class(ret) <- unique.default(c('groupedHyperframe', class(ret)))
  return(ret)
}


#' @importFrom spatstat.geom subset.hyperframe
#' @export
subset.groupedHyperframe <- function(x, ...) {
  ret <- subset.hyperframe(x, ...)
  # a bandage fix hahaha
  group <- attr(x, which = 'group', exact = TRUE)
  if (!all(all.vars(group) %in% names(ret))) return(ret) # just 'hyperframe'
  attr(ret, which = 'group') <- group
  class(ret) <- unique.default(c('groupedHyperframe', class(ret)))
  return(ret)
}



#' @title Extract Grouping Formula from [groupedHyperframe]
#' @description
#' ..
#' 
#' @param object a [groupedHyperframe]
#' @param asList,sep place holders for S3 generic \link[nlme]{getGroupsFormula}
#' @returns 
#' Function [getGroupsFormula.groupedHyperframe()] returns a one-sided \link[stats]{formula}
#' @keywords internal
#' @importFrom nlme getGroupsFormula
#' @export getGroupsFormula.groupedHyperframe
#' @export
getGroupsFormula.groupedHyperframe <- function(object, asList, sep) {
  attr(object, which = 'group', exact = TRUE)
}

