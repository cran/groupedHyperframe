
#' @title Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @description
#' Create 
#' \link[spatstat.explore]{fv.object}s 
#' or 
#' distances
#' from 
#' one-and-only-one \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' inside a \link[spatstat.geom]{hyperframe}.
#' 
#' @param X a \link[spatstat.geom]{hyperframe}, containing ***one-and-only-one*** \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @param op \link[base]{function}, currently functions [fv_ppplist()] or [dist_ppplist()] are accepted
#' 
#' @param ... additional parameters of workhorse functions 
#' [fv_ppplist()] or [dist_ppplist()]
#' 
#' @details
#' See details in workhorse functions [fv_ppplist()] or [dist_ppplist()].
#' 
#' @returns
#' Function [op_hyperframe()] returns a \link[spatstat.geom]{hyperframe} with additional
#' \itemize{
#' 
#' \item \link[spatstat.explore]{fv.object} \link[spatstat.geom:hyperframe]{hypercolumns} if `op = fv_ppplist`. 
#' ***One hypercolumn per \link[base]{numeric} mark*** in the \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}.
#' 
#' \item \link[base]{numeric} \link[spatstat.geom:hyperframe]{hypercolumns} if `op = dist_ppplist`.
#' ***One hypercolumn per \link[spatstat.geom]{is.multitype} mark*** 
#' in the \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}.
#' 
#' }
#' 
#'  
#' @keywords internal
#' @importFrom spatstat.geom is.ppplist as.list.hyperframe cbind.hyperframe
#' @export
op_hyperframe <- function(X, op, ...) {
  
  id <- vapply(X, FUN = is.ppplist, FUN.VALUE = NA)
  if (sum(id) != 1L) stop('allow one-and-only-one ppp-hypercolumn, which may contain one or more mark(s)')
  
  ret0 <- op(x = as.list.hyperframe(X)[[which(id)]], ...)
  
  # re-organize the list!!
  # `ret0`: 1st subject, 2nd mark
  # `ret1`: 1st mark, 2nd subject
  ret1 <- .mapply(FUN = list, dots = ret0, MoreArgs = NULL)
  names(ret1) <- names(ret0[[1L]])
  
  ret <- do.call(
    what = cbind.hyperframe, 
    args = c(list(X), ret1)
  )
  
  if (inherits(X, what = 'groupedHyperframe')) {
    attr(ret, which = 'group') <- attr(X, which = 'group', exact = TRUE)
    class(ret) <- unique.default(c('groupedHyperframe', class(X)))
  } # a bandage fix, for now
  
  return(ret)
  
}


