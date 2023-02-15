#' Plot an scm object.
#' 
#' Rerquires the \code{lattice} and \code{gridExtra} packages to function.
#' 
#' @param x The \code{scm} object to plot.
#' @param ... Other parameters passed to \code{lattice::levelplot}.
#' @return Nothing, a plot is made as a side effect.
#' @export
plot.scm = function(x, ...)
{
    if (requireNamespace("lattice", quietly = TRUE) && requireNamespace("gridExtra", quietly = TRUE))
    {
        P_plot = lattice::levelplot(t(x$P), xlab="Observed", ylab="Predicted", sub="Centre values", ...)
        U_plot = lattice::levelplot(t(x$U), xlab="Observed", ylab="Predicted", sub="Uncertainty", ...)
        gridExtra::grid.arrange(P_plot, U_plot, ncol=2)
    } else {
        stop("Unable to plot due to missing lattice or gridExtra packages")
    }
}
