#' Calculate a subpixel confusion-uncertainty matrix.
#'
#' @param predicted A matrix or data.frame of predicted values.
#' Each row must sum up to 1.
#' @param observed A matrix or data.frame of true observed values against which
#' the predicted values will be validated.
#' Each row must sum up to 1, and must correspond to the equivalent rows in
#' \code{predicted}, i.e. both have to be the same length.
#' @param agreement A function for calculating the diagonals of the confusion matrix.
#' This can be any unary reduction function, such as \code{min}, taking two
#' numbers and returning one.
#' @param disagreement A function for calculating the off-diagonals of the confusion
#' matrix. If the string \code{"SCM"} is given, the disagreement is calculated
#' using the mean of \code{MIN_D} and \code{LEAST_D}, with the associated
#' uncertainty, as recommended by the Silvan-Cardenas and Wang (2008) paper.
#' Otherwise, the function must take four arguments: the overestimation matrix,
#' the underestimation matrix, and indices for the row and column of the result.
#' With a custom function, the uncertainty is not calculated
#' (a zero matrix is returned).
#' @param scale Boolean, whether the result should be scaled (reported in percentages).
#' @param accuracy Boolean, whether to calculate accuracy statistics.
#' Passes the result through \code{accuracy.scm}.
#' @param totals Boolean, whether to calculate the row and column totals.
#' Implies \code{accuracy=TRUE}. Passes the result through \code{totals.scm}.
#' @param plot Boolean, whether to plot the resulting matrices graphically.
#' Passes the result through \code{plot.scm}.
#' @return An object of class \code{scm} that is a list with the elements:
#' matrix \code{P} of the expected values of the subpixel confusion matrix,
#' matrix \code{U} of the uncertainty values around \code{P},
#' strings \code{agreement} and \code{disagreement} showing what value was
#' used for the \code{agreement} and \code{disagreement} arguments for
#' traceability, and optionally the additions from \code{accuracy.scm}
#' and \code{totals.scm}.
#' 
#' @example examples/SCM.R
#' 
#' @export
SCM = function(predicted, observed, agreement=min, disagreement="SCM", scale=FALSE, accuracy=FALSE, totals=FALSE, plot=FALSE)
{
    # Aliases; TODO: remove/rename
    s_nk = predicted
    r_nl = observed
    
    # Sanity checks: the input must be 0-1 range and add up to 100%
    stopifnot(all(r_nl >= 0-options()$ts.eps))
    stopifnot(all(r_nl <= 1+options()$ts.eps))
    stopifnot(all(s_nk >= 0-options()$ts.eps))
    stopifnot(all(s_nk <= 1+options()$ts.eps))
    if (is.vector(s_nk))
        stopifnot(all(round(sum(s_nk), 6) == 1))
    else
        stopifnot(all(round(rowSums(s_nk), 6) == 1))
    if (is.vector(r_nl))
        stopifnot(all(round(sum(r_nl), 6) == 1))
    else
        stopifnot(all(round(rowSums(r_nl), 6) == 1))
    
    if (is.character(disagreement) && disagreement == "SCM") # Mean of MIN_D and LEAST_D + confusion
    {
        p_min = Comparator(s_nk, r_nl, A=agreement, D=MIN_D, scale=scale)
        p_least = Comparator(s_nk, r_nl, A=agreement, D=LEAST_D, scale=scale)
        P_kl = (p_min + p_least)/2
        U_kl = (p_min - p_least)/2
    } else {
        P_kl = Comparator(s_nk, r_nl, A=agreement, D=disagreement, scale=scale)
        U_kl = matrix(0, nrow=nrow(P_kl), ncol=ncol(P_kl), dimnames=dimnames(P_kl)) # No uncertainties for other types; U is a zero matrix
    }

    scm = structure(list(P=P_kl, U=U_kl, agreement=as.character(substitute(agreement)), disagreement=as.character(substitute(disagreement))), class="scm")
    
    if (plot)
        plot(scm)
    
    if (accuracy || totals)
        scm = accuracy.scm(scm)
    
    if (totals)
        scm = totals.scm(scm)

    return(scm)
}
