#' Operator functions for subpixel confusion matrices
#' 
#' These are the operators, both the agreement (with no prefix) and disagreement
#' (with a \code{D_} prefix) that were defined by
#' Silvan-Cardenas and Wang (2008).
#' 
#' @name operators
#' @rdname operators
#' @param s_nk grade of membership of pixel n to class k assigned by the
#' assessed dataset (predicted value)
#' @param r_nl grade of membership of pixel n to class l assigned by the
#' reference dataset (observed value)
#' @param sp_nk The overestimation value (predictions minus observations)
#' @param rp_nl The underestimation value (observations minus predictions)
#' @param k Index in the resulting confusion matrix,
#' indicating which class was predicted (confusion matrix row).
#' @param l Index in the resulting confusion matrix,
#' indicated which class it was supposed to be attributed to
#' (confusion matrix column).
#' @return The resulting statistic as a number.
#' @examples
#' observed = c(X1=0.4, X2=0.3, X3=0.2, X4=0.1)
#' predicted = c(X1=0.5, X2=0.4, X3=0.1, X4=0)
#' # X1 agreement
#' SI(predicted["X1"], observed["X1"])
#' LEAST(predicted["X1"], observed["X1"])
#' # X1 disagrement: could be attributed to either X3 or X4
#' relu <- function(x) sapply(x, function(z) max(0,z))
#' PROD_D(relu(predicted-observed), relu(observed-predicted), 1, 3)
#' MIN_D(relu(predicted-observed), relu(observed-predicted), 1, 3)
#' LEAST_D(relu(predicted-observed), relu(observed-predicted), 1, 3)
#' # Full SCM
#' SCM(predicted, observed, disagreement=PROD_D)$P
#' SCM(predicted, observed, disagreement=MIN_D)$P
#' SCM(predicted, observed, disagreement=LEAST_D)$P
NULL

#' @rdname operators
#' @export
SI = function(s_nk, r_nl)
{
    1 - abs(s_nk - r_nl)/(s_nk + r_nl)
}

#' @rdname operators
#' @export
LEAST = function(s_nk, r_nl)
{
    max(s_nk + r_nl - 1, 0)
}

#' @rdname operators
#' @export
PROD_D = function(sp_nk, rp_nl, k, l)
{
    sp_nk[k] * rp_nl[l] / sum(rp_nl)
}

#' @rdname operators
#' @export
MIN_D = function(sp_nk, rp_nl, k, l)
{
    min(sp_nk[k], rp_nl[l])
}

#' @rdname operators
#' @export
LEAST_D = function(sp_nk, rp_nl, k, l)
{
    max(sp_nk[k]+rp_nl[l]-sum(rp_nl), 0)
}
