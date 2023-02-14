#' Operator functions for subpixel confusion matrices
#' 
#' These are the operators, both the agreement (with no prefix) and disagreement
#' (with a \code{D_} prefix) that were defined by
#' Silvan-Cardenas and Wang (2008).
#' 
#' @rdname operators
#' @param s_nk The overestimation matrix (predictions minus observations)
#' @param r_nl The underestimation matrix (observations minus predictions)
#' @param k Index in the resulting confusion matrix,
#' indicating which class was predicted (confusion matrix row).
#' @param l Index in the resulting confusion matrix,
#' indicated which class it was supposed to be attributed to
#' (confusion matrix column).
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
