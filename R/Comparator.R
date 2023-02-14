#' Comparator function for subpixel confusion matrices
#' 
#' The low-level function used to calculate an \code{SCM}.
#' Used internally by \code{SCM}.
#' Should normally not be called directly unless by experienced users.
#' 
#' @param s_k A matrix or another object coercible to one of predicted values.
#' Each row must sum up to 1.
#' @param r_l A matrix or another object coercible to one of true observed values against which
#' the predicted values will be validated.
#' @param A A function for calculating the diagonals of the confusion matrix.
#' This can be any unary reduction function, such as \code{min}, taking two
#' numbers and returning one.
#' @param D A function for calculating the off-diagonals of the confusion
#' matrix. The function must take four arguments: the overestimation matrix,
#' the underestimation matrix, and indices for the row and column of the result.
#' With a custom function, the uncertainty is not calculated
#' (a zero matrix is returned).
#' @param scale Boolean, whether the result should be scaled (reported in percentages).
#' @return A confusion matrix.
Comparator = function(s_k, r_l, A=MIN, D=PROD_D, scale=FALSE)
{
    # If we have a data.frame, convert into a matrix
    if (is.data.frame(s_k))
        s_k = as.matrix(s_k)
    if (is.data.frame(r_l))
        r_l = as.matrix(r_l)

    stopifnot(is.numeric(s_k))
    stopifnot(is.numeric(r_l))
    stopifnot(!is.null(A) || !is.null(D))
    
    if (!is.null(A))
        A = match.fun(A)
    if (!is.null(D))
        D = match.fun(D)
    
    # If s_k and r_l are matrices/data.frames, we sum them all
    # If not, make it a matrix anyway to make the same code handle it
    if (is.vector(s_k))
        s_k = t(as.matrix(s_k))
    if (is.vector(r_l))
        r_l = t(as.matrix(r_l))
    
    stopifnot(all(dim(s_k) == dim(r_l)))
    
    CumulativeResult = matrix(0, ncol(s_k), ncol(r_l))
    for (n in 1:nrow(s_k))
    {
        s_nk = s_k[n,]
        r_nl = r_l[n,]
        
        K = length(s_nk)
        
        # Overestimation and underestimation
        sp_nk = s_nk - r_nl; sp_nk[sp_nk<0] = 0
        rp_nl = r_nl - s_nk; rp_nl[rp_nl<0] = 0

        Result = matrix(NA, K, K) #p_nkl
        for (k in 1:K)
        {
            for (l in 1:K)
            {
                if (k == l) { # Diagonal
                    if (!is.null(A))
                        Result[k,l] = A(s_nk[k], r_nl[l])
                    else
                        Result[k,l] = D(sp_nk, rp_nl, k, l)
                } else {
                    if (!is.null(D))
                        Result[k,l] = D(sp_nk, rp_nl, k, l)
                    else
                        Result[k,l] = A(s_nk[k], r_nl[l])
                }
            }
        }
        CumulativeResult = CumulativeResult + Result
    }
    rownames(CumulativeResult) = colnames(s_k)
    colnames(CumulativeResult) = colnames(r_l)
    if (scale)
        CumulativeResult = CumulativeResult / nrow(s_k)
    
    return(CumulativeResult)
}
