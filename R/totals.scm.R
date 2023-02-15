#' Calculate total metrics
#' 
#' Accessory function that enhances an input \code{SCM} object's matrices
#' \code{P} and \code{U} with row and column totals. This makes the totals
#' easier to read, but the resulting matrix is no longer compatible with
#' one that is produced by \code{SCM} without enhancement.
#' 
#' @param scm The \code{scm} object to enhance.
#' @return An enhanced \code{scm} object.
#' @examples
#' observed = c(Class_1=0.5, Class_2=0.375, Class_3=0.125)
#' predicted = c(Class_1=0.625, Class_2=0.25, Class_3=0.125)
#' SCM(predicted, observed, agreement="*", disagreement=NULL)
#' totals.scm(SCM(predicted, observed, agreement="*", disagreement=NULL))
#' # Same as:
#' SCM(predicted, observed, agreement="*", disagreement=NULL, totals=TRUE)
#' @export
totals.scm = function(scm)
{
    stopifnot(class(scm) == "scm")

    if (is.null(scm[["P_row_totals"]]))
    {
        warning("Passed an SCM without calculating accuracy metrics. This will be done for you.")
        scm = accuracy.scm(scm)
    }
    
    # These are unused?
    P_kp = rowSums(scm$P)
    P_pl = colSums(scm$P)
    P_pp = sum(scm$P)

    U_kp = rowSums(scm$U)
    U_pl = colSums(scm$U)
    U_pp = sum(scm$U)
    # End unused

    # Add total rows/columns: with uncertainty
    P_FullMatrix = cbind(scm$P, total=scm$P_row_totals)
    P_FullMatrix = rbind(P_FullMatrix, total=c(scm$P_col_totals, scm$P_total))

    U_FullMatrix = cbind(scm$U, total=scm$U_row_totals)
    U_FullMatrix = rbind(U_FullMatrix, total=c(scm$U_col_totals, scm$U_total))
    
    # Add user/producer accuracy
    P_FullMatrix = cbind(P_FullMatrix, user.acc=c(scm$P_user_accuracy, NA))
    P_FullMatrix = rbind(P_FullMatrix, prod.acc=c(scm$P_producer_accuracy, NA, scm$P_overall_accuracy))

    U_FullMatrix = cbind(U_FullMatrix, user.acc=c(scm$U_user_accuracy, NA))
    U_FullMatrix = rbind(U_FullMatrix, prod.acc=c(scm$U_producer_accuracy, NA, scm$U_overall_accuracy))
    
    scm$P = P_FullMatrix
    scm$U = U_FullMatrix
    return(scm)
}
