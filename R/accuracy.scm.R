#' Add accuracy metrics to an scm object
#'
#' An accessory function that enhances an \code{scm} object to include
#' accuracy statistics. This function does not modify the \code{P} and \code{U}
#' matrices, making them compatible with \code{scm} objects without enhancement.
#' 
#' @param scm The \code{scm} object to enhance.
#' @return An enhanced \code{scm} object, with additional items:
#' totals for rows, columns, and overall for both the \code{P} matrix
#' and the statistics' associated uncertainties
#' (\code{P_row_totals}, \code{P_col_totals}, \code{P_total},
#' \code{U_row_totals}, \code{U_col_totals}, \code{U_total}),
#' users' and producers' accuracies (\code{P_user_accuracy}, \code{U_user_accuracy},
#' \code{P_producer_accuracy}, \code{U_producer_accuracy}), overall accuracies
#' (\code{P_overall_accuracy}, \code{U_overall_accuracy}) and kappa statistics
#' (\code{P_kappa}, \code{U_kappa}).
#' @export
accuracy.scm = function(scm)
{
    stopifnot(class(scm) == "scm")
    
    P_kp = rowSums(scm$P)
    P_pl = colSums(scm$P)
    P_pp = sum(scm$P)

    U_kp = rowSums(scm$U)
    U_pl = colSums(scm$U)
    U_pp = sum(scm$U)

    P_OA_s = (P_pp*sum(diag(scm$P))) / (P_pp^2 - U_pp^2)
    U_OA_s = (U_pp*sum(diag(scm$P))) / (P_pp^2 - U_pp^2)

    P_UA_s = (diag(scm$P)*P_kp) / (P_kp^2 - U_kp^2) # Problems when we have uncertainty == prediction, division by 0
    U_UA_s = (diag(scm$P)*U_kp) / (P_kp^2 - U_kp^2) # In which case the diagonals are 0, so it's 0 by definition
    P_UA_s[is.nan(P_UA_s)] = 0
    U_UA_s[is.nan(U_UA_s)] = 0

    P_PA_s = (diag(scm$P)*P_pl) / (P_pl^2 - U_pl^2)
    U_PA_s = (diag(scm$P)*U_pl) / (P_pl^2 - U_pl^2)
    P_PA_s[is.nan(P_PA_s)] = 0
    U_PA_s[is.nan(U_PA_s)] = 0

    # Kappa coefficient
    # Expected proportion of agreement (i.e. when using the monkey method)
    P_e = sum(((P_pp^2 + U_pp^2)*(P_pl*P_kp + U_pl*U_kp) - 2 * P_pp*U_pp*(U_pl*P_kp + P_pl*U_kp)) / (P_pp^2-U_pp^2)^2)
    U_e = sum((2 * P_pp*U_pp*(P_pl*P_kp + U_pl*U_kp) - (P_pp^2 + U_pp^2)*(U_pl*P_kp + P_pl*U_kp)) / (P_pp^2-U_pp^2)^2)

    Sign = (1-P_OA_s-U_OA_s)*(1-P_e-U_e)
    P_Kappa_s = ((P_OA_s-P_e) * (1-P_e) - (sign(Sign)*U_OA_s+U_e) * U_e)/((1-P_e)^2-U_e^2)
    U_Kappa_s = ((sign(Sign)*(1-P_OA_s)*U_e + (1-P_e)*U_OA_s)/((1-P_e)^2-U_e^2))
    
    scm[["P_row_totals"]] = P_kp
    scm[["P_col_totals"]] = P_pl
    scm[["P_total"]] = P_pp
    scm[["U_row_totals"]] = U_kp
    scm[["U_col_totals"]] = U_pl
    scm[["U_total"]] = U_pp
    scm[["P_overall_accuracy"]] = P_OA_s
    scm[["U_overall_accuracy"]] = U_OA_s
    scm[["P_user_accuracy"]] = P_UA_s
    scm[["U_user_accuracy"]] = U_UA_s
    scm[["P_producer_accuracy"]] = P_PA_s
    scm[["U_producer_accuracy"]] = U_PA_s
    scm[["P_kappa"]] = P_Kappa_s
    scm[["U_kappa"]] = U_Kappa_s
    
    return(scm)
}
