#'Get probability of toxicity from the 2-parameter logistic model
#  
#'
#' Returns the probability of toxicity from the 2-parameter logistic model,
#' logit(Pr(Y = 1 | X)) = alpha + beta * x, where
#' f is a known transformation of the doses. Ex: f(x) = x means toxicity increases 
#' linearly in doses, f(x) = exp(x) means toxicity increases exponentially in doses
#'
#' @param reference The reference dose
#' @param n_doses  
#' @param alpha 
#' @param beta 
#'
#' @return
#' @export
#'
#' @examples
get_dose_response <- function(reference, n_doses, alpha, beta) {

  dose_seq = seq(1, n_doses, by = 1)
  standardize_dose <- log(dose_seq/dose_seq[reference])
  
  boot::inv.logit(alpha + beta * standardize_dose) %>% 
    setNames(paste0("ptox", 1:5))
  
}
