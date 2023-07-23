

#' Subplot 2 continuous variables on the same plot.
#'
#' @param df 
#' @param predictor_var 
#' @param target_var 
#'
#' @return
#' @export
#'
#' @examples
subplot_cont_cont_vars <- function(df = diamonds,
                                    predictor_var = "carat",
                                    target_var = "price"){
  p1 <- ggplot(df) +
    geom_histogram(aes(x = .data[[predictor_var]]))
  
  p2 <- ggplot(df) +
    geom_boxplot(aes(x = .data[[predictor_var]]))
  
  
  p3 <- ggplot(df) +
    geom_point(aes(x = .data[[predictor_var]], y = .data[[target_var]]))
  
  ggpubr::ggarrange(ggpubr::ggarrange(p1, p2, nrow = 2, heights = c(3, 1)),
                    p3, ncol = 2)
}



#' Make a subplot of discrete variable and continuous variable
#'
#' @param df
#' @param predictor_var
#' @param target_var
#'
#' @return ggplot
#' @export
#'
#' @examples
subplot_disc_cont_vars <- function(df = diamonds,
                                   predictor_var = "cut",
                                   target_var = "price") {
  p1 <- ggplot(df) +
    geom_bar(aes(y = .data[[predictor_var]]))
  p2 <- ggplot(df) +
    geom_bar(
      aes(x = .data[[target_var]],
          y = .data[[predictor_var]]),
      position = "dodge",
      stat = "summary",
      fun = "mean"
    )
  p3 <- ggplot(df) +
    geom_boxplot(aes(x = .data[[target_var]],
                     y = .data[[predictor_var]]))
  
  ggpubr::ggarrange(p1, p2, p3,
                    ncol = 3,
                    common.legend = TRUE)
}
