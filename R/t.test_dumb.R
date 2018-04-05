#' @title t test for dummies
#' @description t test with diagnostics
#' @param data the data to be included
#' @param response the values column
#' @param group the grouping column
#' @return the same object as the t.test with diagnostics included
#' @details To be done
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname t_test_dumb
#' @export

t_test_dumb <-  function(.data,
                         value,
                         group,
                         check_assumptions = TRUE,
                         var.equal = FALSE,
                         ...) {
  fmla <- substitute(value ~ group) %>% formula
  res <- t.test(fmla, data = .data, var.equal = var.equal, ...)
  if (check_assumptions) {
    value <- enquo(value)
    group <- enquo(group)
    my_data <-
      .data %>% select(!!value, !!group) %>% mutate(group = factor(!!group), value = !!value)

    if ((my_data$group %>% levels %>% length) > 2)
      stop("t-test cannot be used for comparing more than two groups. For 3 or more groups use ANOVA")

    # Normal Assumption
    normtest <- function(x) {
      shapiro.test(x)$p.value
    }
    my_norm = my_data %>% group_by(group) %>% summarize(npval = normtest(value))

    if (sum(my_norm$npval < 0.05) > 0) {

      gg = my_data %>% group_by(group) %>% ggplot(aes(sample = value, fill =
                                                        group)) + geom_qq_band(detrend = FALSE) +
        stat_qq_line(detrend = FALSE) +
        stat_qq_point(detrend = FALSE) +
        theme_bw() + labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + facet_wrap( ~
                                                                                               group, scales = "free")
      res$qqplot<- gg +labs(title = "QQPlots",
                            subtitle = " Data may not be from a normal distribution, \n Please look at the qqplots shown to check normality assumption",
                            caption = "")


    } else{

    }

    # Variance Assumption
    if (var.equal) {
      if (var.test(value ~ group, data = my_data)$p.value < 0.05) {

        boxplot_violin <- ggplot(my_data, aes(x=group, y=value)) +
          geom_violin(aes(fill=group, color=group)) +
          geom_boxplot(width=.1, outlier.shape=NA) +
          theme_minimal() +labs(x = substitute(group), y = substitute(value))
        res$varplot <- boxplot_violin+labs(title = "Violin Plots",
                                           subtitle = "\n Equal variances were specified, however based on the data the variances may not be equal. \n Please check the violinplots shown to confirm your assumptions \n",
                                           caption = "")
      }
    }

  }
  res$qqplot %>% print
  res$varplot %>% print
  return(res)
}



