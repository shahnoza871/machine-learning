#' OutputMeasuresBySubsets - Output Model results by Subset
#' @title Output a Model results in {smbinning} Style
#' @description Output a Model results in {smbinning} Style
#'
#' @param Result_Datatable_Name data.table - Dataset of Model results
#' @param Subset character - character string indicating which subset will use. An Оne of 'train' (default), 'test', 'valid'
#' @param Probs vector of numeric - Predicted Probability of Positive Events
#' @param SmClass vector of integer - Reference (True) Value of Positive Events
#' @param ShowPlot logical -  Should Plots be displayed
#' @param N integer - The number of row for output last Plot into MS Excel file
#' @param SizeOf character or integer - string indicating Size of Subsets
#' @param DefRates character - string indicating Default rates in Subsets
#' @param Offset numeric - The Offset for Model in Score Points
#' @param PDO integer - The Points to Double the Odds for Model in Score Points
#' @param limits range of two integers - The First element must be less that The Second Element & every element must be a multiple of The PDO
#' 
#' @return a numeric - A Row  for output plot in MS Excel file
#' 
#' @references \url{https://www.amazon.com/Credit-Scoring-Toolkit-Management-Automation/dp/0199226407}
#' 
#' @example  \dontrun{
#'     OutputMeasuresBySubsets(Result_Datatable_Name = 'result_dt', 
#'                     Subset = 'train',
#'                     Probs = predict(glmFit, newdata = X[split[[paste0(Subset, '_index')]], ], type = 'response'),
#'                     SmClass = Y[split[[paste0(Subset, '_index')]]] %>% as.integer - 1 # Convert to `smbinning` Style
#'                    )
#' }
#'  
#' @export OutputMeasuresBySubsets

OutputMeasuresBySubsets <- function(Result_Datatable_Name, Subset = 'train', Probs, SmClass, ShowPlot = TRUE, N = 0,
                                    SizeOf = '', DefRates = '', Offset = 500, PDO = 50L, limits = NULL) {
  # Load required parameters and packages
  required_packages <- c('checkmate', 'smbinning', 'dplyr', 'ggplot2', 'ggrepel', 'ROCR', 'cowplot')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_string(Subset, .var.name = '`Subset` character string indicating which subset will use.
                           An Оne of "train" (default), "test", "valid"')
  checkmate::assert_numeric(Probs, lower = 0, upper = 1, any.missing = FALSE, all.missing = FALSE, len = length(SmClass),
                            .var.name = '`Probs` vector of numeric - Predicted Probability of Positive Events')
  checkmate::assert_integer(SmClass, lower = 0L, upper = 1L, any.missing = FALSE, all.missing = FALSE, len = length(Probs),
                            .var.name = '`SmClass` vector of integer - Reference (True) Value of Positive Events')
  checkmate::assert_logical(ShowPlot, any.missing = FALSE, all.missing = FALSE, max.len = 1,
                            .var.name = '`ShowPlot` a logical - Should Plots be displayed')
  checkmate::assert_integer(N, lower = 0L, upper = 1000000L, any.missing = FALSE, all.missing = FALSE,
                            .var.name = '`N` an integer - The number of row for output last Plot into MS Excel file')
  # checkmate::assert_string(SizeOf, .var.name = '`SizeOf` character string indicating Size of Subsets.')
  checkmate::assert_string(DefRates, .var.name = '`DefRates` character string indicating Default rates in Subsets.')
  checkmate::assert_numeric(Offset, lower = 0, upper = 1000, any.missing = FALSE, all.missing = FALSE,
                            .var.name = '`Offset` a numeric - The Offset for Model in Score Points.')
  checkmate::assert_integer(PDO, lower = 20L, upper = 1000L, any.missing = FALSE, all.missing = FALSE,
                            .var.name = '`PDO` an integer - The Points to Double the Odds for Model in Score Points.')
  
  if (is.vector(limits) == TRUE) { 
    if (length(limits) != 2L) 
      stop('"limits": Length of the limits must be 2 and every element must be a multiple of The PDO.', call. = FALSE)
    if (limits[1] >= limits[2]) 
      stop('"limits": The First element must be less that The Second element & every element must be a multiple of The PDO.', call. = FALSE) 
  } else { 
    Threshold <- 
      data.frame(Probs = Probs, SmClass = SmClass) %>% 
      na.omit() %$%
      ifelse(Bad_Is_1 == TRUE, 1 - mean(Probs), mean(Probs))
    
    limits <-
      list( c(-9L, 0L), c(-8L, 1L), c(-7L, 2L), c(-6L, 3L), c(-5L, 4L),
            c(-4L, 5L), c(-3L, 6L), c(-2L, 7L), c(-1L, 8L), c(0L, 9L) )[[
              dplyr::case_when(
                Threshold < 0.10 ~ 1,
                Threshold < 0.20 ~ 2,
                Threshold < 0.30 ~ 3,
                Threshold < 0.40 ~ 4,
                Threshold < 0.50 ~ 5,
                Threshold < 0.60 ~ 6,
                Threshold < 0.70 ~ 7,
                Threshold < 0.80 ~ 8,
                Threshold < 0.90 ~ 9,
                Threshold < 1.00 ~ 10 )
            ]] * PDO + Offset
  }   # The End of if (is.vector(limits) == TRUE) 
  
  if (!is.null(Result_Datatable_Name)) {
    if (exists(Result_Datatable_Name)) result_dt <- 
        data.table::copy( base::get(Result_Datatable_Name) ) %>% data.table::setDT()
    if ( !is.null(result_dt) ) checkmate::assert_data_frame(result_dt, min.rows = 0L, ncol = 15L,
                                                            .var.name = '`result_dt` data.table - Dataset of Model results')
  }  # if (!is.null(Result_Datatable_Name))
  
  Name_Subset <- 
    dplyr::case_when(
      Subset == 'train'      ~ ifelse( exists('TimeBoundaries') == TRUE, sprintf( 'обучающая (`train`) за период [%s - %s]', 
                                                                                  TimeBoundaries[[ Subset ]][['Start']], TimeBoundaries[[ Subset ]][['Finish']] ), 
                                       'обучающая (`train`)' ),
      Subset == 'test'       ~ ifelse( exists('TimeBoundaries') == TRUE, sprintf( 'проверочная (`test`) за период [%s - %s]', 
                                                                                  TimeBoundaries[[ Subset ]][['Start']], TimeBoundaries[[ Subset ]][['Finish']] ), 
                                       'проверочная (`test`)' ),
      Subset == 'validation' ~ sprintf( 'ретро-валидация (`validation`) за период [%s - %s]', 
                                        ifelse(is.null(DF$startdate) == TRUE, '', format(min(DF$startdate), '%Y-%m-%d')), 
                                        ifelse(is.null(DF$startdate) == TRUE, '', format(max(DF$startdate), '%Y-%m-%d')) ),
      TRUE                   ~ ifelse( exists('TimeBoundaries') == TRUE, sprintf( 'контрольная (`valid`) за период [%s - %s]', 
                                                                                  TimeBoundaries[[ Subset ]][['Start']], TimeBoundaries[[ Subset ]][['Finish']] ), 
                                       paste('контрольная (`valid`) - с', format(as.Date(params$Validation_Date),
                                                                                 '%Y-%B-%d')) )
    )
  
  # # https://www.quora.com/What-are-the-inverse-functions-of-these-f-x-x-1-x
  # Probs = 1 - 2^( (ScorePoints - Offset) / PDO ) /
  #         ( 2^( (ScorePoints - Offset) / PDO ) + 1 )
  
  df <- 
    data.frame(Y           = SmClass,  # Converted to `smbinning` Style
               ScorePoints = if (Bad_Is_1 == TRUE) {
                 Offset + PDO * log2((1 - Probs) / (Probs))
               } else { 
                 Offset + PDO * log2((Probs) / (1 - Probs))
               } )
  
  if (length( table(df$Y) ) == 1L) {   # "'SmClass' must be binary (0/1)."
    if (Subset == 'train' | Subset == 'validation') {
      base::assign( Result_Datatable_Name, # Both <<- and assign will work into 'result_dt', 'Provider_result_dt' or 'Bucket_result_dt'
                    rbind( result_dt, data.table::data.table(
                      Name   = ModelName
                      , Gini1  = NA_real_, KS1 = NA_real_, Sens1 = NA_real_, Spec1 = NA_real_
                      , Gini2  = NA_real_, KS2 = NA_real_, Sens2 = NA_real_, Spec2 = NA_real_
                      , Gini3  = NA_real_, KS3 = NA_real_, Sens3 = NA_real_, Spec3 = NA_real_
                      , Note   = SizeOf
                      , DefRat = DefRates
                    ) ),
                    base::globalenv() )
      if (exists('Gini')) base::assign( 'Gini', Gini, base::globalenv() )   # Both <<- and assign will work
    }  # The End of if (Subset == 'train' | Subset == 'validation')
    return(N)
  }    # The End of if (length( table(df$Y) ) == 1L)
  
  predROC <- ROCR::prediction(predictions = Probs, labels = SmClass)
  Sens.model <- ROCR::performance(predROC, measure = 'sens', x.measure = 'cutoff')
  Spec.model <- ROCR::performance(predROC, measure = 'spec', x.measure = 'cutoff')
  # cutoff that yields the highest sensitivity plus specificity - https://stackoverflow.com/questions/35731526/calculate-the-optimal-max-sensitivity-and-specificity-cut-off-value-using-r
  optcut <- Sens.model@x.values[[1]][ which.max(Sens.model@y.values[[1]] + Spec.model@y.values[[1]]) ]
  # Sens.model@x.values[[1]][ which.min(abs(Sens.model@y.values[[1]]-Spec.model@y.values[[1]])) ]
  optcutcomment <- ' (Optimal {Yourden Index} point)'
  
  aucROC <- ROCR::performance(predROC, measure = 'auc')
  Gini <- (aucROC@y.values[[1]] - 0.5) * 2
  auc <- aucROC@y.values[[1]]
  
  # AUC Evaluation
  auceval <- ifelse(auc < 0.6, 'Unpredictive',
                    ifelse(auc < 0.7, 'Poor',
                           ifelse(auc < 0.8, 'Fair',
                                  ifelse(auc < 0.9, 'Good', 'Excellent')))) 
  
  aucROC <- ROCR::performance(predROC, measure = 'tpr', x.measure = 'fpr')
  KS <- max( attr(aucROC, 'y.values')[[1]] - attr(aucROC, 'x.values')[[1]] )
  MaxKS <<- KS
  
  # KS Evaluation
  kseval <- ifelse(KS < 0.3, 'Unpredictive',
                   ifelse(KS < 0.4, 'Fair',
                          ifelse(KS < 0.5, 'Good',
                                 ifelse(KS < 0.6, 'Excellent', 
                                        ifelse(KS < 0.7, 'Awesome', 'That Good. Really?')))))
  
  nmiss <- nrow(df)-nrow(na.omit(df)) # Missing rows
  df$Prediction <- if (Bad_Is_1 == TRUE) ifelse(Probs >= optcut, 1L, 0L) else ifelse(Probs < optcut, 0L, 1L)
  CM <- 
    table(Prediction  = df$Prediction, Reference = df$Y)
  tp <- CM[4] 
  fp <- CM[2]
  fn <- CM[3]
  tn <- CM[1]
  p <- CM[4]+CM[3]
  n <- CM[2]+CM[1]
  BaseLine <-   # Overall Badrate, but need Partical Overall - smbmetrics_df$InvPrecision[ which.min(smbmetrics_df$Sensitivity) ]
    ifelse(Bad_Is_1 == TRUE, 1 - (fp+tn)/(p+n), (fp+tn)/(p+n))
  
  optcut <- if (Bad_Is_1 == TRUE) Offset + PDO * log2((1 - optcut) / (optcut)) else Offset + PDO * log2((optcut) / (1 - optcut))
  OptCutOff <<- optcut
  recsabovecutoff <- nrow(df[df$ScorePoints >= optcut, ]) / nrow(df)
  D_crit = sqrt( - log(0.01 / 2, base = exp(1)) * (1 +  p /  n) / (2 * p) )
  
  if (ShowPlot == TRUE) {
    # Report on Metrics
    admetrics=character()
    admetrics=paste0(admetrics, "\n")
    admetrics=paste0(admetrics, sprintf('\n  %s: %s \n', params$Label, Name_Subset))
    admetrics=paste0(admetrics, "  -------------------------------------------------- \n")
    admetrics=paste0(admetrics, "                  Gini : ",sprintf("%.4f",round(Gini,4))," (", auceval,")\n")
    admetrics=paste0(admetrics, "                    KS : ",sprintf("%.4f",round(KS,4))," (", kseval,")\n")
    admetrics=paste0(admetrics, " D (α=0.01 for ", p, " & ", n, ") : ",sprintf("%.4f",round(D_crit,4))," Two-sample Kolmogorov–Smirnov Сriterion.
    The Hypothesis of Homogeneity of the Two Samples is ", ifelse(KS > D_crit, "REJECTED", "ACCEPTED"), ".\n")
    # admetrics=paste0(admetrics, "                   AUC : ",sprintf("%.4f",round(auc,4))," (", auceval,")\n")
    admetrics=paste0(admetrics, "\n")
    admetrics=paste0(admetrics, "  Classification Matrix \n")
    admetrics=paste0(admetrics, "  -------------------------------------------------- \n")
    admetrics=paste0(admetrics, "           Cutoff (>=) : ",round(as.numeric(optcut),4), optcutcomment,"\n")
    admetrics=paste0(admetrics, "   True Positives (TP) : ",tp,"\n")
    admetrics=paste0(admetrics, "  False Positives (FP) : ",fp,"\n")
    admetrics=paste0(admetrics, "  False Negatives (FN) : ",fn,"\n")
    admetrics=paste0(admetrics, "   True Negatives (TN) : ",tn,"\n")
    admetrics=paste0(admetrics, "   Total Positives (P) : ",p,"\n")
    admetrics=paste0(admetrics, "   Total Negatives (N) : ",n,"\n")
    admetrics=paste0(admetrics, "\n")
    admetrics=paste0(admetrics, "  Business/Performance Metrics \n")
    admetrics=paste0(admetrics, "  -------------------------------------------------- \n")
    admetrics=paste0(admetrics, "      %Records>=Cutoff : ",sprintf("%.4f",round(recsabovecutoff,4)),"\n")
    admetrics=paste0(admetrics, "             Good Rate : ",sprintf("%.4f",round((fp+tn)/(p+n),4))," (Overall)\n")
    admetrics=paste0(admetrics, " Bad Rate (Prevalence) : ",sprintf("%.4f",round((fn+tp)/(p+n),4))," (Overall)\n")
    admetrics=paste0(admetrics, "        Accuracy (ACC) : ",sprintf("%.4f",round((tp+tn)/(tp+fp+tn+fn),4)),"\n")
    admetrics=paste0(admetrics, "     Sensitivity (TPR) : ",sprintf("%.4f",round(tp/p,4)),"\n")
    admetrics=paste0(admetrics, " False Neg. Rate (FNR) : ",sprintf("%.4f",round(fn/p,4)),"\n")
    admetrics=paste0(admetrics, " False Pos. Rate (FPR) : ",sprintf("%.4f",round(fp/n,4)),"\n")
    admetrics=paste0(admetrics, "     Specificity (TNR) : ",sprintf("%.4f",round(tn/n,4)),"\n")
    admetrics=paste0(admetrics, "       Precision (PPV) : ",sprintf("%.4f",round(tp/(tp+fp),4)),"\n")
    admetrics=paste0(admetrics, "  False Discovery Rate : ",sprintf("%.4f",round(fp/(tp+fp),4)),"\n")
    admetrics=paste0(admetrics, "    False Omision Rate : ",sprintf("%.4f",round(fn/(fn+tn),4)),"\n")
    admetrics=paste0(admetrics, "  Inv. Precision (NPV) : ",sprintf("%.4f",round(tn/(fn+tn),4)),"\n")
    admetrics=paste0(admetrics, " F1 score (Dice coef.) : ",sprintf("%.4f",round(2*tp/(2*tp+fp+fn),4)),"\n")
    admetrics=paste0(admetrics,"\n")
    admetrics=paste0(admetrics, "  Note: ",nmiss," rows deleted due to missing data.\n")
    admetrics=paste0(admetrics,"\n")
    admetrics=gsub(", ","",admetrics)
    
    # Metric report
    cat(admetrics)
    
    # ROC & PRC Curves
    pd <- data.frame(fpr = unlist(aucROC@x.values), tpr = unlist(aucROC@y.values))
    
    youden_index_df <- data.frame( FPR = 1 - tn/n, TPR = tp/p, Recall = tp/(tp + fp)
                                   , Legend_Label = paste0("Youden Index (", round2(OptCutOff), ")") )
    
    p1 <-  # Show ROC Curve
      # autoplot(lgb_learner$predict(task = full_tsk, row_ids = split[[paste0(Subset, '_index')]]), 'roc' ) +
      ggplot2::ggplot(pd, aes(x = fpr, y = tpr)) +
      ggplot2::geom_line(colour = 'red', linewidth = 1.5) +
      ggplot2::geom_line(data = data.frame(), aes(x = c(0, 1), y = c(0, 1)), colour = 'grey', linetype = 'dashed') +
      ggplot2::geom_line(data = data.frame(), aes(x = c(0, 1), y = c(1, 0)), colour = 'grey', linetype = 'dotted') +
      ggplot2::geom_line(data = data.frame(), aes(x = c(0, 1 - tn/n), y = c(tp/p, tp/p)), colour = 'grey', linetype = 'dotted') +
      ggplot2::geom_line(data = data.frame(), aes(x = c(1 - tn/n, 1 - tn/n), y = c(0, tp/p)), colour = 'grey', linetype = 'dotted') +
      ggplot2::geom_point(data = youden_index_df, aes(x = FPR, y = TPR), color = 'magenta', size = 5, shape = 3,
                          stroke = 1.5) +
      ggplot2::geom_text(data = youden_index_df, aes(x = FPR, y = TPR, label = Legend_Label),
                         nudge_x = 0.05, nudge_y = 0.05, color = 'magenta', size = 4) +
      ggplot2::scale_x_continuous(breaks = seq( 0, 1, 0.2)) +
      ggplot2::scale_y_continuous(breaks = seq( 0, 1, 0.2)) +
      ggplot2::annotate(geom = 'text', x = 0.8, y = 0.1, label = sprintf('Gini: %4.2f%%', Gini * 100), size = 5) +
      ggplot2::annotate(geom = 'text', x = 0.6, y = 0.0, label = paste0(params$Label, ': ', Name_Subset), size = 3) +
      # ggplot2::annotate(geom = 'text', x = 1 - tn/n + 0.19, y = tp/p, label = 'Optimal (Yourden Index) point', size = 3) +
      ggplot2::theme_classic() +
      ggplot2::theme(panel.border =  element_rect(fill = NA, colour = 'grey20'),
                     panel.grid.major = element_line(colour = 'grey90'), aspect.ratio = 1,
                     plot.title = element_text(face = 'bold')) +  # , hjust = 0.5
      ggplot2::labs(title = sprintf('ROC Curve - N:%4.0f, P:%4.0f',
                                    table(SmClass) %>% .[1], table(SmClass) %>% .[2]),
                    x = 'False Positive Rate (1 - Specificity)', 
                    y = 'True Positive Rate (Sensitivity)')
    
    prcROC <- ROCR::performance(predROC, measure = 'prec', x.measure = 'rec')
    pd <- data.frame(rec = unlist(prcROC@x.values), prec = unlist(prcROC@y.values))
    pd <- na.omit(pd)
    
    p2 <- # Show Precision / Recall (Sensitivity) Curve
      # autoplot(lgb_learner$predict(task = full_tsk, row_ids = split[[paste0(Subset, '_index')]]), 'prc' )+
      ggplot2::ggplot(pd, aes(x = rec, y = prec)) +
      ggplot2::geom_line(colour = 'dodgerblue', linewidth = 1.5) +
      ggplot2::geom_line(data = data.frame(), aes(x = c(0, 1), y = c(BaseLine, BaseLine)), colour = 'grey', linetype = 'dashed') +
      ggplot2::geom_line(data = data.frame(), aes(x = c(0, 1), y = c(tp/(tp + fp), tp/(tp + fp))), colour = 'grey', linetype = 'dotted') +
      ggplot2::geom_line(data = data.frame(), aes(x = c(tp/p, tp/p)), y = c(0, 1), colour = 'grey', linetype = 'dotted') +
      ggplot2::geom_point(data = youden_index_df, aes(x = TPR, y = Recall), color = 'magenta', size = 5, shape = 3,
                          stroke = 1.5) +
      ggplot2::scale_x_continuous(breaks = seq( 0, 1, 0.2)) +
      ggplot2::annotate( geom = 'text', x = 0.22, y = ifelse(BaseLine > 0.95, BaseLine + 0.001, BaseLine + 0.03)
                         , label = sprintf('Baseline Classifier = %0.4f', BaseLine), size = 3 ) +
      ggplot2::annotate( geom = 'text', x = 0.86, y = ifelse(tp/(tp + fp) > 0.95, tp/(tp + fp) + 0.001, tp/(tp + fp) + 0.03)
                         , label = sprintf('%s Precision = %0.4f', stringr::str_to_title(Subset), tp/(tp + fp)), size = 3 ) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::theme_classic() +
      ggplot2::theme(panel.border =  element_rect(fill = NA, colour = 'grey20'),
                     panel.grid.major = element_line(colour = 'grey90'), aspect.ratio = 1,
                     plot.title = element_text(face = 'bold')) +   # , hjust = 0.5
      ggplot2::labs(title = sprintf('Precision / Recall Curve - N:%4.0f, P:%4.0f',
                                    table(SmClass) %>% .[1], table(SmClass) %>% .[2]),
                    x = 'Recall (True Positive Rate or Sensitivity)',
                    y = 'Precision (Positive Predictive Value)')
    
    # print( gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(0.54, 0.46)) )
    # print( patchwork::wrap_plots(p1, p2, widths = c(0.50, 0.50)) )
    print(   cowplot::plot_grid(p1, p2, align = 'h', rel_widths = c(0.50, 0.50)) )
  }   # The End of if (ShowPlot == TRUE)
  
  if (ShowPlot == TRUE) { # Print
    if (N != 0 & exists('wb')) {
      if (!requireNamespace('openxlsx', quietly = TRUE)) 
        stop('"openxlsx" is needed for this function to work. Install it via install.packages("openxlsx")', call. = FALSE)
      openxlsx::insertPlot(wb, sheet = Scorecard, xy = c(11, N),
                           width = 24, height = 10, fileType = 'png', units = 'cm')  
      N <- N + 17L
    }   # The End of if (N != 0 & exists('wb'))
    
    l2 <-
      KSCurveShow(Preds = Probs,  SmClass = SmClass,                                   # `Good` Class Probabilities - numeric vector
                  NameOfModel = ModelName,                             # Name Of The Model
                  NameOfSet = stringr::str_to_title(Subset),           # Name of Dataset
                  Name_Subset = Name_Subset)                           # Full Name of Subset
    print( l2[['KS Chart']] )
    
    if (N != 0 & exists('wb')) {
      openxlsx::insertPlot(wb, sheet = Scorecard, xy = c(11, N),
                           width = 24, height = 10, fileType = 'png', units = 'cm')  
      N <- N + 17L
    } # The End of if (N != 0 & exists('wb'))
  }  # The End of if (ShowPlot == TRUE)
  
  if (!is.null(Result_Datatable_Name)) {
    if (Subset == 'train') {
      base::assign( Result_Datatable_Name, # Both <<- and assign will work into 'result_dt' or 'Peg_result_dt'
                    rbind( result_dt, data.table::data.table(
                      Name   = ModelName
                      , Gini1  = round( Gini, 4)
                      , KS1    = round( KS, 4)
                      , Sens1  = round( ifelse(identical((tp == fn), logical(0)), NA_real_, tp / (tp + fn)), 4 )  # as.numeric(CM$byClass['Sensitivity'])
                      , Spec1  = round( ifelse(identical((tn == fp), logical(0)), NA_real_, tn / (tn + fp)), 4 )  # as.numeric(CM$byClass['Specificity'])
                      , Gini2  = NA_real_, KS2 = NA_real_, Sens2 = NA_real_, Spec2 = NA_real_
                      , Gini3  = NA_real_, KS3 = NA_real_, Sens3 = NA_real_, Spec3 = NA_real_
                      , Note   = SizeOf
                      , DefRat = DefRates
                    ) ),
                    base::globalenv() )
      base::assign( 'Gini', Gini, base::globalenv() )   # Both <<- and assign will work
    }    # The End if (Subset == 'train')
    
    if ( (Subset == 'test' & nrow(result_dt) == 0L) | Subset == 'valid' & nrow(result_dt) == 0L ) {
      result_dt <- rbind( result_dt, data.table::data.table(
        Name   = ModelName
        , Gini1  = NA_real_, KS1 = NA_real_, Sens1 = NA_real_, Spec1  = NA_real_
        , Gini2  = NA_real_, KS2 = NA_real_, Sens2 = NA_real_, Spec2 = NA_real_
        , Gini3  = NA_real_, KS3 = NA_real_, Sens3 = NA_real_, Spec3 = NA_real_
        , Note   = SizeOf
        , DefRat = DefRates
      ) )
      base::assign( Result_Datatable_Name, # Both <<- and assign will work into 'result_dt' or 'Peg_result_dt'
                    result_dt, base::globalenv() )
      base::assign( 'Gini', Gini, base::globalenv() )   # Both <<- and assign will work
    }    # The End if ( (Subset == 'test' & nrow(result_dt) == 0L) ! Subset == 'valid' & nrow(result_dt) == 0L )
    
    if (Subset == 'test') {
      if (nrow(result_dt) == 0L) return(N)
      result_dt[ Name == ModelName,
                 `:=` ( Gini2  = round( Gini, 4)
                        , KS2    = round( KS, 4)
                        , Sens2  = round( ifelse(identical((tp == fn), logical(0)), NA_real_, tp / (tp + fn)), 4 )
                        , Spec2  = round( ifelse(identical((tn == fp), logical(0)), NA_real_, tn / (tn + fp)), 4 )
                 ) ]
      base::assign( Result_Datatable_Name, result_dt, base::globalenv() )   # Both <<- and assign will work
    }    # The End of if (Subset == 'test')
    
    if (Subset == 'valid') {
      result_dt[ Name == ModelName,
                 `:=` ( Gini3  = round( Gini, 4)
                        , KS3    = round( KS, 4)
                        , Sens3  = round( ifelse(identical((tp == fn), logical(0)), NA_real_, tp / (tp + fn)), 4 )
                        , Spec3  = round( ifelse(identical((tn == fp), logical(0)), NA_real_, tn / (tn + fp)), 4 )
                 ) ]
      base::assign( Result_Datatable_Name, result_dt, base::globalenv() )   # Both <<- and assign will work
    }   # The End of if (Subset == 'valid')
    
    if (Subset == 'validation') {
      base::assign( Result_Datatable_Name, # Both <<- and assign will work into 'result_dt', 'Provider_result_dt' or 'Bucket_result_dt'
                    rbind( result_dt, data.table::data.table(
                      Name   = ModelName
                      , Gini1  = NA_real_, KS1 = NA_real_, Sens1 = NA_real_, Spec1 = NA_real_
                      , Gini2  = NA_real_, KS2 = NA_real_, Sens2 = NA_real_, Spec2 = NA_real_
                      , Gini3  = round( Gini, 4 )
                      , KS3    = round( KS, 4 )
                      , Sens3  = round( ifelse(identical((tp == fn), logical(0)), NA_real_, tp / (tp + fn)), 4 )  # as.numeric(CM$byClass['Sensitivity'])
                      , Spec3  = round( ifelse(identical((tn == fp), logical(0)), NA_real_, tn / (tn + fp)), 4 )  # as.numeric(CM$byClass['Specificity'])
                      , Note   = SizeOf
                      , DefRat = DefRates
                    ) ),
                    base::globalenv() )
    }  # The End of if (Subset == 'validation')
  }  # The end of if (!is.null(result_dt))
  
  if (ShowPlot == TRUE) {
    # Distribution's Curve of Predicted Score Points by Good & Bad Class for Model
    if (Gini != 1) {
      CM_Components <- list( TP = tp, TN = tn, FP = fp, FN = fn, CutOff = as.integer(round(optcut, digits = 0)) )
      suppressWarnings(
        l3 <-
          ScoresCurveShow(Preds = Probs,                                       # `Good` Class Probabilities - numeric vector
                          # Observed Classes (Reference) - factor vector
                          Obsers = factor(SmClass, labels = if (Bad_Is_1 == TRUE) c('Good', 'Bad') else c('Bad', 'Good')),
                          NameOfModel = ModelName,                             # Name Of The Model
                          NameOfSet = stringr::str_to_title(Subset),           # Name of Dataset
                          Offset = Offset,                                     # Offset in Scorecard
                          PDO = PDO,                                           # Points to Double the Odds in Scorecard
                          CM_Components = CM_Components )                      # Confusion Matrix Components
      ) 
    }    # The End of if (Gini != 1)
    
    # Distribution of Bins Predicted of Scores by Good & Bad Class for Model
    suppressWarnings(
      l4 <-
        ScorePoinsbyBinShow(Preds = Probs,                                       # `Good` Class Probabilities - numeric vector
                            # Observed Classes (Reference) - factor vector
                            Obsers = factor(SmClass, labels = if (Bad_Is_1 == TRUE) c('Good', 'Bad') else c('Bad', 'Good')),
                            NameOfModel = ModelName,                             # Name Of The Model
                            NameOfSet = stringr::str_to_title(Subset),           # Name of Dataset
                            Offset = Offset,                                     # Offset in Scorecard
                            PDO = PDO,                                           # Points to Double the Odds in Scorecard
                            limits = limits)                                     # Limits of Low & High Margins
    )
    
    if (Gini !=1 ) print( l3[['ScoresCurve Chart']] )
    print( l4[['ScorePoins Chart']] )
    print( l4[['ScorePoins Bins']] )
    
  }   # The End of if (ShowPlot == TRUE) 
  
  return(N)
  
}   # The End of function `OutputMeasuresBySubsets()`


#' KSCurveShow - the Kolmogorov–Smirnov test (KS test) is a nonparametric test of the equality of continuous, one-dimensional probability distribution
#' 
#' @title Chart for Kolmogorov-Smirnov Test
#' @description Generate Kolmogorov-Smirnov Chart for the Predictions & References
#'
#' @param Preds Numeric Vector - 'Good' Class Probabilities
#' @param SmClass vector of integer - Reference (True) Value of Positive Events
#' @param NameOfModel character - Name of Model
#' @param NameOfSet character - Name of Dataset in Model
#' @param Name_Subset character - Full Name of Subset
#'
#' @return A list with An object of class \code{ggplot} from \code{ggplot2} package and KS Statictis Number
#' 
#' @references \url{https://github.com/Microsoft/acceleratoRs/blob/master/CreditRiskPrediction/Code/CreditRiskPrediction.Rmd}
#' 
#' @example  \dontrun{
#' KSCurveShow(Preds = preds[NameOfTheBestModel],
#'             SmClass = as.integer(Y[split$test_index]) - 1L,
#'             NameOfModel = 'The Best Model',
#'             NameOfSet = 'test')
#' }
#'  
#' @export KSCurveShow

KSCurveShow <- function(Preds, SmClass, NameOfModel = NULL, wb = NULL, xy = c(12, 18),
                        output = TRUE, NameOfSet = 'test', Name_Subset = '') {
  
  # Load required packages and parameters
  required_packages <- c('checkmate', 'ggplot2', 'ROCR')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_numeric(Preds, lower = 0L, upper = 1L, .var.name = '`Good` Class Probabilities - numeric vector')
  checkmate::assert_integer(SmClass, lower = 0L, upper = 1L, any.missing = FALSE, all.missing = FALSE, len = length(Preds),
                            .var.name = '`SmClass` vector of integer - Reference (True) Value of Positive Events')
  
  predROC <- 
    ROCR::prediction(Preds,               # `Good` Class Probabilities - numeric vector
                     SmClass)             # Integers of Observed Classes
  
  levels(predROC@labels[[1]]) <- if (Bad_Is_1 == TRUE) c('Good', 'Bad') else c('Bad', 'Good')
  score1 <- predROC@predictions[[1]][predROC@labels[[1]] == 'Good']
  score2 <- predROC@predictions[[1]][predROC@labels[[1]] == 'Bad']
  Reference <- c(rep('Good', length(score1)), rep('Bad', length(score2)))
  dat <- data.frame(KSD = c(score1, score2), Reference = Reference)
  
  # Create ECDF of data
  cdf1 <- ecdf(score1) 
  cdf2 <- ecdf(score2) 
  
  # Find min and max statistics to draw line between points of greatest distance
  minMax <- seq(min(score1, score2), max(score1, score2), length.out = length(score1))
  
  # Find the predicted probability where the cumulative distributions have the biggest difference. 
  ks <- max(abs(cdf1(minMax) - cdf2(minMax)))
  
  diff_df <- data.frame(x = minMax, diff = abs(cdf1(minMax) - cdf2(minMax)), line = 'Diff. of \nSets')
  # Show K-S Curve
  dat <-
    dat %>% 
    { if (nrow(.) > 9999L) dplyr::slice_sample(., n = 9999L) else . }
  # # see: https://stackoverflow.com/questions/30604107/r-conditional-evaluation-when-using-the-pipe-operator - 01.10.21
  # purrr::when( nrow(.) > 5000L ~
  #                dplyr::slice_sample(., n = 5000L), # Get only 5000 cases
  #              ~ .)
  
  x0 <- minMax[which(abs(cdf1(minMax) - cdf2(minMax)) == ks)] 
  y0 <- cdf1(x0)
  y1 <- cdf2(x0)
  if (mean(score1) < 0.5) {
    Coords <- data.frame( x = c(0.9, 0.85, 1.0) * max(Preds), y = c(0.12, 0.05, 0.2) )
  } else {
    Coords <- data.frame( x = c(0.1, 0.15, 0.05) * max(Preds), y = c(0.88, 0.95, 0.5) )
  }
  p <- ggplot2::ggplot(dat, aes(x = KSD, group = Reference, color = Reference)) +
    ggplot2::stat_ecdf(linewidth = 1) +
    ggplot2::geom_line(data = diff_df, aes(x = x, y = diff, color = 'Сurve\nof K-S\nstatistic'), 
                       linetype = 'dotted',  linewidth = 0.5, inherit.aes = FALSE) +
    ggplot2::geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1], color = 'Max of\nK-S\nstatistic'),
                          linetype = 'longdash',  linewidth = 1.0) +
    ggplot2::geom_point(aes(x = x0[1], y = y0[1]), color = '#00BFC4', shape = 25, size = 3) +
    ggplot2::geom_point(aes(x = x0[1], y = y1[1]), color = '#F8766D', shape = 24, size = 3) +
    ggplot2::scale_color_manual(values = c('Bad' = '#F8766D', 'Good' = '#00BFC4', 'Сurve\nof K-S\nstatistic' = 'grey50',
                                           'Max of\nK-S\nstatistic' = '#D01D0D')
                                , breaks = c('Bad', 'Good', 'Сurve\nof K-S\nstatistic', 'Max of\nK-S\nstatistic')) +
    # ggplot2::scale_x_continuous(breaks = seq( 0, 1, 0.2)) +
    ggplot2::scale_y_continuous(breaks = seq( 0, 1, 0.2)) +
    ggplot2::annotate(geom = 'text', x = Coords$x[1], y = Coords$y[1], size = 5, label = sprintf('K-S: %4.2f%%', ks * 100)) +
    ggplot2::annotate(geom = 'text', x = Coords$x[2], y = Coords$y[2], label = paste0(params$Label, ': ', Name_Subset), size = 3) +
    ggplot2::coord_cartesian(xlim = c(0, max(Preds))) +
    ggplot2::theme_classic() + 
    ggplot2::theme(panel.border =  element_rect(fill = NA, colour = 'grey20'),
                   axis.text.x = element_text(hjust = 0.5),
                   panel.grid.major = element_line(colour = 'grey90'), plot.title = element_text(face = 'bold'),
                   legend.position = c( Coords$x[1] / max(Preds),  Coords$y[3]), legend.justification = c(1, 0)
    ) + # Position Legend - top-left corner
    ggplot2::labs(title = sprintf('Cumulitive Distibution of Goods/Bads - N:%4.0f, P:%4.0f', 
                                  table(SmClass) %>% .[1], table(SmClass) %>% .[2]),
                  subtitle = sprintf('Model: %s on %s set', NameOfModel,  #  (Kolmogorov-Smirnov Statistic = %0.4f)
                                     stringr::str_to_title(NameOfSet)),   # , ks
                  x = paste('Probability of', target_name, ifelse(Bad_Is_1 == TRUE, '- Class `Bad` (1)', '- Class `Bad` (0)') ), 
                  y = 'Cumulitive Distibution')
  
  # if(!is.na(NameOfModel)) {
  #   p <- p + labs(caption = NameOfModel)
  # }
  
  out <- list(
    'KS Test'  = ks,
    'KS Chart' = p
  )
  
  return( out )
  
}   # The End of Function `KSCurveShow()`

#' ScoresCurveShow - Distribution of Score Points by Good & Bad Class
#' 
#' @title Chart for Diveregence of Score Points 
#' @description Distribution of Score Points by Good & Bad Class
#'
#' @param Preds Vector of numeric - 'Good' Class Probabilities
#' @param Obsers Vector of factor - Observed Classes (Reference) 'Bad' & 'Good'
#' @param NameOfModel character - Name of Model
#' @param wb a workbook - Workbook object from \code{openxlsx} package containing a worksheet for Chart for Diveregence of Score Points
#' @param xy Vector of integer - A vector of the form c(startCol, startRow) for workbook
#' @param output a logical - if must output value of Kullback-Leibler’s Divergence
#' @param NameOfSet character - Name of Dataset in Model
#' @param Offset numeric - The Offset for Model in Score Points
#' @param PDO integer - The Points to Double the Odds for Model in Score Points
#' @param CM_Components list - Confusion Matrix Components (TP, TN, FP, FN, CutOff)
#' 
#' @return A list with An object of class \code{ggplot} from \code{ggplot2} package and Divergence Coefficient Number
#' 
#' @references \url{https://www.amazon.com/Credit-Scoring-Toolkit-Management-Automation/dp/0199226407}
#' 
#' @example  \dontrun{
#' ScoresCurveShow(Preds = preds[NameOfTheBestModel],
#'                 Obsers = Y[inTest],
#'                 NameOfModel = 'The Best Model')
#' }
#'  
#' @export ScoresCurveShow

ScoresCurveShow <- function(Preds, Obsers, NameOfModel = NULL, wb = NULL, xy = c(11, 37),
                            output = TRUE, NameOfSet = 'Test Set', Offset = 500, PDO = 50L, CM_Components) {
  
  # Load required packages and parameters
  required_packages <- c('checkmate', 'dplyr', 'ggplot2', 'janitor', 'philentropy')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  if ( checkmate::test_data_frame(Preds) ) {
    Probs <- Preds %>% pull  # For Predictions from 'caret' package
  } else {
    Probs <- Preds           # for Predictions from simple GLM or lightGBM
  }
  
  checkmate::assert_numeric(Probs, lower = 0L, upper = 1L, .var.name = '`Good` Class Probabilities - numeric vector')
  checkmate::assert_atomic_vector(Obsers, .var.name = 'Observed Classes (Reference) - factor vector')
  checkmate::assert_factor( Obsers, .var.name = 'Observed Classes (Reference) - factor vector' )
  checkmate::assert_numeric(Offset, lower = 0, upper = 1000, any.missing = FALSE, all.missing = FALSE,
                            .var.name = '`Offset` an numeric - The Offset for Model in Score Points.')
  checkmate::assert_integer(PDO, lower = 20L, upper = 1000L, any.missing = FALSE, all.missing = FALSE,
                            .var.name = '`PDO` an integer - The Points to Double the Odds for Model in Score Points.')
  checkmate::assert_list(CM_Components, any.missing = FALSE, len = 5L, #  types = c('integer'), 
                         .var.name = '`CM_Components` an integer list - Confusion Matrix Components (TP, TN, FP, FN, CutOff).')
  
  # // Anderson, R. The credit scoring toolkit: theory and practice for retail credit risk management and decision automation. [Text] – New York: Oxford University press, 2007. – 428 p. – ISBN 0199226407 - https://www.twirpx.com/file/901319/
  
  # // Сорокин А. С. Построение скоринговых карт с использованием модели логистической регрессии // Интернет-журнал «НАУКОВЕДЕНИЕ» Выпуск 2, март – апрель 2014. https://naukovedenie.ru/PDF/180EVN214.pdf
  
  # // Сорокин А. С. К вопросу валидации модели логистической регрессии в кредитном скоринге // Интернет-журнал «НАУКОВЕДЕНИЕ» Выпуск 2, март – апрель 2014. https://naukovedenie.ru/PDF/173EVN214.pdf
  
  # # Odds to scaled score conversion
  # 
  # S =	700; D = 16; G = 2; I =	50
  # 
  # # Source: Anderson, R. (2007). The credit scoring toolkit: Theory and practice for retail credit risk management and decision automation. p. 428
  # 
  # c_ <- (S * log(D * G) - (S + I) * log(D)) / log(G)      # scaled constant
  # i_ <- I / log(G)                                        # score multiplier
  
  df <- # data.frame(ScorePoints = (c_ + i_ * log(Probs /(1 - Probs))), Reference = Obsers) %>% 
    { if (Bad_Is_1 == TRUE) {
      data.frame(ScorePoints = (Offset + PDO * log2((1 - Probs) /(Probs))), Reference = Obsers) 
    }  else {  
      data.frame(ScorePoints = (Offset + PDO * log2(Probs /(1 - Probs))), Reference = Obsers) }  } %>% 
    # Remove subject with -Inf & Inf
    .[ !is.infinite(.[, 'ScorePoints']), ]
  
  # Compare mean functions by speed - https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group
  
  mu <- dplyr::summarise(df, dplyr::across(tidyselect::where(is.numeric), mean), .by = Reference)
  
  # see https://stackoverflow.com/questions/58785930/r-find-maximum-of-density-plot
  d <- stats::density(df[, 'ScorePoints'])
  df1 <- 
    data.frame(x = ceiling(d$x / 10 * 2) / 2 *10 , y = d$y) %>%
    dplyr::distinct(x, .keep_all = TRUE)
  range_axe_x <- range(df1[df1$y > 0.0000005, 'x'])
  
  d_bad <- d <- stats::density(df[df$Reference == 'Bad', 'ScorePoints'])
  d_good <- stats::density(df[df$Reference == 'Good', 'ScorePoints'])
  # modes <- function(d){
  #   i <- which(diff(sign(diff(d$y))) < 0) + 1
  #   data.frame(x = d$x[i], y = d$y[i])
  # }
  # 
  # print(modes(d))
  
  df1 <- 
    data.frame(x =  CM_Components[['CutOff']], y = c(-Inf, Inf), Reference = 'Optimal \nCutOff')
  
  df2 <- 
    data.frame(x = ceiling(d_bad$x / 10 * 2) / 2 *10 , bad_y = d_bad$y) %>%
    dplyr::inner_join( x = ., data.frame(x = ceiling(d_good$x / 10 * 2) / 2 *10 , good_y = d_good$y)
                       , by = c('x' = 'x')) %>% 
    dplyr::distinct(x, .keep_all = TRUE) %>% 
    dplyr::mutate(x, mean_y = (bad_y + good_y) / 2)
  x <- ceiling(mu$ScorePoints / 10) * 10
  mean_y <- df2[df2$x %in% x, 'mean_y']
  if (all.equal( mean_y, numeric(0) ) == TRUE) { mean_y <- c(0, 0)}
  false_y <- c(df2[df2$x %in% x[1], 'good_y'] / 2, df2[df2$x %in% x[2], 'bad_y'] / 2)
  
  # Divergence of Good & Bad Score Points by Jensen-Shannon
  divergence <- df %>% 
    dplyr::mutate(ScorePoints = round(ScorePoints, digits = 0)) %>%
    janitor::tabyl(ScorePoints, Reference) %>%
    { philentropy::JSD( x = rbind(.[, c('Bad')]/sum(.[, c('Bad')]), .[, c('Good')]/sum(.[, c('Good')])), unit = 'log') }
  
  # if (output) {
  #   writeLines(sprintf('Estimate a Jensen-Shannon’s Divergence on %s set = %0.4f', NameOfSet, divergence))
  # }
  
  # Show of Distribution of Score Points by Good & Bad Class
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_density(aes(ScorePoints, colour = Reference, fill = Reference, alpha = I(0.5))) +
    ggplot2::geom_density(aes(x = ScorePoints, size = I(1))) +
    # ggplot2::scale_x_continuous( breaks = seq(from = 100L, to = 800L, by = 100L ) ) +
    ggplot2::geom_line(data = df1, aes(x = x, y = y, color = Reference, fill = Reference), linetype = 'longdash', linewidth = 1) +
    ggplot2::geom_vline(xintercept = mean(df$ScorePoints, na.rm = TRUE), linetype = 'dashed', linewidth = 0.5) +
    ggplot2::geom_vline(data = mu, aes(xintercept = ScorePoints, color = Reference),
                        linetype = 'dashed', linewidth = 1, show.legend = FALSE) +
    ggplot2::scale_fill_manual(name = 'Reference', values = c(`Optimal \nCutOff` = 'magenta', Bad = '#F8766D', Good = '#00BFC4'))+
    ggplot2::scale_color_manual(name = 'Reference', guide = 'none', values = c(`Optimal \nCutOff` = 'magenta', Bad = '#F8766D', Good = '#00BFC4'))+
    # ggplot2::guides(fill = ggplot2::guide_legend(order = 1), colour = ggplot2::guide_legend(order = 2)) +
    ggplot2::theme_classic() + 
    ggplot2::theme(panel.border =  element_rect(fill = NA, colour = 'grey20'),
                   panel.grid.major = element_line(colour = 'grey90'),
                   plot.title = element_text(face = 'bold'),
                   legend.position = c(0.08, 0.14), legend.justification = c(1, 0),
                   axis.text.x = element_text(hjust = 0.5)) +
    ggplot2::labs(title = sprintf('Distribution of Score Points & Confusion Matrix - N:%4.0f, P:%4.0f',
                                  table(Obsers) %>% .[1], table(Obsers) %>% .[2]),
                  subtitle = sprintf('Model: %s on %s set (Jensen-Shannon Divergence = %0.4f)', NameOfModel,
                                     stringr::str_to_title(NameOfSet), divergence),
                  x = 'Predicted Score Points and Means', y = 'Density')
  
  # if(!is.na(NameOfModel)) {
  #   p <- p + labs(caption = NameOfModel)
  # }
  
  # range_axe_x <-                                           # Extract plot axes' ranges for a ggplot2 object
  #   ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
  # 
  # if ( !checkmate::test_integerish(range_axe_x, lower = 0, upper = 1000, any.missing = FALSE, len = 2L, null.ok = FALSE) )
  #   range_axe_x <- c(-100L, 1200L)    
  
  range_axe_y <-
    ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
  
  p <- p +
    ggplot2::annotate(geom = 'label', label = CM_Components[['CutOff']], y = +range_axe_y[2] * 0.03,
                      x = CM_Components[['CutOff']], color = c('magenta')) +
    ggplot2::geom_text(data = df, aes(label = round(mean(ScorePoints), 0), x = mean(ScorePoints),
                                      y = range_axe_y[2] * 0.98)) +
    ggplot2::xlim(range_axe_x)
  
  if (Bad_Is_1 == TRUE) {
    p <- p +
      ggplot2::annotate(geom = 'label', label = round(mu[['ScorePoints']], 0), 
                        y = -range_axe_y[2] * 0.03, x = mu[['ScorePoints']],
                        color = c('#00BFC4', '#F8766D')) +
      ggplot2::annotate(geom = 'label', x =  mu[['ScorePoints']], y = rev(mean_y), #c(d$y[which.max(d$y)], range_axe_y[2])*0.6,
                        label = c(sprintf('TN = %s', format(CM_Components[['TN']], big.mark = ' ')), 
                                  sprintf('TP = %s', format(CM_Components[['TP']], big.mark = ' '))), 
                        color = c('#00BFC4', '#F8766D'), hjust = 'center', size = 4) +
      ggplot2::annotate(geom = 'label', x =  mu[['ScorePoints']], y = +range_axe_y[2] * 0.20,  # false_y, 
                        label = c(sprintf('FN = %s', format(CM_Components[['FN']], big.mark = ' ')), 
                                  sprintf('FP = %s', format(CM_Components[['FP']], big.mark = ' '))), 
                        color = c('#F8766D', '#00BFC4'), hjust = 'center', size = 4)
  } else {
    p <- p +
      ggplot2::annotate(geom = 'label', label = round(mu[['ScorePoints']], 0),
                        y = -range_axe_y[2] * 0.03, x = mu[['ScorePoints']],
                        color = c('#F8766D', '#00BFC4')) +
      ggplot2::annotate(geom = 'label', x =  mu[['ScorePoints']], y = mean_y, #c(d$y[which.max(d$y)], range_axe_y[2])*0.6,
                        label = c(sprintf('TN = %s', format(CM_Components[['TN']], big.mark = ' ')), 
                                  sprintf('TP = %s', format(CM_Components[['TP']], big.mark = ' '))), 
                        color = c('#F8766D', '#00BFC4'), hjust = 'center', size = 4) +
      ggplot2::annotate(geom = 'label', x =  mu[['ScorePoints']], y = +range_axe_y[2] * 0.20,  # false_y, 
                        label = c(sprintf('FN = %s', format(CM_Components[['FN']], big.mark = ' ')), 
                                  sprintf('FP = %s', format(CM_Components[['FP']], big.mark = ' '))), 
                        color = c('#00BFC4', '#F8766D'), hjust = 'center', size = 4)
  }     # The End of if (Bad_Is_1 == TRUE)
  
  # Export Distribution of Score Points into MS Excel
  if (! is.null(wb)) {
    if (!requireNamespace('openxlsx', quietly = TRUE)) 
      stop('\'openxlsx\' is needed for this function to work. Install it via install.packages(\'openxlsx\')',
           call. = FALSE)
    
    checkmate::assert_class( wb, classes = c('Workbook')
                             , .var.name = 'Workbook object from {openxlsx} package containing a worksheet for a Chart' )
    checkmate::assert_integerish( xy, lower = 0L, upper = 2^16-1, any.missing = FALSE, len = 2L, null.ok = FALSE
                                  , .var.name = 'A vector of the form c(startCol, startRow) for workbook' )
    
    openxlsx::insertPlot(wb, sheet='Scorecard', xy = xy, width = 10 * (1 + sqrt(5))/2, height = 10, units = 'cm')
  }
  
  out <- list(
    'Divergence Coef'   = 0, # divergence,
    'ScoresCurve Chart' = p
  )
  
  return(out)
  
}   # The End of function `ScoresCurveShow()`


#' ScorePoinsbyBinShow - Distribution of Score Points by Good & Bad Class
#' @title Chart for Diveregence of Score Points 
#' @description Distribution of Score Points by Good & Bad Class
#'
#' @param Preds Vector of numeric - 'Good' Class Probabilities of Data.frame
#' @param Obsers Vector of factor - Observed Classes (Reference) 'Bad' & 'Good'
#' @param NameOfModel character - Name of Model
#' @param wb a workbook - Workbook object from \code{openxlsx} package containing a worksheet for Chart for Diveregence of Score Points
#' @param xy Vector of integer - A vector of the form c(startCol, startRow) for workbook
#' @param NameOfSet character - Name of Dataset in Model
#' @param Offset numeric - The Offset for Model in Score Points
#' @param PDO integer - The Points to Double the Odds for Model in Score Points
#' @param limits range of two integers - The First element must be less that The Second Element & every element must be a multiple of The PDO
#' 
#' @return a list where 'plot' is the object of class \code{ggplot} from \code{ggplot2} package
#'   'df' \code{data.frame} with the Distribution of Score Points by Good & Bad Class by bins
#' 
#' @references \url{https://www.amazon.com/Credit-Scoring-Toolkit-Management-Automation/dp/0199226407}
#' 
#' @example  \dontrun{
#'     ScorePoinsbyBinShow(Preds = Probs,
#'                           Obsers = Obsers,
#'                           NameOfModel = 'The Best Model',
#'                           NameOfSet = stringr::str_to_title(NameSet) )
#' }
#'  
#' @export ScorePoinsbyBinShow

ScorePoinsbyBinShow <- function(Preds, Obsers, NameOfModel = NULL, wb = NULL, xy = c(11, 37),
                                output = TRUE, NameOfSet = 'Test Set', Offset = 500, PDO = 50L,
                                limits = c(350L, 650L)) {
  
  # // Anderson, R. The credit scoring toolkit: theory and practice for retail credit risk management and decision automation. [Text] – New York: Oxford University press, 2007. – 428 p. – ISBN 0199226407 - https://www.twirpx.com/file/901319/
  
  # // Сорокин А. С. Построение скоринговых карт с использованием модели логистической регрессии // Интернет-журнал «НАУКОВЕДЕНИЕ» Выпуск 2, март – апрель 2014. https://naukovedenie.ru/PDF/180EVN214.pdf
  
  # // Сорокин А. С. К вопросу валидации модели логистической регрессии в кредитном скоринге // Интернет-журнал «НАУКОВЕДЕНИЕ» Выпуск 2, март – апрель 2014. https://naukovedenie.ru/PDF/173EVN214.pdf
  
  # Load required parameters and packages
  required_packages <- c('checkmate', 'dplyr', 'ggplot2', 'ggrepel', 'Hmisc', 'cowplot')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_numeric(Offset, lower = 0, upper = 1000, any.missing = FALSE, all.missing = FALSE,
                            .var.name = '`Offset` a numeric - The Offset for Model in Score Points.')
  checkmate::assert_integer(PDO, lower = 20L, upper = 1000L, any.missing = FALSE, all.missing = FALSE,
                            .var.name = '`PDO` an integer - The Points to Double the Odds for Model in Score Points.')
  if (length(limits) != 2L) 
    stop('"limits": Length of the limits must be 2 and every element must be a multiple of The PDO.', call. = FALSE)
  if (limits[1] >= limits[2]) 
    stop('"limits": The First element must be less that The Second element & every element must be a multiple of The PDO.', call. = FALSE)
  
  if ( checkmate::test_data_frame(Preds) ) {
    Probs <- Preds %>% pull  # For Predictions from 'caret' package
  } else {
    Probs <- Preds           # for Predictions from simple GLM or LightGBM
  }
  
  checkmate::assert_numeric(Probs, lower = 0L, upper = 1L, .var.name = '`Good` Class Probabilities - numeric vector')
  checkmate::assert_atomic_vector(Obsers, .var.name = 'Observed Classes (Reference) - factor vector')
  checkmate::assert_factor( Obsers, .var.name = 'Observed Classes (Reference) - factor vector' )
  
  # # Odds to scaled score conversion
  # 
  # S =	700; D = 16; G = 2; I =	50   # S =	700; D = 128; G = 2; I =	50
  # 
  # # Source: Anderson, R. (2007). The credit scoring toolkit: Theory and practice for retail credit risk management and decision automation. p. 428
  # 
  # c_ <- (S * log(D * G) - (S + I) * log(D)) / log(G)      # scaled constant
  # i_ <- I / log(G)                                        # score multiplier
  Bins <- c(-Inf, seq(from = limits[1], to = limits[2], by = PDO), Inf)
  
  df <- 
    { if (Bad_Is_1 == TRUE) {
      data.frame(ScorePoints = (Offset + PDO * log2((1 - Probs) /(Probs))), Reference = Obsers) 
    }  else {  
      data.frame(ScorePoints = (Offset + PDO * log2(Probs /(1 - Probs))), Reference = Obsers) }  } %>% 
    dplyr::mutate( Prediction = dplyr::if_else(ScorePoints >= OptCutOff, 'Good', 'Bad') %>% factor(), PD = 1 - Probs )
  
  SPB_df <- data.frame(
    Hmisc::cut2(df$ScorePoints, Bins) %>% table
  ) %>% 
    setNames(c('Bin', 'Total')) %>% 
    dplyr::bind_cols(., Hmisc::cut2(df %>% dplyr::filter(Reference == 'Good') %>% 
                                      dplyr::pull(ScorePoints), Bins) %>% table %>% 
                       data.frame %>% setNames(c('Bin', 'Good') ) %>% dplyr::select(Good)) %>% 
    dplyr::bind_cols(., Hmisc::cut2(df %>% dplyr::filter(Reference == 'Bad') %>%
                                      dplyr::pull(ScorePoints), Bins) %>% table %>% 
                       data.frame %>% setNames(c('Bin', 'Bad') ) %>% dplyr::select(Bad)) %>% 
    dplyr::bind_cols(., Hmisc::cut2(df %>% dplyr::filter(Prediction == 'Bad') %>%
                                      dplyr::pull(ScorePoints), Bins) %>% table %>% 
                       data.frame %>% setNames(c('Bin', 'Prediction_Bad') ) %>% dplyr::select(Prediction_Bad)) %>% 
    dplyr::bind_cols(., 
                     dplyr::left_join( x = dplyr::select(., Bin),  
                                       y = dplyr::mutate( df, Bin = Hmisc::cut2(ScorePoints, Bins)
                                                          , PD) %>%
                                         dplyr::group_by(Bin) %>% 
                                         dplyr::summarise(Pred_Bad = mean(PD), .groups = 'drop'),
                                       by = c('Bin' = 'Bin') ) %>%
                       tidyr::replace_na( list(Pred_Bad = 0.0) ) %>% 
                       dplyr::select(Pred_Bad)
    ) %>% 
    dplyr::mutate( Share_Good  = Good / sum(Good)
                   , Share_Bad        = Bad / sum(Bad)
                   , Share_Pred_Bad   = Prediction_Bad / sum(Prediction_Bad)
                   , ShareOfBad       = Bad / Total
                   , MeanOfPred_PD    = Pred_Bad
                   , Share_Total      = Total / sum(Total)
                   , Odd_Bad          = Good / Bad ) %>% 
    # Kolmogorov-Smirnov Tests by Bins
    dplyr::bind_cols(., 
                     dplyr::left_join( x = dplyr::select(., Bin),  
                                       y = dplyr::mutate(df, Bin = Hmisc::cut2(ScorePoints, Bins)
                                                         , PD) %>%
                                         dplyr::filter(Reference == 'Good') %>% 
                                         dplyr::group_by(Bin) %>%
                                         dplyr::select(Bin, PD) %>%
                                         tidyr::nest(PD) %>% 
                                         dplyr::summarise( Good = data ) %>% 
                                         dplyr::inner_join( x = .,
                                                            y = dplyr::mutate(df, Bin = Hmisc::cut2(ScorePoints, Bins)
                                                                              , PD) %>%
                                                              dplyr::filter(Reference == 'Bad') %>% 
                                                              dplyr::group_by(Bin) %>%
                                                              dplyr::select(Bin, PD) %>%
                                                              tidyr::nest(PD) %>% 
                                                              dplyr::summarise( Bad = data ),
                                                            by = c('Bin' = 'Bin')) %>%
                                         tidyr::unnest_wider(Good) %>%
                                         dplyr::rename(PD_Good = PD) %>% 
                                         tidyr::unnest_wider(Bad) %>%
                                         dplyr::rename(PD_Bad = PD) %>% 
                                         dplyr::mutate(KS = purrr::map2(.x = PD_Good, .y =  PD_Bad, 
                                                                        ~ ks.test(x = .x, y = .y)[['statistic']]) %>%
                                                         unlist() ),
                                       by = c('Bin' = 'Bin') ) %>%
                       tidyr::replace_na( list(KS = 0.0) ) %>% 
                       dplyr::select(KS)
    ) %>% 
    tibble::add_row( Bin = 'Total', Share_Good = 1, Share_Bad = 1, Share_Pred_Bad = 1,
                     ShareOfBad = sum(.[['Bad']]) / sum(.[['Total']]),
                     MeanOfPred_PD  = mean(df$PD, na.rm = TRUE),
                     Share_Total = 1, Odd_Bad = sum(.[['Good']]) / sum(.[['Bad']]),  KS = MaxKS, # max(.[['KS']]),
                     .after = nrow(.) ) %>% 
    dplyr::transmute(Bin = factor(Bin, levels = Bin), Share_Good, Share_Bad,ShareOfBad, MeanOfPred_PD, Share_Total,
                     Odd_Bad = dplyr::if_else(is.finite(Odd_Bad), Odd_Bad, NA), KS)
  
  intervals_logical <- purrr::map2_lgl(
    Bins[-length(Bins)],   # нижние границы интервалов
    Bins[-1],              # верхние границы интервалов
    ~ .x <= OptCutOff && OptCutOff < .y )
  index <- which(intervals_logical)[1]
  # sprintf("[%s, %s)", Bins[index], Bins[index + 1])
  
  df1 <- 
    data.frame(x =  SPB_df[index, 'Bin'], y = c(-Inf, Inf), Share_of = 'Optimal \nCutOff'); names(df1)[3] <- 'Share of'
  
  p1 <- 
    ggplot2::ggplot(data = SPB_df[-nrow(SPB_df), ]) +
    ggplot2::geom_bar( aes(x = Bin, y = Share_Total, color = 'Bins in Total'),
                       stat = 'identity', fill = 'dodgerblue' ) +
    ggplot2::geom_point(aes(x = Bin, y = ShareOfBad, group = 1)
                        , color = '#F8766D', size = 3) + 
    ggplot2::geom_line(aes(x = Bin, y = ShareOfBad, group = 1, color = '`Bad` in Bin')
                       , size = 1, show.legend = TRUE) + 
    ggplot2::geom_line(data = df1, aes(x = x, y = y, color = `Share of`)
                       , linetype = 'longdash', linewidth = 1) +
    ggplot2::scale_y_continuous( labels = scales::percent, limits = c(0.0, NA_real_), name = 'bars = Share of Bins in Total / line = Share of `Bad` in Bin'
                                 # # apply inverse of above transformation to correctly scale secondary axis ( / 5
                                 #                              , sec.axis = sec_axis(trans = ~ . / 5, name = 'Share of `Bad` in Bin'
                                 #                                                    , labels = scales::percent)
    ) +
    ggrepel::geom_label_repel( aes(x = Bin, y = ShareOfBad, label = sprintf('%2.1f%%', ShareOfBad * 100)),
                               color = '#F8766D', size = 5, fontface = 'bold', nudge_y = 0.05 ) +
    ggplot2::geom_text( aes(x = Bin, y = Share_Total, label = scales::percent(Share_Total, accuracy = 0.1) ),
                        color = 'black', size = 5, position = position_stack(vjust = 0.5) ) +
    ggplot2::annotate( geom = 'label', label = round2(OptCutOff), x = SPB_df[index, 'Bin'], 
                       y = max(SPB_df[-nrow(SPB_df), 'Share_Total']), color = c('magenta') ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = element_text(size = 8, hjust = 0.5),
                   panel.border =  element_rect(fill = NA, colour = 'grey20'),
                   panel.grid.major = element_line(colour = 'grey90'),
                   legend.justification = c(1, 0), legend.position.inside = c(0.9, 0.8)) + # Position Legend-top-right corner
    ggplot2::labs(title = sprintf('Distribution of Score Points by Bin - N:%4.0f, P:%4.0f', table(Obsers) %>% .[1],
                                  table(Obsers) %>% .[2]),
                  subtitle = sprintf('Model: %s on %s set (bars - Share of Bins in Total, line - Share of `Bad` in Bin)', NameOfModel, stringr::str_to_title(NameOfSet)),
                  x = 'Bins of Predicted Score Points', y = NULL, color = 'Share of') +
    ggplot2::scale_fill_manual(name = 'Share of', values = c('Bins in Total' = 'dodgerblue')) +
    ggplot2::scale_color_manual(name = 'Share of', values = c( '`Bad` in Bin' = '#F8766D'
                                                               , 'Bins in Total' = 'dodgerblue', 'Optimal \nCutOff' = 'magenta'))
  
  p2 <- 
    ggplot2::ggplot(data = SPB_df[-nrow(SPB_df), ], aes(x = Bin, y = Odd_Bad)) +
    ggplot2::geom_bar(stat = 'identity', fill = 'chartreuse') +
    ggplot2::geom_vline( aes(xintercept = SPB_df[index, 'Bin'])
                         , color = 'magenta', linetype = "longdash", linewidth = 1) +
    ggplot2::annotate( geom = 'label', label = round2(OptCutOff), x = SPB_df[index, 'Bin'], 
                       y = max(SPB_df[is.finite(SPB_df[, 'Odd_Bad']), 'Odd_Bad']), color = c('magenta') ) +
    ggplot2::scale_y_continuous( limits = c(0.0, NA_real_) ) +
    ggplot2::geom_label( aes(x = Bin, y = Odd_Bad, label = sprintf('%2.1f:1', Odd_Bad)),
                         color = 'black', size = 5, fontface = 'bold' ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = element_text(size = 8, hjust = 0.5),
                   panel.border =  element_rect(fill = NA, colour = 'grey20'),
                   panel.grid.major = element_line(colour = 'grey90'),
                   legend.justification = c(1, 0), legend.position.inside = c(0.9, 0.8)) + # Position Legend-top-right corner
    ggplot2::labs(title = sprintf('Change Good:Bad Odds by increase Score Points - N:%4.0f, P:%4.0f',
                                  table(Obsers) %>% .[1], table(Obsers) %>% .[2]),
                  subtitle = sprintf('Model: %s on %s set', NameOfModel, stringr::str_to_title(NameOfSet)),
                  x = 'Bins of Predicted Score Points', y = 'Good:Bad Odds Ratio')
  
  p <- # patchwork::wrap_plots(p1, p2, widths = c(0.50, 0.50))
    # gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(0.54, 0.46))
    cowplot::plot_grid(p1, p2, align = 'h', rel_widths = c(0.50, 0.50))
  
  # Export Distribution of Score Points into MS Excel
  if (! is.null(wb)) {
    if (!requireNamespace('openxlsx', quietly = TRUE)) 
      stop('\'openxlsx\' is needed for this function to work. Install it via install.packages(\'openxlsx\')',
           call. = FALSE)
    
    checkmate::assert_class( wb, classes = c('Workbook')
                             , .var.name = 'Workbook object from {openxlsx} package containing a worksheet for a Chart' )
    checkmate::assert_integerish( xy, lower = 0L, upper = 2^16-1, any.missing = FALSE, len = 2L, null.ok = FALSE
                                  , .var.name = 'A vector of the form c(startCol, startRow) for workbook' )
    
    openxlsx::insertPlot(wb, sheet='Scorecard', xy = xy, width = 10 * (1 + sqrt(5))/2, height = 10, units = 'cm')
  }
  
  out <- 
    list('ScorePoins Chart' = # patchwork::wrap_plots(p1, p2, widths = c(0.50, 0.50)),
           # gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(0.54, 0.46)),
           cowplot::plot_grid(p1, p2, align = 'h', rel_widths = c(0.54, 0.46)),
         'ScorePoins Bins'  = SPB_df
    )
  
  return( out )
  
}   # End of function `ScorePoinsbyBinShow` 


# Erase all Attributes from Vector
Remove_Attributes <- function(x) {attributes(x) <- NULL; return(x)}


# Computes the absolute value 
abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ' ', scientific = FALSE, trim = TRUE)
}


# Round .5 Up to Next Higher Value - https://statisticsglobe.com/r-round-ceiling-floor-trunc-signif-function-example
round2 <- function(x, digits = 0) {  # Function to always round 0.5 up
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}   # The End of Function `round2()`


#' Smbinning.PSI - Population Stability Index
#' @title Output a list with data.frame of Population Stability Index
#' @description #' Often models are developed using multiple periods in time for a number of reasons.
#' For example, to avoid seasonality, to increase the size of the population, and some others.
#' With a metrics like the Population Stability Index (PSI), users can check if there is
#' a significant variation in the distribution of a certain feature by partition (usually time)
#' using the first one as the reference.
#' @param df Data frame.
#' @param y Column name the indicates the different partitions.
#' @param x Feature to be evaluated in terms of stability (It must be factor).
#' 
#' @return Three crosstabs by feature and period that show the frequency (psicnt), 
#' percentage (psipct) and PSI (psimg), and a plot for the analyzed characteristic.
#' 
#' @examples 
#' # Check stability for one variable
#' Smbinning.PSI(df=df1,y="period",x="inc") 
#' 
#' @export Smbinning.PSI

Smbinning.PSI = function(df,y,x){
  i=which(names(df)==x)
  j=which(names(df)==y)
  if(!is.data.frame(df)){return(stop("Not a data frame"))}
  else if(identical(i,integer(0))){return(stop(paste("Characteristic",x,"not found")))}
  else if(identical(j,integer(0))){return(stop(paste("Characteristic",y,"not found")))}
  else if(class(df[,i])!="factor"){return(stop("x must be formatted as factor"))}
  else {
    psicnt=table(df[,i],df[,j],useNA = "ifany") # Table with counts including NA
    options(scipen=999) # Remove Scientific Notation
    psipct=prop.table(psicnt, margin=2) # Table with column percentage
    psimg=psipct # Shell for PSI table
    n=ncol(psipct) # Number of columns (Periods)
    m=nrow(psipct) # Number of rowa (Periods)
    
    psimg[,1]=0 # Step 1: PSI=0 for first column
    
    # PSI Period VS First Period
    for (k in 2:n){
      for (l in 1:m){
        if(psipct[l,1]>0 & psipct[l,k]>0) 
        {psimg[l,k]=round((psipct[l,k]-psipct[l,1])*log(psipct[l,k]/psipct[l,1]),8)}
        else {psimg[l,k]=0}
      }
    }
    
    psimg=rbind(psimg, PSI=colSums(psimg))
    psimg=as.table(psimg) # Table with Mg PSI
    psitable=psimg[nrow(psimg),] # Extract total PSI only
    psitable=as.data.frame(psitable)
    # Plot
    psitable$Partition=rownames(psitable) # Create column "Partition"
    rownames(psitable)=NULL  # Remove rownames
    names(psitable)=c("PSI","Partition") # Rename columns
    psitable=psitable[,c("Partition","PSI")] # Reorder
    
    return( list(psicnt=psicnt, psipct=psipct, psimg=psimg) )
  }
}   # The End of function `Smbinning.PSI()`


#' DescriptionOfVariables - Output of Description Of Variables in Dataset for Workflow
#'
#'
#' @title Description Of Variables in Dataset
#' @description #' Output of Description Of Variables in Dataset for Workflow
#'
#' @param DF data.frame - Dataset for Workflow
#' 
#' @return A Text as raw Markdown content
#' 
#' @example  \dontrun{
#'     DescriptionOfVariables( DF = DF[split[['train_index']], ] )
#' }
#'  
#' @export DescriptionOfVariables
DescriptionOfVariables <- function(DF) {
  
  # Load required parameters and packages
  # Load required parameters and packages
  required_packages <- c('checkmate', 'dplyr', 'purrr', 'tibble')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_data_frame(DF, min.rows = 1, min.cols = 1, all.missing = TRUE, 
                               .var.name = '`DF` Data.frame - Dataset for Workflow')
  
  df <- 
    DF %>% 
    { tibble::tibble(
      ID = c(1:ncol(.)),
      Field = names(.),
      Class = purrr::map_chr(., function(x) utils::head(base::class(x), n = 1)) ) }
  
  DF %>%
    { if (is.null(attr(., 'variable.labels'))) df %>% dplyr::mutate(Labels = NA_character_) else dplyr::left_join( x = df
                                                                                                                   , y = data.frame(Labels = attr(., 'variable.labels')) %>% tibble::rownames_to_column(var = 'Field')
                                                                                                                   , by = c('Field' = 'Field') ) } %>%
    dplyr::transmute( Text = paste0(ID, '. **', dplyr::if_else(Field == target_col, paste0('<u>', Field, '</u>'), Field),
                                    '** [<span style="color: ',
                                    dplyr::case_when(
                                      Class == 'integer'   ~ 'red;">',
                                      Class == 'numeric'   ~ 'darkorange;">',
                                      Class == 'Date'      ~ 'chartreuse;">',
                                      Class == 'factor'    ~ 'violet;">',
                                      Class == 'character' ~ 'blue;">',
                                      .default =             'grey;">',
                                    ),
                                    Class, dplyr::if_else(is.na(Labels), '</span>]', paste('</span>]:', Labels)), '.\n') ) %>%
    dplyr::pull(Text) %>% cat(.)
  
}    # The End of function `DescriptionOfVariables()`


#' OutputEDAofDataset - Output of Exploratory Data Analysis of Dataset Subset
#'
#'
#' @title Exploratory Data Analysis of Dataset Subset
#' @description #' Output of Exploratory Data Analysis of Dataset Subset using {smbinning} package
#'
#' @param df data.frame - Dataset for Exploratory Data Analysis (EDA)
#' @param IsTrainSet logical - Should use the dataset as Train
#' 
#' @return a *gt* table object - Result of Exploratory Data Analysis
#' 
#' @references \url{https://cran.dev/smbinning}
#' 
#' @example  \dontrun{
#'     OutputEDAofDataset(df = DF[split[['train_index']], ], 
#'                  IsTrainSet = TRUE
#'                 )
#' }
#'  
#' @export OutputEDAofDataset
OutputEDAofDataset <- function(df, IsTrainSet = FALSE ) {
  
  # Load required parameters and packages
  required_packages <- c('checkmate', 'smbinning', 'dplyr', 'tidyr', 'tibble', 'gt', 'colorspace')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_data_frame(df, min.rows = 3, min.cols = 1, all.missing = TRUE, 
                               .var.name = '`df` Data.frame - Dataset for Exploratory Data Analysis')
  checkmate::assert_logical(IsTrainSet , len = 1, any.missing = FALSE,
                            .var.name = 'IsTrainSet logical - Should use the dataset as Train')
  
  df1 <- 
    smbinning::smbinning.eda( df, rounding = 3
                              , pbar = ifelse(nrow(df) > 9e5, 1, 0))$eda %>% 
    { if (IsTrainSet == TRUE) {
      dplyr::left_join( x = .
                        , y = df %>% 
                          dplyr::reframe( dplyr::across(.cols = tidyselect:::where(is.numeric), 
                                                        .fns = ~ stats::quantile(.x, type = 1, 
                                                                                 probs = c(0.95, 0.96, 0.97),
                                                                                 na.rm = TRUE)) ) %>% 
                          tibble::add_column(., Metrics = c('Q95', 'Q96', 'Q97')) %>% 
                          tidyr::pivot_longer(!Metrics, names_to = 'Field', values_to = 'Quantile') %>% 
                          tidyr:: pivot_wider(names_from = Metrics, values_from = Quantile)
                        , by = c('Field' = 'Field'))  
    } else . }
  
  #  Summary functions for `Date` columns: 
  df2 <-
    df %>% 
    dplyr::reframe( dplyr::across( tidyselect:::where(lubridate::is.Date), ~ summary(.x, digits = 4) ) )
  
  df2 <- if (ncol(df2) > 0) {
    df2 %>% t() %>% as.data.frame() %>% setNames( c('Min', 'Q25', 'Q50', 'Avg', 'Q75', 'Max') ) %>% 
      dplyr::mutate(dplyr::across(.cols = tidyselect:::where(is.numeric), .fns = ~as.Date(.x, origin = '1970-01-01'))) %>%
      tibble::rownames_to_column(., var = 'Field') %>% 
      dplyr::inner_join( x = df1 %>% dplyr::select(Field, Recs, Miss, Unique), y = ., by = c('Field' = 'Field') ) %>% 
      tibble::add_column(., Type = 'Date', .after = 'Field') %>% 
      { if (IsTrainSet == TRUE) { 
        dplyr::mutate(., StDv = NA, Neg = NA, Zero  = NA, Pos = NA, OutLo = NA, OutHi = NA) 
      } else { 
        dplyr::mutate(., StDv = NA, Neg = NA, Zero  = NA, Pos = NA, OutLo = NA, OutHi = NA, Q95 = NA, Q96 = NA, Q97 = NA)
      } } 
  } else data.frame()
  
  #  Summary functions for `character` columns: 
  df3 <-
    df %>%
    dplyr::reframe(dplyr::across(  tidyselect:::where(is.character), 
                                   list(
                                     Min  = ~min(nchar(., allowNA = TRUE), na.rm = TRUE),
                                     Max  = ~max(nchar(., allowNA = TRUE), na.rm = TRUE),
                                     Neg  = ~sum(. %in% c("")),                           # Пустые строки (Empty)
                                     Zero = ~sum(grepl("^\\s+$", .)),                     # Строки из одних пробелов (Whitespace)
                                     Pos  = ~sum( dplyr::coalesce(nchar(.x, allowNA = TRUE) > 0, FALSE) & !grepl("^\\s+$", .x) )
                                   ),
                                   .names = '{.col}|@{.fn}')) %>% 
    t() %>% as.data.frame() %>%
    tibble::rownames_to_column(., var = 'V0') %>% 
    tidyr::separate(V0, into = c('Field', 'metric'), sep = '\\|@') %>% 
    tidyr::pivot_wider(names_from = metric, values_from = V1) %>% 
    tibble::add_column(., Type = 'Character', .after = 'Field')
  
  if (nrow(df3) > 0) df1 <- dplyr::left_join(x = df1, y= df3, by = c('Field' = 'Field'), suffix = c('', '_new')) %>% 
    # Теперь заменим значения, где это необходимо
    dplyr::mutate(dplyr::across(c('Type', 'Min', 'Max', 'Neg', 'Zero', 'Pos'), 
                                ~dplyr::coalesce(base::get(paste0(dplyr::cur_column(), '_new')), .),
                                .names = '{.col}')) %>%
    dplyr::select(-tidyselect::ends_with('_new')) # Удаляем временные колонки
  
  { if (nrow(df2) > 0) {
    dplyr::left_join( x = df1 %>% dplyr::select(Field)
                      , y = dplyr::anti_join( x = df1, y = df2, by = c('Field' = 'Field') ) %>% 
                        dplyr::mutate( dplyr::across(tidyselect::where(is.numeric) & !c(Recs, Miss, Unique, Neg, Zero, Pos), 
                                                     as.character ) ) %>% 
                        dplyr::bind_rows( dplyr::mutate(df2, across(where(lubridate::is.Date), ~format(.x, '`%y-%m-%d')) ) )
                      , by = c('Field' = 'Field') ) 
  } else df1 } %>% 
    gt::gt(id = 'one', rowname_col = 'Field') %>% 
    gt::tab_header( title = gt::md('**Exploratory Data Analysis** (EDA)') ) %>%
    gt::tab_style( style = gt::cell_fill(color = 'beige'), 
                   locations = gt::cells_body(rows = (Unique < 9 & Type == 'Num/Int')) ) %>% 
    gt::tab_style( style = gt::cell_fill(color = 'orangered'), locations = gt::cells_body(rows = Unique == 0) ) %>% 
    gt::tab_style( style = gt::cell_fill(color = 'coral'), locations = gt::cells_body(rows = Unique == 1) ) %>% 
    gt::tab_style( style = gt::cell_fill(color = 'orange'), locations = gt::cells_body(rows = Unique == 2) ) %>% 
    gt::tab_style( style = gt::cell_fill(color = 'purple1'), locations = gt::cells_body(rows = Type == 'Character') ) %>%
    gt::tab_style( style = gt::cell_fill(color = 'azure'), locations = gt::cells_body(rows = Type == 'Other') ) %>%
    gt::tab_style( style = gt::cell_fill(color = colorspace::lighten('thistle1', amount = 0.5)),
                   locations = gt::cells_body(rows = Type == 'Factor') ) %>%
    gt::tab_style( style = gt::cell_fill(color = colorspace::lighten('chartreuse', amount = 0.75)),
                   locations = gt::cells_body(rows = Type == 'Date') ) %>%
    gt::tab_style( style = gt::cell_text(weight = 'bold'), locations = gt::cells_body(columns = c(Miss, Unique)) ) %>%
    gt::tab_style( style = gt::cell_borders(sides = c('left'), color = 'grey80', weight = gt::px(3)),
                   locations = gt::cells_body( columns = c(Min, Neg, OutLo, Q95) ) ) %>% # IQR, 
    { if (IsTrainSet == TRUE) gt::tab_style(., style = gt::cell_text(color = 'red', weight = 'bold'), locations = gt::cells_body(rows = nchar(Field) > 31) ) else . } %>% 
    gt::cols_width( !dplyr::one_of('Field') ~ gt::px(55) ) %>% 
    # gt::cols_width( tidyselect:::any_of( c('Min', 'Q25', 'Q50', 'Avg', 'Q75', 'Max', 'StDv') ) ~ gt::px(40) ) %>% 
    gt::fmt_integer(column = tidyselect:::where(is.integer), sep_mark = ' ') %>%
    gt::sub_missing( columns = dplyr::everything(), rows = dplyr::everything(), missing_text = '…' ) %>% 
    gt::cols_label( Miss = gt::md('**Miss**'), Unique = gt::md('**Unique**') ) %>% 
    gt::tab_options(table.font.size = 10, container.width = gt::px(1200), container.height = gt::px(880)) %>%
    gt::tab_source_note( source_note = gt::html( paste0('<i><small>Примечание:</i> Разведочный Анализ ', 
                                                        ifelse(IsTrainSet == TRUE, 'обучающей выборки исходного', 'валидационного'), 
                                                        ' набора данных проведен пакетом <a href="https://CRAN.R-project.org/package=smbinning"><code>{smbinning}</code></a> версии ', 
                                                        packageVersion('smbinning'), '.</small>') ) ) %>% 
    # see https://stackoverflow.com/questions/75451920/freeze-panes-for-a-very-wide-table-in-gt
    gt::opt_row_striping() %>% 
    gt::opt_css(    css = "
                #one .gt_stub {
                position: sticky;
                left: 0;
                }

                #one .gt_col_heading {
                position: sticky;
                top: 0;
                }

                #one .gt_table {
                border-collapse: separate;
                border-spacing: 0;
                }

                thead th:first-child {
                left: 0;
                z-index: 2;
                }", add = FALSE)
  
}   # The End of function `EDAofDataset()`


#' OutpuEntropyBasedFeatureSelection - Output data.frame with Entropy Based Feature Selection result
#' @title Output a data.frame in {gt} Style
#' @description #' Output a data.frame in {gt} Style with Entropy Based Feature Selection
#'
#' @param df Data.frame - Dataset for Modeling
#' @param target_col character - Target Column Name of data.frame
#' 
#' @example  \dontrun{
#'     OutpuEntropyBasedFeatureSelection( df = DF[split$train_index, ],
#'                                        target_col = target_col
#'                                      )
#' }
#'  
#' @export OutpuEntropyBasedFeatureSelection

OutpuEntropyBasedFeatureSelection <- function(df, target_col) {
  
  # Load required parameters and packages
  required_packages <- c('checkmate', 'FSelectorRcpp', 'dplyr', 'gt')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_data_frame(df, min.rows = 1,min.cols = 3, # all.missing = FALSE, 
                               .var.name = '`df` Data.frame - Dataset for Modeling')
  checkmate::assert_string(target_col, .var.name = '`target_col` character - Target Column Name of data.frame')
  
  target <- df %>% .[, target_col]
  df$period <- NULL
  df$target_col <- NULL
  
  Labels_vec <- 
    if (params$Do_GLM == TRUE) stringr::str_remove_all(names(DF), pattern = '_fct')  else names(DF)
  
  FSelection_df <-
    FSelectorRcpp::information_gain(formula = GB_flag ~ ., data = DF[split$train_index, ], type = 'infogain', equal = TRUE, nbins = 5) %>%
    { if (is.null(Labels)) . else dplyr::left_join( x = .
                                                    , y = { if (params$Do_GLM == TRUE & exists('feature_names')) {
                                                      unlist( Labels[stringr::str_remove_all(feature_names, pattern = '_fct')] ) 
                                                    } else Labels[Labels_vec] %>% na.omit() %>% unlist() } %>%
                                                      data.frame(.) %>%
                                                      tibble::rownames_to_column(.data = ., var = 'Variable') %>%
                                                      setNames(c('Variable', 'Note')) %>%
                                                      { if (params$Do_GLM == TRUE) dplyr::mutate(.,
                                                                                                 Variable = paste0(Variable, '_fct')) else . }
                                                    , by = c('attributes' = 'Variable') ) } %>% 
    { if (exists('Note', .)) { dplyr::transmute(., ID = c(1:nrow(.)), Variable = attributes, Note, Infogain = importance) 
    } else dplyr::transmute(., ID = c(1:nrow(.)), Variable = attributes, Note = NA_character_, Infogain = importance) } 
  
  # FSelection_df <-  
  #   FSelectorRcpp::information_gain(formula = GB_flag ~ ., data = DF[split$train_index, ], type = 'symuncert') %>% 
  #     dplyr::transmute(Symuncert = importance) %>% 
  #       dplyr::bind_cols(FSelection_df, .)
  
  # FSelection_df <- # The algorithm finds weights of continuous and discrete attributes basing on a distance between instances.
  #   FSelectorRcpp::relief(formula = GB_flag ~ ., data = DF[split$train_index, ], neighboursCount = 5, sampleSize = 20) %>% 
  #     dplyr::transmute(RReliefF = importance) %>%
  #       dplyr::bind_cols(FSelection_df, .)
  
  FSelection_df <-  
    FSelectorRcpp::information_gain(formula = GB_flag ~ ., data = DF[split$train_index, ], type = 'gainratio', equal = TRUE, nbins = 5) %>% 
    dplyr::transmute(Gainratio = importance) %>% 
    dplyr::bind_cols(FSelection_df, .) %>% 
    dplyr::arrange(-Infogain) %>% 
    dplyr::filter( Variable != 'period' )
  
  if (params$Do_GLM == TRUE) return(FSelection_df)
  
  FSelection_df %>% gt::gt() %>%  # rowname_col = 'ID'
    gt::tab_header( title = gt::md(paste0('Entropy-Based Feature Importance by **', target_col, '** in _Train Set_')) ) %>%
    gt::tab_style(style = gt::cell_text(weight = 'bold'), locations = gt::cells_column_labels(columns = everything())) %>%
    gt::cols_width( ID ~ gt::px(50), Variable ~ gt::px(220), Infogain ~ gt::px(90), Gainratio ~ gt::px(90) ) %>%
    gt::tab_style( style = gt::cell_text(weight = 'bold'), locations = gt::cells_body(columns = c('Variable')) ) %>%
    gt::sub_missing(columns = tidyselect::everything(), rows = tidyselect::everything(), missing_text = '―') %>%
    gt::fmt_percent(columns = tidyselect:::where(is.double), decimals = 2, sep_mark = ' ') %>%
    { if (max(df$Infogain) > 0.002) gt::data_color( ., columns = c('Infogain'), rows = `Infogain` > 0.002, method = 'numeric', domain = c(0, 0.5), palette = c('lightblue' %>% gt::adjust_luminance(steps = +2), 'navy') ) else . } %>%
    { if (max(df$Gainratio) > 0.002) gt::data_color( ., columns = c('Gainratio'), rows = `Gainratio` > 0.002, method = 'numeric', domain = c(0, 0.5), palette = c('coral' %>% gt::adjust_luminance(steps = +2), 'brown') ) else . } %>%
    gt::tab_source_note( source_note = gt::html( paste0('<i>Source:</i> Feature Importance from <a href="https://CRAN.R-project.org/package=FSelectorRcpp"><code>FSelectorRcpp</code></a> version ', packageVersion('FSelectorRcpp'), '.') ) ) %>%
    # gt::opt_interactive( use_filters  = TRUE, page_size_default = Max_Vars ) %>%
    gt::opt_row_striping()
  
}    # The End of function `OutpuEntropyBasedFeatureSelection()`


#' EXplanationsOfModel - Create Explanation Of Model
#' @title Create Explanation for GLM or GBM
#' @description Create Explanation Of Model and Output The Feature Importance Information 
#'
#' @param model object - a model to be explained
#' @param DataFrameName character - Data.frame name
#' @param Sample character - character string indicating which subset will use. An Оne of 'train_index', 'test_index' (default), 'valid_index'
#' 
#' @return a explainer -  a list with the following fields: model (the explained model), data (the dataset used for training), y (response for observations from data), weights (sample weights for data. NULL if weights are not specified), y_hat (calculated predictions), residuals (calculated residuals), predict_function (function that may be used for model predictions, shall return a single numerical value for each observation), residual_function (function that returns residuals, shall return a single numerical value for each observation), class (class/classes of a model), label (label of explainer) and model_info (named list contating basic information about model, like package, version of package and type)
#' 
#' @references \url{https://www.r-bloggers.com/dalex-and-h2o-machine-learning-model-interpretability-and-feature-explanation}
#' 
#' @example  \dontrun{
#'     EXplanationsOfModel(model = modelFit,
#'                         DataFrameName = 'DF',
#'                         Sample = 'test_index')
#' }
#'  
#' @export EXplanationsOfModel

EXplanationsOfModel <- function(model = modelFit, DataFrameName = 'DF', Sample = 'test_index') {
  
  loss_one_minus_gini <- function(observed, predicted) {
    # Calculate Loss for Gini statistic
    pred <- data.frame(fitted.values = predicted, y = observed)
    pred_sorted <- pred[order(pred$fitted.values, decreasing = TRUE),
    ]
    roc_y <- factor(pred_sorted$y)
    levels <- levels(roc_y)
    x <- cumsum(roc_y == levels[1])/sum(roc_y == levels[1])
    y <- cumsum(roc_y == levels[2])/sum(roc_y == levels[2])
    auc <- sum((x[2:length(roc_y)] - x[1:length(roc_y) - 1]) *
                 y[2:length(roc_y)])
    return(1 - (auc - 0.50) * 2)
  }   # End of function `loss_one_minus_gini`
  
  
  # Load required parameters and packages
  required_packages <- c('DALEX', 'stringr', 'dplyr', 'ggplot2', 'ingredients', 'gt')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_string(DataFrameName, .var.name = '`DataFrameName` character string Data.frame name')
  checkmate::assert_string(Sample, .var.name = '`Sample` character string indicating which subset will use. An Оne of "train_index", "test_index" (default), "valid_index"')
  
  # I. Creating Model Explainer from 'DALEX' package
  Explain_Label <-  paste(ModelName, 'by Features in', stringr::str_to_title(Sample), 'set')
  
  if ( class(model)[1] == 'glm' ) {
    model_explain <- DALEX::explain( model = model,
                                     data = if (DataFrameName == 'X') X[ split[[Sample]], feature_names ] else base::get(DataFrameName)[ split[[Sample]], ],
                                     y = if (model[['family']][['family']] == 'binomial') {
                                       ifelse( Y[split[[Sample]]] == 'Bad', ifelse(Bad_Is_1, 0L, 1L),   # Reverse
                                               ifelse(Bad_Is_1, 1L, 0L) )
                                     } else Y[split[[Sample]]],
                                     label = paste('GLM:', ModelName) )
  } else {
    library('DALEXtra')         # Extension for 'DALEX' Package
    model_explain <- DALEXtra::explain_xgboost( model = model,
                                                data =  as.matrix(DF1[split[[Sample]], feature_names ]),  # Only used variables
                                                y = ifelse( Y[split[[Sample]]] == 'Bad', 0L, 1L ),    # Reverse
                                                colorize = TRUE,
                                                label = paste('LightGBM:', ModelName),
                                                type = 'classification' ) # if (model$eval_train()[[1]][[2]] == 'binary') 'classification' else 'regression' )
  }   # The End of if ( class(model)[1] == 'glm' )
  
  if ( exists('FeatGroups') | length(feature_names) <= Max_Vars ) {
    # Interpretable Machine Learning: Feature Importance
    
    # II. Feature Importance
    IsBinaryGLM <- if (is.null(model[["family"]][["family"]]) == TRUE) FALSE else model[["family"]][["family"]] == 'binomial'
    if (length(feature_names) <= Max_Vars) {
      # Importance by Each Features
      FeaturesGroups <- NULL
    } else {
      # Importance by Detailed Features Groups
      FeaturesGroups <- lapply( X = FeatGroups, FUN = intersect, y = if (params$Do_GLM == TRUE & IsBinaryGLM) stringr::str_remove_all(feature_names, '_fct') else feature_names )
      FeaturesGroups <- if (params$Do_GLM == TRUE & IsBinaryGLM) sapply(X = FeaturesGroups[ lengths(FeaturesGroups) > 0 ], FUN = paste, '_fct', sep = '') else FeaturesGroups[ lengths(FeaturesGroups) > 0 ]
      Explain_Label  <- paste(ModelName, 'by Features Groups in', stringr::str_to_title(Sample), 'set')
    }    # The End if (length(feature_names) <= Max_Vars)
    
    set.seed(seed)
    feat_imp <- ingredients::feature_importance( model_explain, loss_function = loss_one_minus_gini
                                                 , type = c('raw', 'ratio', 'difference')[3]
                                                 , n_sample = ifelse(length(split[[Sample]]) < 1e5, length(split[[Sample]]), 100000L)
                                                 , B = ifelse(length(split[[Sample]]) < 1e5, 10, 5)
                                                 # Importance by Detailed Features Groups
                                                 , variable_groups =  FeaturesGroups
                                                 , label           =  Explain_Label )
    
    importance_df <-
      feat_imp %>% 
      dplyr::group_by( variable ) %>%
      dplyr::summarise(importance = mean(dropout_loss), .groups = 'drop') %>%
      dplyr::mutate(Variable = gsub('_fct', '', variable) %>% as.factor,
                    Importance = ifelse(round(importance, digits = 7) == 0, NA, importance) )%>%
      dplyr::select(Variable, Importance) %>% 
      dplyr::arrange(dplyr::desc(Importance)) %>% 
      na.omit()
    
    # Список НАИБОЛЕЕ востребованных признаков на  Gini
    x <-
      importance_df %>% 
      dplyr::filter(!Variable %in% c('_full_model_', '_baseline_')) %>% 
      { if (nrow(.) > Max_Vars) dplyr::slice_min(Mean, n = Max_Vars) else . } %>% 
      dplyr::mutate(Variable = as.character(Variable)) %>% 
      dplyr::pull(Variable)
    
    p0 <- plot(feat_imp) +
      ggplot2::labs( y = 'Loss Gini after variables permutations', title = 'The Features Importance Plot (Gini-based)',
                     subtitle = NULL, caption = Model_Subtitle[[ as.character(params$Model_Type) ]] )
  } else {
    # Model Performance from 'DALEX' package
    DALEX::model_performance(model_explain) %>%
      plot(., geom = 'histogram')
    
    # Variable importance
    importance_df <-
      DALEX::variable_importance(model_explain, type = 'raw', loss_function = loss_one_minus_gini) %>%
      dplyr::mutate(Importance = -(dropout_loss - .[variable == '_full_model_', 'dropout_loss'])) %>%
      dplyr::filter(!stringr::str_detect(variable, 'full_model|baseline')) %>%
      dplyr::mutate(Variable = base::gsub('_fct', '', variable)) %>%
      dplyr::select(Variable, Importance) %>% 
      dplyr::arrange(dplyr::desc(Importance))
    
    # Список НАИБОЛЕЕ востребованных признаков на  Gini
    x <-
      importance_df %>%
      dplyr::filter(!Variable %in% c('_full_model_', '_baseline_')) %>% 
      dplyr::group_by( Variable ) %>%
      dplyr::summarise(Mean = mean(Importance), .groups = 'drop') %>%
      dplyr::arrange( dplyr::desc(Mean) ) %>%
      { if (nrow(.) > Max_Vars) dplyr::slice_head(., n = Max_Vars) else . } %>% 
      dplyr::pull(Variable)
    
    p0 <- DALEX::variable_importance( model_explain, type = c('raw', 'ratio', 'difference')[3]
                                      , loss_function = loss_one_minus_gini ) %>%
      plot() + ggplot2::labs(title = 'Variable Importance', caption = 'by Gini Coefficient') +
      ggplot2::guides(color = 'none') + ggplot2::theme_grey()
    
  } # End if ( exists('FeatGroups') | length(feature_names) <= Max_Vars )
  
  # Show Feature Importance Plot
  print(p0)
  
  writeLines('\n Extract Feature Importance Names:'); dput(x)
  
  # Output Chart: EXplanations of Model by Variable Importance
  if (params$Do_GLM == TRUE & IsBinaryGLM & exists('wb')) openxlsx::insertPlot(wb, sheet = 'IV Table', xy = c(1, ncol(DF) + 4), width = 24, height = 10, units = 'cm')
  # The End of if (params$Do_GLM == TRUE & IsBinaryGLM & exists('wb')) 
  
  table1 <- 
    importance_df %>% 
    dplyr::filter(!Variable %in% c('_full_model_', '_baseline_')) %>% 
    dplyr::group_by( Variable ) %>%
    dplyr::summarise(`Вклад по Gini` = mean(Importance), .groups = 'drop') %>%
    dplyr::arrange( dplyr::desc(`Вклад по Gini`) ) %>%
    { if ( is.null(Labels) & is.null(unlist(Labels[feature_names])) ) . else dplyr::left_join( x = .
                                                                                               , y = unlist( Labels[stringr::str_remove_all(feature_names, pattern = '_fct')] ) %>%
                                                                                                 utils::stack() %>%
                                                                                                 setNames(c('Описание', 'Предиктор'))
                                                                                               , by = c('Variable' = 'Предиктор') ) %>% 
        dplyr::transmute(Variable, `Описание`, `Вклад по Gini`) } %>% 
    tibble::add_column(., .before = 'Variable', ID = c(1:nrow(.))) %>% 
    gt::gt(rowname_col = 'ID') %>%
    gt::tab_header( title = gt::md('Feature Importance by **Gini** in _Test Set_') ) %>%
    gt::tab_style( style = gt::cell_text(weight = 'bold'), locations = gt::cells_body(columns = c(Variable)) ) %>%
    gt::sub_missing(columns = tidyselect::everything(), rows = tidyselect::everything(), missing_text = '―') %>% 
    gt::fmt_percent(column = tidyselect:::where(is.double), decimals = 1, sep_mark = ' ') %>%
    gt::data_color( columns = c('Вклад по Gini'), rows = `Вклад по Gini` > 0.002, method = 'numeric', domain = c(0,0.5),
                    palette = c('lightblue' %>% gt::adjust_luminance(steps = +2), 'navy') ) %>% 
    gt::tab_source_note( source_note = gt::html( paste0('<i>Source:</i> Feature Importance from <a href="https://CRAN.R-project.org/package=DALEX"><code>DALEX</code></a> version ', packageVersion('DALEX'), '.') ) ) %>%
    gt::opt_row_striping()
  
  # openxlsx::addWorksheet(wb0 <- openxlsx::createWorkbook(), sheetName = 'Example', gridLines = F)
  # openxlsx::writeData(wb0, sheet = 1, x = importance_df, withFilter = TRUE); openxlsx::openXL(wb0)
  
  base::assign( 'importance_df', # Both <<- and assign will work into 'importance_df'
                importance_df, base::globalenv() )
  base::assign( 'FeatureImportance_table', # Both <<- and assign will work into 'FeatureImportance_table'
                table1, base::globalenv() )
  return( model_explain )
  
}   # The End function `EXplanationsOfModel()`


#' CalculateGiniStability - The scorecard's stability in the future is critical
#' @title Weekly Gini's Stability to Predictive Model
#' @description Output Gini's Stability by Weeks
#' 
#' @param gini_in_time vector of numeric - Dataset of Model results
#' @param w_fallingrate numeric - The Weekly Falling Rate
#' @param w_resstd numeric - The Diminution for Standard Deviation of Residuals 
#' 
#' @return a numeric - Gini's Stability to Predictive Model
#' 
#' @references \url{https://www.kaggle.com/competitions/home-credit-credit-risk-model-stability}
#' 
#' @example  \dontrun{
#'     CalculateGiniStability( gini_in_time = Peg_result_dt$Gini2 # Test_index
#'                           , w_fallingrate = 88.0
#'                           , w_resstd = -0.5
#'                    )
#' }
#'  
#' @export CalculateGiniStability

CalculateGiniStability <- function( gini_in_time
                                    , w_fallingrate = 88.0
                                    , w_resstd      = -0.5) {
  
  # see # https://www.kaggle.com/code/carloshuertas/homecreditmetrictest
  
  x <- seq_along(gini_in_time)
  y <- gini_in_time
  lm_model <- lm(y ~ x)
  coef_values <- coef(lm_model)
  a <- coef_values[2]
  b <- coef_values[1]
  y_hat <- a * x + b
  residuals <- y - y_hat
  res_std <- sd(residuals)
  avg_gini <- mean(gini_in_time)
  return(avg_gini + w_fallingrate * min(0, a) + w_resstd * res_std)
  
}   # The End of function `CalculateGiniStability()`

CalculateMetricsBySubsets <- function(dat = df, var = 'Peg') {
  
  ModelName <<- eval(rlang::sym(var), dat)[1]
  SizeOf    <- dat %>% 
    dplyr::group_by(Subset) %>% 
    dplyr::summarise( Count = sprintf('%.0f', dplyr::n() ) ) %>%
    dplyr::pull(Count) %>% paste(., collapse = ', ')
  DefRates  <- dat %>%
    dplyr::group_by(Subset) %>% 
    dplyr::summarise( Mean = sprintf('%0.2f%%', dplyr::if_else(Bad_Is_1 == TRUE,  mean(GB_flag, na.rm = TRUE), 
                                                               mean(1 - GB_flag, na.rm = TRUE) ) * 100), .groups = 'drop') %>%
    dplyr::pull(Mean) %>% paste(., collapse = ', ')
  
  if (Sample == 'validation') {
    N <- OutputMeasuresBySubsets( Result_Datatable_Name = 'Peg_result_dt', Subset = 'validation',
                                  , Probs  = eval(rlang::sym('Probs'), dplyr::filter(dat, Subset == 'valid'))
                                  , SmClass = eval(rlang::sym(target_col), dplyr::filter(dat, Subset == 'valid'))
                                  , ShowPlot = FALSE, N = 0L, SizeOf = SizeOf, DefRates = DefRates )
  } else {
    for (Sub_Set in NamesOfDatasets) if (nrow(dat[ Subset == Sub_Set ]) > 0 ) N <- 
        OutputMeasuresBySubsets( Result_Datatable_Name = 'Peg_result_dt'
                                 , Subset = Sub_Set
                                 , Probs  = eval(rlang::sym('Probs'), dplyr::filter(dat, Subset == Sub_Set))
                                 , SmClass = eval(rlang::sym(target_col), dplyr::filter(dat, Subset == Sub_Set))
                                 , ShowPlot = FALSE, N = 0L, SizeOf = SizeOf, DefRates = DefRates )
  }
  
}    # The End of Function `CalculateMetricsBySubsets()`


#' EvaluateModelByPeg - Assessment Efficiency of the Model by Peg
#' @title Evaluate the Model by Peg
#' @description Assessment Performance of the Model by Peg Variable with some Metrics
#'
#' @param df - Data.table with Reference, Prediction & Peg Variable
#' @param Name character - Label of Peg Variable in Data.table (Non-Continuous Numerical Variable)
#' 
#' @return a Output Data.table of class \code{data.table} from \code{data.table} package
#'   with Efficiency of the Model by Peg
#' 
#' @references \url{}
#' 
#' @example  \dontrun{
#' .[, Peg := as.character(period)] %>% 
#'   EvaluateModelByPeg(df = ., Name = 'Periods')
#' }
#'  
#' @export EvaluateModelByPeg

EvaluateModelByPeg <- function(df, Name = 'Organization Type') {
  
  # Load required parameters and packages
  required_packages <- c('checkmate', 'data.table', 'purrr')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_string(Name, .var.name = '`Name` character string explanatory Peg variable in `df`')
  checkmate::assert_data_table(df, min.rows = 1L, min.cols = 3L, .var.name = 
                                 '`df` data.table - Dataset with Reference, Prediction & Peg Columns')
  
  # Calculate All Dataset of Probability (Probs) & Reference Classes (SmClass)
  GetProbsAndSmClass(row_ids = c(1:nrow(DF)))
  
  df <- df %>%
    .[, .( Peg = Peg
           , Subset = factor( Subset, levels = NamesOfDatasets ) # , Subset = factor( NA_character_, levels = NamesOfDatasets )
           , GB_flag = SmClass
           , Probs = Probs ) ] %>%
    # data.table::setnames( old = names(.), new = c(names(.)[-ncol(.)], paste0('Probability_of_', target_name)) ) %>% 
    # .[order( Peg )] %>%
    data.table::merge.data.table( x =  .
                                  , y = { .[, .(PegAggr = sprintf('%s: %0.2f%%', Peg, .N / nrow(.) * 100)), by = 'Peg'] }
                                  , by = c('Peg' = 'Peg'), all = FALSE ) %>% 
    .[, `:=` (Peg = PegAggr, PegAggr = NULL) ] %>% 
    { if (Sample == 'validation') .[Subset == 'valid', ] else . } 
  
  df %>% 
    split(df$Peg) %>%
    purrr::walk2(.x = ., .y = 'Peg', .f = CalculateMetricsBySubsets)
  
  if (Sample != 'validation') Peg_result_dt %>%
    OutputDataFrame( df = ., 
                     title = paste0('<p style="color:', BrColor, '">Classification results by <b>TRAIN</b>, <b>TEST</b> & <b>VALIDATION</b> Sets by <b>', Name, '</b></p>'),
                     stubhead = Name,
                     col.names = c('Name', unlist(lapply(c('Train Set', 'Test Set', 'Validation Set'),
                                                         function(x) paste(x, c('Gini', 'KS', 'Sens', 'Spec'), sep = "_"))),
                                   'Datasets_Sizes of', paste0('Datasets_', target_name, ' Rates') )
    )
}   #  The End of function `EvaluateModelByPeg()`


#' OutputDataFrame - Output data.frame with Model result
#' @title Output a data.frame in {gt} Style
#' @description #' Output a data.frame in knitr Style with
#'
#' @param df Data.frame - Dataset of Evaluating Models
#' @param title character - Title of data.frame
#' @param stubhead character - Header for First Column of Output Table
#' @param col.names vector of characters - Names of columns in a data.frame
#' 
#' @references \url{https://www.amazon.com/Credit-Scoring-Toolkit-Management-Automation/dp/0199226407}
#' 
#' @example  \dontrun{
#'     OutputDataFrame(df = data.table::setDF(result_dt), 
#'                     title = 'Classification results by TRAIN, TEST & VALIDATION Set',
#'                     stubhead = 'Models', 
#'                     col.names = c('Name', unlist(lapply(c('Gini', 'KS', 'Sens', 'Spec'),
#'                                      function(x) paste(x, c('Train Set', 'Test Set', 'Validation Set'), sep = "_"))),
#'                                   paste(c('Sizes of', paste(target_name, 'Rates')), 'Datasets', sep = '_'))
#'                    )
#' }
#'  
#' @export OutputDataFrame

OutputDataFrame <- function(df, title, stubhead, col.names) {
  
  # Load required parameters and packages
  required_packages <- c('checkmate', 'dplyr', 'gt')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_data_frame(df, min.rows = 1, ncol = length(col.names), # all.missing = FALSE, 
                               .var.name = '`df` Data.frame - Dataset of Evaluating Models')
  checkmate::assert_string(title, .var.name = '`title` character - Title of data.frame')
  checkmate::assert_string(stubhead, .var.name = '`stubhead` character - stubhead of row.names')
  checkmate::assert_character(col.names, any.missing = FALSE, all.missing = FALSE, len = ncol(df),
                              .var.name = '`col.names` vector of characters - Names of columns in a data.frame')
  
  df %>% 
    setNames( col.names ) %>% 
    gt::gt(rowname_col = 'Name') %>%
    gt::tab_header(
      title = gt::html(title),
      subtitle = gt::md(paste('_Calculated:_', format(Sys.time(), "%a  %d %B %Y %X")))
    ) %>% 
    gt::tab_stubhead(label = stubhead) %>%
    gt::tab_spanner_delim(delim = '_') %>% 
    gt::tab_style( style = list( gt::cell_fill(color = 'thistle1'), gt::cell_text(weight = 'bold')  ),
                   locations = gt::cells_body( columns = c(dplyr::ends_with('_Gini'), dplyr::ends_with('_KS')) ) ) %>% 
    gt::cols_width( Name ~ gt::px(100), dplyr::starts_with('Datasets_') ~ gt::px(130) ) %>% 
    gt::fmt_percent( columns = -dplyr::starts_with(c('Name', 'Datasets_')), decimals = 1 ) %>% 
    gt::fmt_integer( columns = tidyselect:::where(is.integer), sep_mark = ' ' ) %>% 
    gt::sub_missing( columns = tidyselect::everything(), rows = tidyselect::everything(), missing_text = '…' ) %>% 
    gt::data_color( columns = c('Test Set_Gini'),
                    fn = scales::col_numeric( palette = c('red', 'orange', 'green'), 
                                              domain = c(0, 1), na.color = 'azure') ) %>% 
    { if (Is_Show_Development_Dataset == FALSE) gt::cols_hide(., columns = dplyr::starts_with(c('Train', 'Test'))) else . } %>% 
    { if (Is_Show_Validation_Dataset == FALSE) gt::cols_hide(., columns = dplyr::starts_with(c('Validation'))) else . } %>% 
    gt::tab_options( 
      # heading.title.font.size = 'large',
      table.font.size = 13,
      table.border.top.color = 'white',
      heading.border.bottom.color = BrColor,
      table.border.bottom.color = 'white',
      # column_labels.background.color = BrColor,
      column_labels.border.top.color = BrColor,
      column_labels.border.bottom.color = BrColor,
      table_body.border.bottom.color = BrColor,
      table_body.hlines.color = 'white'
    ) %>% 
    gt::opt_row_striping()
  
}   # The End of function `OutputDataFrame()`


#' GetOptimalCutOffValue - Get An Optimal CutOff Value
#' @title Get An Optimal CutOff Value
#' @description Get An Optimal CutOff Value using Reference & Prediction vectors
#'
#' @param Probs Numeric - a vector of Predicted Probability of Positive Events Or Score Points
#' @param SmClass Integer - a vector of Reference Values with 0L & 1L
#' 
#' @return an Integer -  a value of Optimal CutOff
#' 
#' @references \url{https://cran.dev/smbinning}
#' 
#' @example  \dontrun{
#'     GetOptimalCutOffValue(Probs = Probs, 
#'                           SmClass = SmClass)
#' }
#'  
#' @export GetOptimalCutOffValue

GetOptimalCutOffValue <- function(Probs = Probs, SmClass = SmClass) {
  # Get A Value of OptimalcCutOff
  
  # Load required parameters and packages
  required_packages <- c('checkmate', 'ROCR')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_numeric(Probs, any.missing = FALSE, all.missing = FALSE, len = length(SmClass),
                            .var.name = '`Probs` vector of numeric - Predicted Probability of Positive Events Or Score Points')
  checkmate::assert_integer(SmClass, lower = 0L, upper = 1L, any.missing = FALSE, all.missing = FALSE, len = length(Probs),
                            .var.name = '`SmClass` vector of integer - Reference (True) Value of Positive Events')
  
  # https://www.quora.com/What-are-the-inverse-functions-of-these-f-x-x-1-x  
  if (max(Probs, na.rm = TRUE) > 1) if (do_GLM == TRUE) Probs <- (1 - 2^( (Probs - Offset)/PDO ) / ( 2^( (Probs - Offset)/PDO ) + 1 )) else Probs <- (1 + 2^( (Probs - Offset)/PDO ) / (1 - 2^( (Probs - Offset)/PDO ) )) # for Score Points - message('Надо исправить формулу для LightGBM')
  
  predROC    <- ROCR::prediction(predictions = Probs, labels = SmClass)
  Sens.model <- ROCR::performance(predROC, measure = 'sens', x.measure = 'cutoff')
  Spec.model <- ROCR::performance(predROC, measure = 'spec', x.measure = 'cutoff')
  # cutoff that yields the highest sensitivity plus specificity - https://stackoverflow.com/questions/35731526/calculate-the-optimal-max-sensitivity-and-specificity-cut-off-value-using-r
  optcut <- Sens.model@x.values[[1]][ which.max(Sens.model@y.values[[1]] + Spec.model@y.values[[1]]) ]
  optcut <- if (do_GLM == TRUE) Offset + PDO * log2((optcut) / (1 - optcut)) else Offset + PDO * log2((1 - optcut) / (optcut))
  optcut <- as.integer( round2(optcut, digits = 0) )
  
  return( optcut )
  
}    # The End of Function `GetOptimalCutOffValue()`


GetMetricLabel <- function(metric = 'gini') {
  return(
    dplyr::case_when(
      metric == 'gini'          ~ '**Коэффициент Gini**',
      metric == 'bad rates'     ~ '**Уровень Дефолта**',
      metric == 'average'       ~ '**Средний балл**',
      metric == 'median'        ~ '**Медиана балла**',
      metric == 'cutoff'        ~ '**Оптимальный балл отсечения**',
      metric == 'predclass'     ~ '**Доля займов с баллами ниже CutOff**',
      metric == 'oddlcotmco'    ~ '**Отношение Уровня Продолжения просрочек < CutOff / >= CutOff**'
    )
  )
}     # The End of function `GetMetricLabel()`


#' OutputRegionalOblMetric - Output Regional Metric by Oblasts
#' @title Output Regional Metric by Oblasts
#' @description Output Regional Metric by Oblasts as Table & Map
#'
#' @param DF Data.frame - Dataset for Modeling
#' @param metric character -  A Name of Metric
#' @param var character - A Column Name of data.frame with Predicted Probability
#' @param Pallete vector - A Character Vector with color codes (hex) or color names
#' 
#' @references \url{https://cran.dev/tmap}
#' 
#' @example  \dontrun{
#'     OutputRegionalOblMetric(DF = DF,
#'                             metric = 'bad rates', 
#'                             var = 'GB_flag',
#'                             Pallete = 'Greens')
#' }
#'  
#' @export OutputRegionalOblMetric

OutputRegionalOblMetric <- function(DF = DF, metric = 'gini', var = 'Probs', Pallete = 'Greens', Year = 2022L) {
  # Output Regional Metrics in Table & Map
  
  # Load required parameters and packages
  required_packages <- c('checkmate', 'data.table', 'dplyr', 'rlang', 'ModelMetrics', 'janitor',
                         'gt', 'tmap', 'tmaptools', 'tidyselect', 'geokz')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_data_frame(DF, min.rows = 1,min.cols = 2, # all.missing = FALSE, 
                               .var.name = '`df` Data.frame - Dataset for Modeling')
  checkmate::assert_string(metric, .var.name = '`metric` character - A Name of Metric')
  checkmate::assert_string(var, .var.name = '`var` character - A Column Name of data.frame with Predicted Probability')
  checkmate::assert_character(Pallete, any.missing = FALSE, min.len = 1,
                              .var.name = '`Pallete` vector - A Character Vector with color codes (hex) or color names')
  
  Metric_label <- GetMetricLabel(metric = metric)
  
  DF <- 
    DF %>% 
    dplyr::filter(!is.na( !!rlang::sym(var) ) & !is.na( !!rlang::sym(target_col) ))
  
  MinusOrPlus <- # Convert a Direction of Score  Points into a Direction of Probability of Default
    if(max(eval(rlang::sym(var), DF), na.rm = TRUE) > 1) -1L else +1L
  
  df <- DF %>%
    data.table::setDT() %>%
    .[, {
      Metric <- switch( metric,
                        'gini' = ModelMetrics::gini(actual = get(target_col), predicted = MinusOrPlus * get(var)),
                        'bad rates' = mean(get(target_col), na.rm = TRUE),
                        'average' = as.integer(mean(get(var), na.rm = TRUE)),
                        'median' = as.integer(median(get(var), na.rm = TRUE)),
                        'cutoff' = as.integer(GetOptimalCutOffValue(Probs = base::get(var), SmClass = base::get(target_col)))
      )
      LoanAmount0 <- switch( metric, 'bad rates' = median(ta_kzt, na.rm = TRUE), 0L ) # 0 - Default
      LoanAmount1 <- switch( metric, 'bad rates' = mean(ta_kzt, na.rm = TRUE), 0L )   # 0 - Default
      
      .(Metric = Metric, LoanAmount0 = LoanAmount0, LoanAmount1 = LoanAmount1)
    }, by = REG_ADDRESS_DISTRICTS] %>% 
    { if (metric == 'gini') .[Metric < 0.30, Metric := NaN] else . } %>% 
    data.table::setDF(.)
  
  # if (metric == 'cutoff') df$Metric = as.integer(df$Metric)
  
  df <- 
    DF %>% 
    janitor::tabyl(REG_ADDRESS_DISTRICTS) %>% 
    janitor::adorn_totals(where = 'row', fill = "-", na.rm = TRUE, name = 'Итого по Казахстану') %>%
    janitor::untabyl(.) %>% 
    dplyr::left_join( x = ., y = df, by = c('REG_ADDRESS_DISTRICTS' = 'REG_ADDRESS_DISTRICTS') ) %>% 
    dplyr::mutate( Metric = dplyr::if_else(REG_ADDRESS_DISTRICTS == 'Итого по Казахстану', Metric_KZ, Metric),
                   metric =  metric) %>% 
    { if (metric == 'bad rates') dplyr::transmute( ., REG_ADDRESS_DISTRICTS, n, percent
                                                   , LoanAmount0 = dplyr::if_else(REG_ADDRESS_DISTRICTS == 'Итого по Казахстану', median(DF$ta_kzt, na.rm = TRUE), LoanAmount0)
                                                   , LoanAmount1 = dplyr::if_else(REG_ADDRESS_DISTRICTS == 'Итого по Казахстану', mean(DF$ta_kzt, na.rm = TRUE), LoanAmount1)
                                                   , Metric, metric) else . }
  if ( metric %in% c('average', 'median', 'cutoff') ) df$Metric <- as.integer(df$Metric)
  
  table <- df %>% 
    dplyr::mutate(color = '') %>% 
    gt::gt() %>% 
    gt::tab_header(title = gt::md( paste0('**', Title, '**') )) %>%
    gt::data_color( columns = n, method = 'numeric', rows = c(1:(nrow(df) - 1))
                    , palette = tmaptools::get_brewer_pal(dplyr::case_when(metric == 'gini' ~ 'GnBu', 
                                                                           metric == 'bad rates' ~ 'OrRd',
                                                                           metric == 'average' ~ 'PuRd', 
                                                                           metric == 'median' ~ 'Reds',
                                                                           metric == 'cutoff' ~ 'Oranges'),
                                                          n = nrow(KAZ_tbl),
                                                          contrast = c(.1, .5), plot = FALSE) ) %>%
    gt::cols_hide(columns = c('metric')) %>% 
    gt::data_color( columns = Metric,
                    target_columns = color,
                    method = 'numeric',
                    # domain = c(4E7, 7E7),
                    palette = tmaptools::get_brewer_pal(dplyr::case_when(metric == 'gini' ~ 'GnBu', 
                                                                         metric == 'bad rates' ~ 'OrRd',
                                                                         metric == 'average' ~ 'PuRd', 
                                                                         metric == 'median' ~ 'Reds',
                                                                         metric == 'cutoff' ~ 'Oranges'),
                                                        n = nrow(KAZ_tbl),
                                                        contrast = c(.2, .7), plot = FALSE)
    ) %>% 
    gt::fmt_integer( column = tidyselect:::where(is.integer), sep_mark = ' ' ) %>% 
    gt::tab_style(
      style = list(gt::cell_text(weight = 'bold')),
      locations = gt::cells_body(columns = tidyselect::everything(), rows = length(tidyselect::everything()))
    ) %>% 
    gt::tab_style( style = gt::cell_text(weight = 'bold'), locations = gt::cells_body(columns = c(n, Metric)) ) %>%
    gt::tab_style( style = gt::cell_text(style = 'italic'), locations = gt::cells_body( columns = percent) ) %>%
    gt::tab_style( style = gt::cell_borders(sides = c('left'), color = BrColor, weight = gt::px(2)),
                   locations = gt::cells_body( columns = c(percent, Metric) ) ) %>%
    gt::fmt_percent(columns = tidyselect:::where(is.double), decimals = 2) %>% 
    gt::sub_missing( columns = everything(), rows = everything(), missing_text = '…' ) %>% 
    { if (metric == 'bad rates') gt::fmt_number( ., column = c(LoanAmount0, LoanAmount1), suffixing = TRUE )
      else . } %>%
    gt::cols_width( color ~ gt::px(20) ) %>% 
    gt::cols_label( REG_ADDRESS_DISTRICTS = 'Адм.-терр. единицы 1 уровня (Области)', n = gt::md('**Кол-во контрактов**'), 
                    percent = 'Уд. вес', Metric = gt::md(Metric_label), color = '' ) %>%
    { if (metric == 'bad rates') gt::cols_label( ., LoanAmount0 = gt::md('**Медианный займ**'),
                                                 LoanAmount1 = gt::md('**Средний займ**') ) else gt::cols_hide(., columns = c(LoanAmount0, LoanAmount1)) } %>%
    gt::tab_options( 
      column_labels.font.size = gt::pct(110),
      # column_labels.font.weight = 'bold',
      # row_group.background.color = colorspace::lighten(BrColor, amount = 0.95),
      # row_group.font.size = gt::pct(110),
      # row_group.font.weight = 'bold',
      data_row.padding = gt::px(-1),
      table.font.size = 12.5,
      table.border.top.color = 'white',
      heading.border.bottom.color = BrColor,
      table.border.bottom.color = 'white',
      column_labels.border.top.color = BrColor,
      column_labels.border.bottom.color = BrColor,
      # row_group.border.top.color = BrColor,
      # row_group.border.bottom.color = BrColor,
      table_body.border.bottom.color = BrColor,
      table_body.hlines.color = 'white'
    ) %>% 
    # gt::opt_interactive() %>%
    # gt::opt_vertical_padding(scale = 0.65) %>% 
    gt::opt_row_striping()
  
  cat( paste('\n\n#### Картограмма', ifelse(params$Has_Code_folding != 'show', ' {.active .unlisted .unnumbered}\n', '')) )
  
  Metric_text <- if (metric == 'bad rates') stringr::str_to_title(paste(target_name, 'rates')) else stringr::str_to_title(metric)
  
  Title_Col <- 
    paste0( Metric_text, ifelse(metric %in% c('gini', 'bad rates'), ', %', '') )
  
  kaz_tm <-  
    dplyr::left_join( x = df , y = KAZ_tbl, by = c('REG_ADDRESS_DISTRICTS' = 'REG_ADDRESS_DISTRICTS') ) %>% 
    dplyr::inner_join( x = geokz::get_kaz_oblasts_map(Year = Year),
                       y = .,
                       by = c('ADM1_PCODE' = 'ADM1_PCODE') ) %>%
    dplyr::mutate( Metric = dplyr::if_else(metric %in% c('gini', 'bad rates'), Metric * 100, Metric)
                   , percent = percent * 100
                   , labels = dplyr::if_else(metric != 'bad rates', 
                                             sprintf('%s (%2.0f)', woe.name, Metric), 
                                             sprintf('%s (%3.1f)', woe.name, Metric) ),
                   , labels = dplyr::if_else(stringr::str_detect(labels, '(NaN)'), woe.name, labels) ) %>%
    dplyr::select(-metric) %>% 
    tmap::tm_shape(.) +
    tmap::tm_borders('azure', lty = 'solid', lwd = 2) +
    tmap::tm_fill( col = 'Metric', title = Title_Col, palette = Pallete, legend.show = FALSE ) +
    tmap::tm_squares( size = 'percent', title.size = 'Regional Share of Loans, %', border.col = 'grey40', 
                      scale = 5, perceptual = TRUE, n = 5, col = 'Metric', title.col = Title_Col,
                      palette = Pallete, legend.hist = TRUE, legend.hist.z = 4,
                      legend.hist.title = paste('Distribution of Regions by', Metric_text) ) +
    tmap::tm_text( text = 'labels', size = 1.0 ) +
    tmap::tm_legend(outside = TRUE, legend.title.size = 1.5, legend.text.size = 1.2 ) +
    tmap::tm_layout( main.title = sprintf( paste0(Title, ' (', Metric_text, ifelse(metric %in% c('gini', 'bad rates'), ' = %3.2f %%)', ' = %3.0f)')), ifelse(metric %in% c('gini', 'bad rates'), Metric_KZ * 100, Metric_KZ)), main.title.size = 1.2 ) +
    # tmap::tm_format('World_wide') +
    tmap::tm_credits('Источник: БДКИ ТОО "ПКБ" , 2024', position = c('right', 'BOTTOM'))
  
  base::suppressMessages( expr = print( kaz_tm ) )
  
  if (metric  %in%  c('bad rates')) {
    
    cat( paste0('\n\n#### Диаграмма', ifelse(params$Has_Code_folding != 'show', ' {.unlisted .unnumbered}\n', '')) )
    
    df %>% 
      data.table::setDT() %>%
      .[, `:=` (
        Share = LoanAmount1 / 1000000,
        type  = factor( data.table::fcase(
          REG_ADDRESS_DISTRICTS == 'Итого по Казахстану', 1L,
          REG_ADDRESS_DISTRICTS %in% c('г. Астана', 'г. Алматы', 'г. Шымкент'), 3L,
          REG_ADDRESS_DISTRICTS == '   Неизвестно', 4L,
          default = 2L
        )) )] %>% 
      OutputLollipopChart( dt = ., x = 'REG_ADDRESS_DISTRICTS', title = title # Global variable
                           , xLab = 'Адм.-терр. единицы 1 уровня (Области)', yLab = 'млн тенге' )
    
  }   # The End of if (metric == 'bad rates')
  
  cat( paste0('\n\n#### Таблица', ifelse(params$Has_Code_folding != 'show', ' {.unlisted .unnumbered}\n', '')) )
  
  print( table )
  
}    # The End of Function `OutputRegionalOblMetric()`


#' OutputLollipopChart - Output Metric using Lollipop Chart
#' @title Output Metric using Lollipop Chart
#' @description Output Metric by Peg Variable (Region or Branch, for example) using A Lollipop Chart
#'
#' @param dt Data.table - Dataset
#' @param x character - A Column Name of Dataset as Peg
#' @param title character - A Title for the Lollipop Chart
#' @param xLab vector - A title of X axis for Peg Variable
#' @param yLab vector - A title of Y axis 
#' 
#' @references \url{https://cran.dev/tmap}
#' 
#' @example  \dontrun{
#'     OutputLollipopChart(dt = DT,
#'                         x = 'Branch', 
#'                         title = 'Эффективность модели по видам экономической деятельности на проверочной выборке',
#'                         xLab = 'Виды экономической деятельности',
#'                         yLab  = 'млн тенге')
#' }
#'  
#' @export OutputLollipopChart

OutputLollipopChart <- function(dt, x = 'Branch', title = NULL, xLab = 'Виды экономической деятельности', yLab = 'Gini, %') {
  # Lollipop Plot by ADM1 or Branches
  
  # Load required parameters and packages
  required_packages <- c('checkmate', 'data.table', 'ggplot2', 'ggpubr')
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but is not installed."))
    }   # The End of if (!requireNamespace(pkg, quietly = TRUE))
  }    # The End of for (pkg in required_packages)
  
  checkmate::assert_data_table(dt, min.rows = 3,min.cols = 2, # all.missing = FALSE, 
                               .var.name = '`dt` Data.table - Dataset')
  checkmate::assert_string(x, .var.name = '`x` character - A Column Name of Dataset as Peg')
  checkmate::assert_string(title, .var.name = '`title` character - A Title for the Lollipop Chart')
  checkmate::assert_string(xLab, .var.name = '`xLab` character - A title of X axis for Peg Variable')
  checkmate::assert_string(yLab, .var.name = '`yLab` character - A title of Y axis')
  
  UseLogScale <- 
    if (abs( max(dt[, 'Share', with = FALSE], na.rm = TRUE) /
             min(dt[, 'Share', with = FALSE], na.rm = TRUE) ) > 10) TRUE else FALSE
  
  suppressMessages( 
    p <- 
      ggpubr::ggdotchart( dt, x = x, y = 'Share', color = 'type',                       # Color by groups
                          palette = c('gold', '#00a6c8', '#FC4E07', 'gray70'),          # Custom color palette
                          sorting = 'descending',                                       # Sort value in descending order
                          add = 'segment', add.params = list(color = 'gray60', linewidth = 1), # Change segment color and size
                          dot.size = if (UseLogScale == TRUE) 11 else 10,               # Large dot size
                          label = round(dt[, base::get('Share')], 1),                   # Add mpg values as dot labels
                          rotate = TRUE,                                                # Rotate vertically
                          title = title, xlab = xLab, 
                          ylab = if (UseLogScale == TRUE) paste(yLab, '(логарифм. шкала)') else yLab,
                          font.label = list( color = 'white', size = if (UseLogScale == TRUE) 9 else 10
                                             , vjust = 0.5),                              # Adjust label parameters
                          ggtheme = ggpubr::theme_pubr()                                # ggplot2 theme
      ) %>% 
      { if (UseLogScale == TRUE) . + ggplot2::scale_y_log10() +
          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_line(color = "gray80", linewidth = 0.4, linetype = 'dashed')
          ) else . } +
      ggplot2::scale_x_discrete( labels = function(x) { sapply(x, function(label) {
        label_upper <- toupper(label)
        if (label == 'Итого по Казахстану') bquote(bold( .(label_upper) )) else 
          if (label %in% c('   Неизвестно', 'г. Астана', 'г. Алматы', 'г. Шымкент')) {
            bquote(italic(.(label)))
          } else label } ) } ) + 
      ggplot2::theme( legend.position = 'none',
                      title = ggplot2::element_text(angle = 0, size = 11),
                      axis.title.x = ggplot2::element_text(angle = 0, size = 14),
                      axis.text.x = ggplot2::element_text(angle = 0, vjust = 1, hjust = 0.5, size = 13),
                      axis.title.y = ggplot2::element_text(angle = 90, size = 12),
                      axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust = 1, size = 9) )
  ) 
  
  p <-
    if (UseLogScale == TRUE) p + ggplot2::scale_y_log10() else p
  
  print( p )
  
}   # The End of function `OutputLollipopChart()`
