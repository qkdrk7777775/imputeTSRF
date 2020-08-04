#' multivariable Time Series Data Imputation method
#'
#' This function is a package created based on the function of 'imputeTS', 'MissForest' packages algorithm
#' @param 'NAdata' is data with missing columns in multiple columns
#' @export
#' @examples
#' # imputation value return
#' impute=imputeTSRF(NAdata,return_kalman = T)
#' # Performance evaluation is performed for cases where NAdata is missing for each column.
#' idx1=impute_valid(data,impute[[1]],NAdata)

imputeTSRF=function(NAdata,return_kalman=F,return_model=T,smooth=T,seed=1,...){
  temp=NAdata
  numeric_col=names(which(sapply(NAdata,is.numeric)))
  impute_kalman=list();model=list()

  for(i in numeric_col){
    NAdata[,i]=imputeTS::na_kalman(NAdata[,i],smooth=smooth,...)
    message(paste0(i,' impute using Kalman'))
  }
  if(return_kalman){
    impute_kalman=NAdata
  }
  for(col_order in numeric_col[order(apply(is.na(temp[numeric_col]),2,sum),decreasing = T)]){
    set.seed(seed)
    fit=ranger::ranger(data=na.omit(temp[numeric_col]),as.formula(paste0(col_order,'~.')),...)
    temp[is.na(temp[col_order]),col_order]=predict(fit,NAdata[is.na(temp[col_order]),])$predictions
    if(return_model){
      model[[col_order]]=fit
    }
    message(paste0(col_order,' impute using forest'))
  }
  if(length(model)!=0){
    return(list(temp,impute_kalman,model))
  }else{
    return(temp)
  }
}
