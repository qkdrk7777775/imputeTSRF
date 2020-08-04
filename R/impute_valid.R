#' multivariable Time Series Data performance evaluation method
#'
#' This function is a package created based on the function of 'spTimer' packages algorithm
#' @param NA ,b : two numbers to be operated
#' @keywords subtract a from b
#' @export
#' @examples
#' # imputation value return
#' impute=imputeTSRF(NAdata,return_kalman = T)
#' # Performance evaluation is performed for cases where NAdata is missing for each column.
#' idx1=impute_valid(data,impute[[1]],NAdata)
impute_valid=function(data,imputeData,NAdata,col_name=NULL){
  if(is.null(col_name)){
    valid_list=names(which(sapply(data,is.numeric)))}
  bind_temp=list()
  for(col in valid_list){
    message(col)
    if((class(data)[1])!='data.frame'){
      na_idx=which(is.na(NAdata[[col]]))
      bind_temp[[col]]=spTimer::spT.validation(data[[col]][na_idx],imputeData[[col]][na_idx],...)
    }else{
      na_idx=which(is.na(NAdata[,col]))
      bind_temp[[col]]=spTimer::spT.validation(data[na_idx,col],imputeData[na_idx,col],...)
    }
  }
  idx=data.frame(dplyr::bind_rows(bind_temp))
  rownames(idx)=names(bind_temp[[1]])
  return(idx)
}
