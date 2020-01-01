#' dreport.single Function
#' 
#' This function allows you to get duncan multiple comparison analysis result in the form of manuscript
#' @param 
#' @keywords dreport.single
#' @export
#' @example 
dreport.single<-function(x, y){
  model<-aov(y~x)
  com<-duncan.test(model, 'x', alpha = 0.05)
  arrange(com$groups, by=row.names(com$groups))->a
  paste0(round(a[[1]], digits = 2), a[[2]])->b
  round(sqrt(com$statistics[[1]]), digits = 2)->c
  format(summary(model)[[1]][5]$`Pr(>F)`[1], format = "e", digits = 2)->d
  out<-as.data.frame(matrix(c(b, c, d), 1, 5))
  colnames(out)<-c(levels(x), "SEM", "P value")
  rownames(out)<-paste(deparse(substitute(y)))
  env<-c(var_x = paste(deparse(substitute(x))),
         var_y = paste(deparse(substitute(y))))
  output <-list(env = env, summary = summary(model), multcom = com, 
                abstract = out)
  output
}