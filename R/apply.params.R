apply.params <-
function(proj, params, which=1:length(proj$disc))
{
  if(length(proj$disc)==0){return(proj)}
  params <- params[which(names(params) %in% colnames(proj$disc[[1]]$groups))]
  if(length(params)==0){return(proj)}
  
  proj$disc[which] <- lapply(proj$disc[which],
                             function(disc)
                             {
                               
                               vals <- matrix(rep(unlist(params),nrow(disc$groups)), nrow=nrow(disc$groups), byrow=T)
                               disc$groups[,names(params)]<- vals           
                               
                               return(disc)
                             } 
  )
  return(proj)
}
