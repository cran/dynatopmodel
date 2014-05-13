CheckParams <-
function(obj, names, warn="message", stop=F)
{
  bad <- which(!names %in% obj)  
  if(length(bad)>0)
  {
    msg <- paste("Parameters not found ", paste(names[bad], "\n"))   
    if(warn=="warn"){     
      warning(msg)
    }
    else if("warn"=="message"){
      message(msg)  
    }
    else if("warn"=="stop"){
      stop(msg)
    }
    return(FALSE)
  }
  return(TRUE)
}
