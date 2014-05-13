GetUserResponse <-
function(prompt, 
							def="y" #  result if user hits enter - default is "yes" 
							)
{
	resp <- readline(prompt)
	if(resp == ""){resp<-def}
	return(resp=="y")
}
