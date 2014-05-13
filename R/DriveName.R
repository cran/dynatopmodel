DriveName <-
function(pth)
{
  pth <- path.expand(pth)
  return(strsplit(pth, split="/")[[1]][1])
}
