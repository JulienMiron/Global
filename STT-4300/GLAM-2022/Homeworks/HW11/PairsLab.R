PairsLab <- function(data,label){
	pairs(data,panel=function(x,y){z <- label;points(x,y,type="n");text(x,y,z)})
}
