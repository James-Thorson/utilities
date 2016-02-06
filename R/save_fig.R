
save_fig = function( filename, width, height, units="in", res=200, type="png", ...){
  if( type=="png" ) png( filename=filename, width=width, height=height, units=units, res=res, ... )
  if( type=="pdf" ) pdf( filename=filename, width=width, height=height, units=units, res=res, ... )
  if( type=="tif" | type=="tiff" ) tiff( filename=filename, width=width, height=height, units=units, res=res, ... )
}
