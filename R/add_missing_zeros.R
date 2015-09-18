
add_missing_zeros = function( data_frame, unique_sample_ID_colname, sample_colname, species_subset=NULL, species_colname, combine_multiple_records=FALSE, verbose=TRUE ){

  # set of species and samples
  species_set = unique( data_frame[,species_colname] )
  if( !is.null(species_subset)) species_set = intersect( species_set, species_subset )
  if( verbose==TRUE ) message( "Species to include: ", paste(species_set,collapse=", ") )
  unique_sample_ID_set = unique(data_frame[,unique_sample_ID_colname])
  if( verbose==TRUE ) message( "Number of samples to include for each species: ",length(unique_sample_ID_set) )
  
  # Data frame for unique tows
  unique_data_frame = data_frame[match( unique_sample_ID_set, data_frame[,unique_sample_ID_colname]),]
  unique_data_frame[,sample_colname] = 0
  
  # Loop through species
  new_data_frame = NULL
  for(p in 1:length(species_set)){
    temp_data_frame = unique_data_frame
    temp_data_frame[,species_colname] = species_set[p]
    for(i in 1:nrow(temp_data_frame)){               #  
      #Match = which( data_frame[,unique_sample_ID_colname]==unique_sample_ID_set[i] & data_frame[,species_colname]==species_set[p])
      Match = which( data_frame[,unique_sample_ID_colname]==unique_sample_ID_set[i] )
      Match = Match[which( data_frame[Match,species_colname]==species_set[p]) ]
      if( length(Match)>1 ){
        if( combine_multiple_records==FALSE) stop( "multiple unique-IDs with catch for same species")
        if( combine_multiple_records==TRUE) temp_data_frame[i,sample_colname] = sum(data_frame[Match,sample_colname])
      }
      if( length(Match)==1 ){
        temp_data_frame[i,sample_colname] = data_frame[Match,sample_colname]
      }
      if( verbose==TRUE & (i%%1000)==0 ) message( "Processing row ",i," for ",species_set[p] )
    }
    new_data_frame = rbind(new_data_frame, temp_data_frame)
    if( verbose==TRUE ) message( "Finished processing for ",species_set[p] )
  }

  # Sanity checks
  Which = which( data_frame[,species_colname] %in% species_set )
  if( sum(new_data_frame[,sample_colname]) != sum(data_frame[Which,sample_colname]) ) stop( "missing rows in new data frame")
  
  # Return new data frame
  return( new_data_frame )
}

