
add_missing_zeros = function( data_frame, unique_sample_ID_colname, sample_colname, species_subset=NULL, species_colname, if_multiple_records="Error", verbose=TRUE, na.rm=FALSE, save_name=NULL ){

  if( is.null(save_name) || !file.exists(save_name) ){
    # set of species and samples
    species_set = unique( data_frame[,species_colname] )
    if( !is.null(species_subset)) species_set = species_subset[which(species_subset%in%species_set)]
    unique_sample_ID_set = unique(data_frame[,unique_sample_ID_colname])
    if( verbose==TRUE ){
      message( "Species to include: ", paste(species_set,collapse=", ") )
      message( "Number of samples to include for each species: ",length(unique_sample_ID_set) )
    }

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
          if( if_multiple_records=="Error") stop( "multiple unique-IDs with catch for same species")
          if( if_multiple_records=="Combine") temp_data_frame[i,sample_colname] = sum(data_frame[Match,sample_colname])
          if( if_multiple_records=="First") temp_data_frame[i,sample_colname] = data_frame[Match[1],sample_colname]
        }
        if( length(Match)==1 ){
          temp_data_frame[i,sample_colname] = data_frame[Match,sample_colname]
        }
        if( verbose==TRUE & (i%%1000)==0 ) message( "Processing row ",i," for ",species_set[p] )
      }
      new_data_frame = rbind(new_data_frame, temp_data_frame)
      if( verbose==TRUE ) message( "Finished processing for ",species_set[p] )
    }

    # Relevel
    new_data_frame[,species_colname] = factor(new_data_frame[,species_colname], levels=species_set)

    # Exclude NAs
    if( na.rm==TRUE ){
      data_frame = na.omit( data_frame )
    }

    # Sanity checks
    Which = which( data_frame[,species_colname] %in% species_set )
    if( if_multiple_records!="First"){
      if( sum(new_data_frame[,sample_colname],na.rm=TRUE) != sum(data_frame[Which,sample_colname],na.rm=TRUE) ) stop( "missing rows in new data frame")
    }

    # Save
    if( !is.null(save_name) ) save(new_data_frame, file=save_name)
  }else{
    load(file=save_name)
    if( verbose==TRUE ){
      species_set = unique( data_frame[,species_colname] )
      unique_sample_ID_set = unique(data_frame[,unique_sample_ID_colname])
      message("Loading from ", save_name)
      message( "Species included: ", paste(species_set,collapse=", ") )
      message( "Number of samples included for each species: ",length(unique_sample_ID_set) )
    }
  }

  # Return new data frame
  return( new_data_frame )
}

