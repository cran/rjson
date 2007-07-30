toJSON <- function( x )
{
	if( !is.vector(x) && !is.null(x) ) {
		x <- as.list( x )
		warning("JSON only supports vectors and lists - But I'll try anyways")
	}
	
	if( is.null(x) )
		return( "null" )
	
	if( is.list(x) ) {
		if( is.null(names(x)) )
			stop("not yet handled")
		if( any(duplicated(names(x))) )
			stop( "A JSON list must have unique names" );
		str = "{"
		first_elem = TRUE
		for( n in names(x) ) {
			if( first_elem )
				first_elem = FALSE
			else
				str = paste(str, ',', sep="")
			str = paste(str, deparse(n), ":", toJSON(x[[n]]), sep="")
		}
		str = paste( str, "}", sep="" )
		return( str )
	}
	
	if( length(x) != 1 ) {
		if( !is.null(names(x)) )
			return( toJSON(as.list(x)) )
		str = "["
		first_elem = TRUE
		for( val in x ) {
			if( first_elem )
				first_elem = FALSE
			else
				str = paste(str, ',', sep="")
			str = paste(str, toJSON(val), sep="")
		}
		str = paste( str, "]", sep="" )
		return( str )
	}
	
	if( is.logical(x) )
		return( ifelse(x, "true", "false") )
	
	if( is.character(x) )
		return( gsub("\\/", "\\\\/", deparse(x)) )
	
	if( is.numeric(x) )
		return( as.character(x) )
	
	stop( "shouldnt make it here - unhandled type not caught" )
}

fromJSON <- function( json_str )
{
	if( !is.character(json_str) )
		stop( "JSON objects must be a character string" )
	chars = strsplit(json_str, "")[[1]]
	return( .parseValue( chars, 1)$val )
}

.parseValue <- function( chars, i )
{
	#ignore whitespace
	while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" )
		i = i + 1
			
	ch = chars[i]
	if( ch == "{" ) {
		return( .parseObj( chars, i ) )
	}
	if( ch == "[" ) {
		return( .parseArray( chars, i ) )
	}
	if( ch == "\"" ) {
		return( .parseString( chars, i ) )
	}
	if( any(grep("[0-9\\-]", ch)) ) {
		return( .parseNumber( chars, i ) )
	}
	if( ch == "t" ) {
		return( .parseTrue( chars, i ) )
	}
	if( ch == "f" ) {
		return( .parseFalse( chars, i ) )
	}
	if( ch == "n" ) {
		return( .parseNull( chars, i ) )
	}
	stop("shouldnt reach end of parseValue")
}

.parseObj <- function( chars, i )
{
	obj <- list()
	if( chars[i] != "{" ) stop("error - no openning tag")
	i = i + 1
	
	while( TRUE ) {
	
		#ignore whitespace
		while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" )
			i = i + 1
		
		#get key
		str = .parseString( chars, i )
		key = str$val
		i = str$size
		
		#ignore whitespace
		while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" )
			i = i + 1
		
		#verify seperater
		if( chars[i] != ":" ) stop("error - no seperator")
		i = i + 1
		
		#ignore whitespace
		while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" )
			i = i + 1
		
		#get value
		val = .parseValue( chars, i )
		obj[[key]] <- val$val
		i = val$size
	
		#ignore whitespace
		while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" )
			i = i + 1
		if( chars[i] == "}" ) {
			i = i + 1
			break
		}
		if( chars[i] != "," ) stop("error - no closing tag")
		i = i + 1
	}
	return( list(val=obj, size=i) )
}

.parseArray <- function( chars, i )
{
	useVect <- TRUE
    arr <- list()
	if( chars[i] != "[" ) stop("error - no openning tag")
	i = i + 1

	while( TRUE ) {
	
		#ignore whitespace
		while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" )
			i = i + 1
	
				
		#get value
		val = .parseValue( chars, i )
        arr[[length(arr)+1]] <- val$val
        if( is.list(val$val) || length(val$val) > 1)
        	useVect <- FALSE
        	
		i = val$size
		
		#ignore whitespace
		while( chars[i] == " " || chars[i] == "\t" || chars[i] == "\n" )
			i = i + 1
		
		if( chars[i] == "]" ) { 
			i = i + 1
			break
		}
		if( chars[i] != "," ) stop("error - no closing tag")
		i = i + 1
	}
    if( useVect )
    	arr <- unlist(arr)
	return( list(val=arr, size=i) )
}

.parseString <- function( chars, i )
{
	str_start = i
	if( chars[i] != "\"") stop("error")
	i = i + 1
	
	while( TRUE ) {
		while( chars[i] != "\\" && chars[i] != "\"" )
			i = i + 1
		if( chars[i] == "\\" )
			i = i + 2 #skip the next char
		else
			break
	}
	str_end = i
	i = i + 1
	return(list(
		val=eval(parse( text=paste(chars[str_start:str_end], collapse="") )), 
		size=i ))
}

.parseNumber <- function( chars, i )
{
	str_start = i

	if( chars[i] == "-" )
		i = i + 1
	
	if( chars[i] == "0" ) {
		i = i + 1
		if( any(grep("[1-9]", chars[i])) ) stop("JSON specs don't allow a number like \"012\"")
	} else if( any(grep("[1-9]", chars[i])) ) {
		i = i + 1
		while( any(grep("[0-9]", chars[i])) )
			i = i + 1
	} else {
		stop( "doesn't look like a valid JSON number" )
	}
	
	if( chars[i] == "." ) {
		i = i + 1
		while( any(grep("[0-9]", chars[i])) )
			i = i + 1
	}
	
	if( chars[i] == "e" || chars[i] == "E" ) {
		i = i + 1
		if( chars[i] == "-" || chars[i] == "+" )
			i = i + 1
		while( any(grep("[0-9]", chars[i])) )
			i = i + 1
	}
	str_end = i-1
	
	return(list(
		val=eval(parse( text=paste(chars[str_start:str_end], collapse="") )), 
		size=i ))
}

.parseTrue <- function( chars, i )
{
	if( paste(chars[i:(i+3)], collapse="") == "true" )
		return( list(val=TRUE,size=i+4) )
	stop("error parsing true value (maybe the word starts with t but isnt true)")
}

.parseFalse <- function( chars, i )
{
	if( paste(chars[i:(i+4)], collapse="") == "false" )
		return( list(val=FALSE,size=i+5) )
	stop("error parsing false value (maybe the word starts with f but isnt false)")
}

.parseNull <- function( chars, i )
{
	if( paste(chars[i:(i+3)], collapse="") == "null" )
		return( list(val=FALSE,size=i+4) )
	stop("error parsing null value (maybe the word starts with n but isnt null)")
}





