#include <R.h>
#include <Rdefines.h>

#define DEFAULT_VECTOR_START_SIZE 10 /* allocate vectors this size to start with, then grow them as needed */
#define MAX_NUMBER_BUF 256

/* converts a single string into a list of (key, value) lists
   i.e. convert "nokey keyname=foo" into the following R list:
   list(list(value="nokey"), list(key="keyname", value="foo"))
   
   args: str - R character string
         env - R environment for use in calling R code
         fname - R character string of filename for use in error reporting
         lineNum - R numeric value for line number for use in error reporting
   
   return: R list of 1-element, or 2-element lists.
           i.e.: "nokey keyname=foo" becomes
           list(list(value="nokey"), list(key="keyname", value="foo"))
*/
SEXP parseValue( const char *s, const char **next_ch );
SEXP parseNull( const char *s, const char **next_ch );
SEXP parseTrue( const char *s, const char **next_ch );
SEXP parseFalse( const char *s, const char **next_ch );
SEXP parseString( const char *s, const char **next_ch );
SEXP parseNumber( const char *s, const char **next_ch );
SEXP parseArray( const char *s, const char **next_ch );
SEXP parseList( const char *s, const char **next_ch );

SEXP mkError( const char* format, ...);


SEXP mkError( const char* format, ...)
{
	SEXP p, classp;
	char buf[ 256 ];
	va_list args;
	va_start( args, format );
	vsnprintf( buf, 256, format, args );
	va_end( args );

	PROTECT( p = allocVector(STRSXP, 1) );
	SET_STRING_ELT( p, 0, mkChar( buf ) );
	PROTECT( classp = allocVector(STRSXP, 1) );
	SET_STRING_ELT( classp, 0, mkChar( "try-error" ) );
	SET_CLASS( p, classp );
	UNPROTECT( 2 );
	return p;
}



SEXP fromJSON( SEXP str_in )
{
	const char *s = CHAR(STRING_ELT(str_in,0));
	const char *next_ch;
	return parseValue( s, &next_ch );
}

SEXP parseValue( const char *s, const char **next_ch )
{
	int i = 0;

	/* ignore whitespace */
	while( s[ i ] == ' ' || s[ i ] == '\t' || s[ i ] == '\n' )
		i++;

	if( s[i] == '{' ) {
		return parseList( s + i, next_ch );
	}
	if( s[i] == '[' ) {
		return parseArray( s + i, next_ch );
	}
	if( s[i] == '\"' ) {
		return parseString( s + i, next_ch );
	}
	if( ( s[i] >= '0' && s[i] <= '9' ) || *s == '-' ) {
		return parseNumber( s + i, next_ch );
	}
	if( s[i] == 't' ) {
		return parseTrue( s + i, next_ch );
	}
	if( s[i] == 'f' ) {
		return parseFalse( s + i, next_ch );
	}
	if( s[i] == 'n' ) {
		return parseNull( s + i, next_ch );
	}

	return mkError( "unexpected string (or empty string)'\n" );
}

SEXP parseNull( const char *s, const char **next_ch )
{
	if( strncmp( s, "null", 4 ) == 0 ) {
		*next_ch = s + 4;
		return R_NilValue;
	}
	return mkError( "parseNull: expected to see 'null' - likely an unquoted string starting with 'n'.\n" );
}

SEXP parseTrue( const char *s, const char **next_ch )
{
	SEXP p;
	if( strncmp( s, "true", 4 ) == 0 ) {
		*next_ch = s + 4;
		PROTECT( p = NEW_LOGICAL( 1 ) );
		LOGICAL( p )[ 0 ] = TRUE;
		UNPROTECT( 1 );
		return p;
	}
	return mkError( "parseTrue: expected to see 'true' - likely an unquoted string starting with 't'.\n" );
}

SEXP parseFalse( const char *s, const char **next_ch )
{
	SEXP p;
	if( strncmp( s, "false", 5 ) == 0 ) {
		*next_ch = s + 5;
		PROTECT( p = NEW_LOGICAL( 1 ) );
		LOGICAL( p )[ 0 ] = FALSE;
		UNPROTECT( 1 );
		return p;
	}
	return mkError( "parseFalse: expected to see 'false' - likely an unquoted string starting with 'f'.\n" );
}

SEXP parseString( const char *s, const char **next_ch )
{
	SEXP p;
	/*assert( s[ 0 ] == '"' );*/
	int i = 1; /*skip the start quote*/

	int buf_size = 256;
	char *buf = (char*) malloc( buf_size );
	int buf_i = 0;
	if( buf == NULL )
		return mkError( "error allocating memory in parseString" );
	if( sizeof( char ) != 1 )
		return mkError( "parseString sizeof(char) != 1" );

	int copy_start = i;
	int bytes_to_copy;

	while( 1 ) {
		while( s[ i ] != '\\' && s[ i ] != '"' && s[ i ] != '\0' )
			i++;
		if( s[ i ] == '\0' ) {
			return mkError( "unclosed string\n" );
		}

		if( s[ i ] == '\\' ) {
			if( s[ i + 1 ] == '\0' ) {
				return mkError( "unclosed string\n" );
			}
			if( s[ i + 2 ] == '\0' ) {
				return mkError( "unclosed string\n" );
			}

			/* save string chunk from copy_start to i-1 */
			bytes_to_copy = i - copy_start;
			if( bytes_to_copy > 0 ) {
				if( buf_size - 1 <= i ) {
					/* grow memory */
					buf = realloc( buf, i * 2 );
					if( buf == NULL )
						return mkError( "error allocating memory in parseString" );
				}
				memcpy( buf + buf_i, s + copy_start, bytes_to_copy );
				buf_i += bytes_to_copy;
			}
			i++;

			/* save s[i] */
			switch( s[ i ] ) {
				case '"':
				case '\\':
				case '/':
					buf[ buf_i ] = s[ i ];
					break;
				case 'b':
					buf[ buf_i ] = '\b';
					break;
				case 'f':
					buf[ buf_i ] = '\f';
					break;
				case 'n':
					buf[ buf_i ] = '\n';
					break;
				case 'r':
					buf[ buf_i ] = '\r';
					break;
				case 't':
					buf[ buf_i ] = '\t';
					break;
				case 'u':
					return mkError( "unicode is not (yet) supported by rjson - sorry" );
					break;
				default:
					return mkError( "unexpected escaped character '\\%c'", s[ i ] );
					break;
			}

			i++; /* move to next char */
			copy_start = i;
			buf_i++;
		} else {
			/*must be a quote that caused us the exit the loop, first, save remaining string data*/
			bytes_to_copy = i - copy_start;
			if( bytes_to_copy > 0 ) {
				if( buf_size - 1 <= i ) {
					/* grow memory */
					buf = realloc( buf, i * 2 );
					if( buf == NULL )
						return mkError( "error allocating memory in parseString" );
				}
				memcpy( buf + buf_i, s + copy_start, bytes_to_copy );
				buf_i += bytes_to_copy;
			}
			buf[ buf_i ] = '\0';
			break; /*exit the loop*/
		}
	}

	*next_ch = s + i + 1;
	PROTECT(p=allocVector(STRSXP, 1));
	SET_STRING_ELT(p, 0, mkChar( buf ));
	free( buf );
	UNPROTECT( 1 );
	return p;
}

void setArrayElement( SEXP array, int unsigned i, SEXP val )
{
	if( IS_LOGICAL( array ) )
		LOGICAL( array )[ i ] = LOGICAL( val )[ 0 ];
	else if( IS_INTEGER( array ) )
		INTEGER( array )[ i ] = INTEGER( val )[ 0 ];
	else if( IS_NUMERIC( array ) )
		REAL( array ) [ i ] = REAL( val )[ 0 ];
	else if( IS_CHARACTER( array ) )
		SET_STRING_ELT( array, i, STRING_ELT(val, 0) ); /*TODO fixme val must be a single char, not vector*/
/*	else if( IS_COMPLEX( array ) )
		COMPLEX( array [ i ] = COMPLEX( val )[ 0 ]; */
	else
		Rprintf( "unsupported SEXPTYPE: %i\n", TYPEOF( array ) );
}

SEXP parseArray( const char *s, const char **next_ch )
{
	SEXP p = NULL, array = NULL;
	/*assert( *s == '[' )*/
	s++; /*move past '['*/
	int objs = 0;
	int is_list = FALSE;
	SEXPTYPE  p_type = -1;
	unsigned int array_i = 0;

	while( 1 ) {
		/*ignore whitespace*/
		while( *s == ' ' || *s == '\t' || *s == '\n' )
			s++;
		if( *s == '\0' ) {
			UNPROTECT( objs );
			return mkError( "incomplete array\n" );
		}

		if( *s == ']' ) {
			return allocVector(VECSXP, 0);
		}

		PROTECT( p = parseValue( s, next_ch ) );
		objs++;
		s = *next_ch;

		if( array == NULL ) {
			/*create a vector of type that matches p*/
			if( GET_LENGTH( p ) != 1 )
				p_type = VECSXP;
			else
				p_type = TYPEOF( p );
			PROTECT( array = allocVector( p_type, DEFAULT_VECTOR_START_SIZE ) );
			objs++;
			is_list = ( p_type == VECSXP );
		}

		/*check array type matches*/
		if( is_list == FALSE && TYPEOF( p ) != TYPEOF( array ) ) {
			Rprintf( "changing to list\n" );
			PROTECT( array = coerceVector( array, VECSXP ) );
			objs++;
			is_list = TRUE;
		}

		/*checksize*/
		unsigned int array_size = GET_LENGTH( array );
		if( array_i >= array_size ) {
			SET_LENGTH( array, array_size * 2 );
		}

		/*save element*/
		if( is_list == TRUE )
			SET_VECTOR_ELT( array, array_i, p);
		else
			setArrayElement( array, array_i, p );
		array_i++;

		/*ignore whitespace*/
		while( *s == ' ' || *s == '\t' || *s == '\n' )
			s++;
		if( *s == '\0' ) {
			UNPROTECT( objs );
			return mkError( "incomplete array\n" );
		}

		/*end of array*/
		if( *s == ']' ) {
			break;
		}

		/*more elements to come*/
		if( *s == ',' ) {
			s++;
		} else if( *s == '\0' ) {
			UNPROTECT( objs );
			return mkError( "incomplete array\n" );
		} else {
			UNPROTECT( objs );
			return mkError( "unexpected character: %c\n", *s );
		}
	}

	/*trim to the correct size*/
	SET_LENGTH( array, array_i );

	*next_ch = s + 1;

	UNPROTECT( objs );
	return array;
}

SEXP parseList( const char *s, const char **next_ch )
{
	SEXP key, val, list, list_names;
	/*assert( *s == '{' )*/
	s++; /*move past '{'*/
	int objs = 0;
	unsigned int list_i = 0;

	PROTECT( list = allocVector( VECSXP, DEFAULT_VECTOR_START_SIZE ) );
	PROTECT( list_names = allocVector( STRSXP, DEFAULT_VECTOR_START_SIZE ) );
	objs += 2;

	while( 1 ) {
		/*ignore whitespace*/
		while( *s == ' ' || *s == '\t' || *s == '\n' )
			s++;
		if( *s == '\0' ) {
			UNPROTECT( objs );
			return mkError( "incomplete list\n" );
		}

		if( *s == '}' ) {
			return allocVector(VECSXP, 0);
		}

		/*get key*/
		PROTECT( key = parseValue( s, next_ch ) );
		objs++;
		s = *next_ch;

		if( IS_CHARACTER( key ) == FALSE ) {
			UNPROTECT( objs );
			return mkError( "list keys must be strings\n" );
		}

		/*ignore whitespace*/
		while( *s == ' ' || *s == '\t' || *s == '\n' )
			s++;
		if( *s != ':' ) {
			UNPROTECT( objs );
			return mkError( "incomplete list\n" );
		}
		s++; /*move past ':'*/

		/*ignore whitespace*/
		while( *s == ' ' || *s == '\t' || *s == '\n' )
			s++;
		if( *s == '\0' ) {
			UNPROTECT( objs );
			return mkError( "incomplete list\n" );
		}

		/*get value*/
		PROTECT( val = parseValue( s, next_ch ) );
		objs++;
		s = *next_ch;

		/*checksize*/
		unsigned int list_size = GET_LENGTH( list );
		if( list_i >= list_size ) {
			SET_LENGTH( list, list_size * 2 );
			SET_LENGTH( list_names, list_size * 2 );
		}

		/*save key and value*/
		SET_STRING_ELT( list_names, list_i, STRING_ELT(key, 0) );
		SET_VECTOR_ELT( list, list_i, val );
		list_i++;

		/*ignore whitespace*/
		while( *s == ' ' || *s == '\t' || *s == '\n' )
			s++;
		if( *s == '\0' ) {
			UNPROTECT( objs );
			return mkError( "incomplete list\n" );
		}

		/*end of list*/
		if( *s == '}' ) {
			break;
		}

		/*more elements to come*/
		if( *s == ',' ) {
			s++;
		} else if( *s == '\0' ) {
			UNPROTECT( objs );
			return mkError( "incomplete list\n" );
		} else {
			UNPROTECT( objs );
			return mkError( "unexpected character: %c\n", *s );
		}
	}

	/*trim to the correct size*/
	SET_LENGTH( list, list_i );
	SET_LENGTH( list_names, list_i );

	/*set names*/
	setAttrib( list, R_NamesSymbol, list_names );

	*next_ch = s + 1;

	UNPROTECT( objs );
	return list;

}

SEXP parseNumber( const char *s, const char **next_ch )
{
	SEXP p;
	const char *start = s;
	char buf[ MAX_NUMBER_BUF ];

	if( *s == '-' )
		s++;

	if( *s == '\0' ) {
		return mkError( "unexpected character: %c\n", *s );
	}

	if( *s == '0' ) {
		s++;
		if( ( *s >= '0' && *s <= '9' ) || *s == 'x' ) {
			return mkError( "hex or octal is not valid json\n" );
		}
	}

	while( *s >= '0' && *s <= '9' ) {
		s++;
	}

	if( *s == '.' ) {
		s++;
		while( *s >= '0' && *s <= '9' )
			s++;
	}

	/*exponential*/
	if( *s == 'e' || *s == 'E' ) {
		s++;
		if( *s == '+' || *s == '-' )
			s++;
		while( *s >= '0' && *s <= '9' )
			s++;
	}
	
	unsigned int len = s - start;
	if( len >= MAX_NUMBER_BUF ) {
		return mkError( "buffer issue parsing number: increase MAX_NUMBER_BUF (in parser.c) current value is %i\n", MAX_NUMBER_BUF );
	}

	/*copy to buf, which is used with atof*/
	strncpy( buf, start, len );
	buf[ len ] = '\0';

	*next_ch = s;

	PROTECT( p = allocVector( REALSXP, 1 ) );
	REAL(p)[ 0 ] = atof( buf );
	UNPROTECT( 1 );
	return p;
}

