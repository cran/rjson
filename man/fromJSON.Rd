\name{fromJSON}
\alias{fromJSON}
\title{Convert JSON To R}

\description{ Convert a JSON object into an R object. }

\usage{fromJSON( json_str )}

\arguments{
\item{json_str}{a JSON object to convert}
}

\value{R object that corresponds to the JSON object}

\seealso{
\code{\link{toJSON}}
}

\examples{

#watch out for R turning null into FALSE
fromJSON( "[true, false, null]" )
##[1]  TRUE FALSE __FALSE__

#but if you use a hash, this will be avoided
fromJSON( '{"a":true, "b":false, "c":null}' )

}

\keyword{interface}
