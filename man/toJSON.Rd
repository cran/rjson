\name{toJSON}
\alias{toJSON}
\title{Convert R To JSON}

\description{ Convert an R object into a corresponding JSON object. }

\usage{toJSON( x )}

\arguments{
\item{x}{a vector or list to convert into a JSON object}
}

\value{a string containing the JSON object}

\seealso{
\code{\link{fromJSON}}
}

\examples{
x <- list( alpha = 1:5, beta = "Bravo", 
           gamma = list(a=1:3, b=NULL), 
           delta = c(TRUE, FALSE) )
json <- toJSON( x )
fromJSON( json )


#named vectors are treated as JSON objects (lists)
toJSON(islands[1:4])


#data.frames must be converted into a list before converting into JSON
plot(cars, pch=2)
json_cars <- toJSON(as.list(cars))
points( data.frame( fromJSON( json_cars ) ), col="red", pch=3 )
}

%TODO find better keyword
\keyword{interface}
