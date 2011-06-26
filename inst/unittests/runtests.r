library( rjson )
library( RUnit )


path <- system.file( "unittests", package="rjson" )
#path <- "."
test.suite <- defineTestSuite( "json unittests", dirs = path, testFileRegexp = "^test\\..*\\.[rR]$" )
runTestSuite( test.suite, verbose = 100 )
