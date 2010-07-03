library( rjson )
library( RUnit )


path <- system.file( "unittests", package="rjson" )
path <- "C:\\Users\\alex\\Documents\\programming\\R\\rjson\\rjson\\inst\\unittests"
test.suite <- defineTestSuite( "json unittests", dirs = path, testFileRegexp = "^test\\..*\\.[rR]$" )
runTestSuite( test.suite, verbose = 100 )
