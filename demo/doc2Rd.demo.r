# Work out where this file lives!

textfilename <- attr( pos.to.env( which( search()=='package:mvbutils')), 'path')
textfilename <- file.path( textfilename, 'demo', 'sample.fun.rrr')

sample.fun <- source.mvb( textfilename)

cat( "'sample.fun' has been read in from \"sample.fun.rrr\" by 'source.mvb'.")
cat( "Press <ENTER> to see its informal help: ")
readline()
help( sample.fun)

cat( "Press <ENTER> to see 'cat( doc2Rd( sample.fun), sep='\n')': ")
readline()
cat( doc2Rd( sample.fun), sep='\n')

