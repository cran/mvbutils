# Best to call source.mvb( "source.mvb.demo.rrr") directly.
# If this file gets 'source'd instead, it tries to work out where it is...

textfilename <- attr( pos.to.env( which( search()=='package:mvbutils')), 'path')
textfilename <- file.path( textfilename, 'demo', 'source.mvb.demo.rrr')
cat( "Actually calling source.mvb('", textfilename, "')\n", sep='')

result <- source.mvb( textfilename, echo=TRUE)
