# Work out where this file lives!

textfilename <- attr( pos.to.env( which( search()=='package:mvbutils')), 'path')
textfilename <- file.path( textfilename, 'demo', 'original.dochelp.rrr')

original.dochelp <- source.mvb( textfilename)

cat( "'original.dochelp' has been read in from \"original.dochelp.rrr\" by 'source.mvb'.")
cat( "Press <ENTER> to see 'print( original.dochelp)': ")
readline()
print( original.dochelp)

cat( "Press <ENTER> to see 'write.sourceable.function( original.dochelp, stdout())': ")
readline()
write.sourceable.function( original.dochelp, stdout())

cat( "Press <ENTER> to see the original flat-format help for 'dochelp': ")
readline()

help( original.dochelp)

