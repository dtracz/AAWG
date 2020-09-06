
compile:
	ghc -dynamic Server.hs -o test.x -lssd -L./net/build/ -optl-Wl,-rpath,'$ORIGIN/net/build/' -XPackageImports
