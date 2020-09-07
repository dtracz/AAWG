
compile:
	ghc -dynamic Server.hs -o server.x -lssd -L./net/build/ -optl-Wl,-rpath,'$$ORIGIN/net/build/' -XPackageImports
	mkdir -p tmp
	rm *.hi
	rm *.o
	rm -f tmp/*


clean:
	rm server.x

