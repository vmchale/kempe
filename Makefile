.PHONY: install clean

install:
	cabal install exe:kc --overwrite-policy=always

clean:
	rm -rf dist-newstyle *.rlib *.d *.rmeta *.o
