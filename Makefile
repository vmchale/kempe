.PHONY: install

install:
	cabal install exe:kc --overwrite-policy=always
