recurfind = $(shell find $(1) -name '$(2)')

BUILT = .built
CONFIGURED = .configured

SANDBOX = cabal.sandbox.config
INSTALL_ARGS = -j --force-reinstalls
CABAL_FILE = $(wildcard *.cabal)
HS_SOURCES = $(call recurfind, src, [^.]*.hs)

################################################################################

$(SANDBOX) :
	-mkdir -p vendor
	-cd vendor; \
	git clone -b development https://github.com/haskell-distributed/distributed-process; \
	git clone -b development https://github.com/haskell-distributed/network-transport; \
	git clone -b development https://github.com/haskell-distributed/network-transport-inmemory; \
	git clone -b development https://github.com/haskell-distributed/distributed-process-platform;
	cabal sandbox init
	cabal sandbox add-source vendor/network-transport
	cabal sandbox add-source vendor/network-transport-inmemory
	cabal sandbox add-source vendor/distributed-process
	cabal sandbox add-source vendor/distributed-process-platform

setup : $(SANDBOX)

unsetup :
	cabal sandbox delete

################################################################################

$(CONFIGURED) : $(CABAL_FILE)
	cabal install -j --only-dependencies
	cabal configure --enable-tests
	touch $(CONFIGURED)

$(BUILT) : $(CONFIGURED) $(HS_SOURCES)
	cabal build
	touch $(BUILT)

build : setup $(BUILT)
testloop : build
	./dist/build/testloop/testloop
