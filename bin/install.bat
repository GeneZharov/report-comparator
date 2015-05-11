SET sdir=%~dp0
pushd %sdir:~0,-1%\..

REM git clone https://github.com/kahless/russian-address-parser parser

cabal sandbox init
cabal sandbox add-source parser
REM cabal install --dependencies-only
REM cabal configure
REM cabal build
cabal install

popd
