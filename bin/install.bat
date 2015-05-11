SET sdir=%~dp0
pushd %sdir:~0,-1%\..

git clone https://github.com/kahless/russian-address-parser parser

cabal sandbox init
cabal sandbox add-source parser
cabal install

popd
