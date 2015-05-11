SET sdir=%~dp0
pushd %sdir:~0,-1%\..

git pull
pushd parser & git pull & popd
cabal install

popd
