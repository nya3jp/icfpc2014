#sudo apt-get -y update
#sudo apt-get -y install git ghc emacs screen cabal-install make libghc-zlib-dev nodejs nodejs-dev
#wget http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-x86_64-unknown-linux-deb7.tar.bz2 -O ghc.tar.bz2
#tar xf ghc.tar.bz
#(cd ghc-7.8.3; ./configure; sudo make install)
#cabal install cabal-install
#sudo cp $HOME/.cabal/bin/cabal `which cabal`
#cabal --version
(cd LambdaManP; cabal install --enable-shared)
