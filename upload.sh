#!/bin/bash -xe

cabal clean
cabal configure -fproduction
cabal build
rm -rf static/tmp
strip dist/build/haskellers/haskellers
bzip2 dist/build/haskellers/haskellers
scp -i ~/.ec2/ec2-keypair.pem -r favicon.ico static dist/build/haskellers/haskellers.bz2 ubuntu@www.haskellers.com:/home/ubuntu/haskellers
ssh ubuntu@www.haskellers.com 'cd haskellers && sh update.sh'
