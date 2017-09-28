#!/bin/bash

function run {
    NAME=$1
    shift
    echo "-=-=- $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    "$@"
    CODE=$?
    if [ $CODE -ne 0 ]; then
        echo "-=-=- $NAME failed! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
        exit $CODE
    else
        echo "-=-=- End of $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    fi
}

echo ** OCAMLROOT=$OCAMLROOT

#echo "APPVEYOR_PULL_REQUEST_NUMBER = $APPVEYOR_PULL_REQUEST_NUMBER"
#echo "APPVEYOR_PULL_REQUEST_TITLE = $APPVEYOR_PULL_REQUEST_TITLE"
#echo "APPVEYOR_PULL_REQUEST_HEAD_REPO_NAME = $APPVEYOR_PULL_REQUEST_HEAD_REPO_NAME"
#echo "APPVEYOR_PULL_REQUEST_HEAD_REPO_BRANCH = $APPVEYOR_PULL_REQUEST_HEAD_REPO_BRANCH"
#echo "APPVEYOR_PULL_REQUEST_HEAD_COMMIT = $APPVEYOR_PULL_REQUEST_HEAD_COMMIT"
#echo "APPVEYOR_REPO_PROVIDER = $APPVEYOR_REPO_PROVIDER"
#echo "APPVEYOR_REPO_NAME = $APPVEYOR_REPO_NAME"
#echo "APPVEYOR_REPO_BRANCH = $APPVEYOR_REPO_BRANCH"

cd $APPVEYOR_BUILD_FOLDER

# Do not perform end-of-line conversion
git config --global core.autocrlf false
git clone https://github.com/ocaml/ocaml.git --branch $OCAMLBRANCH --depth 1 --recurse-submodules ocaml

cd ocaml

MAKEOCAML=make
CONFIG_DIR=config
HEADER_DIR=byterun/caml

case $OCAMLBRANCH in
    4.03|4.04)
        MAKEOCAML="make -f Makefile.nt"
        HEADER_DIR=config
        ;;
    4.05)
        HEADER_DIR=config
        ;;
esac

if [ ! -f $OCAMLROOT/STAMP ] || [ "$(git rev-parse HEAD)" != "$(cat $OCAMLROOT/STAMP)" ]; then
    cp config/m-nt.h $HEADER_DIR/m.h
    cp config/s-nt.h $HEADER_DIR/s.h

    sed -e "s|PREFIX=.*|PREFIX=$OCAMLROOT|" \
        -e "s|OTHERLIBRARIES=.*|OTHERLIBRARIES=|" \
        -e "s|WITH_DEBUGGER=.*|WITH_DEBUGGER=|" \
        -e "s|WITH_OCAMLDOC=.*|WITH_OCAMLDOC=|" \
        config/Makefile.msvc64 > $CONFIG_DIR/Makefile
    #run "Content of config/Makefile" cat $CONFIG_DIR/Makefile

    run "make world.opt" $MAKEOCAML flexdll world.opt
    run "make install" $MAKEOCAML install

    git rev-parse HEAD > $OCAMLROOT/STAMP
fi

export CAML_LD_LIBRARY_PATH=$OCAMLROOT/lib/stublibs

cd $APPVEYOR_BUILD_FOLDER

run "make flexlink.exe" make MSVC_DETECT=0 flexlink.exe

CHAINS="mingw mingw64 cygwin cygwin64 msvc msvc64"

for CHAIN in $CHAINS; do
    run "make build_$CHAIN" make build_$CHAIN
done

for CHAIN in $CHAINS; do
    run "make demo_$CHAIN" make demo_$CHAIN
done
