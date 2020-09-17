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

function configure_ocaml {
    if [[ -z $HEADER_DIR ]] ; then
      # Unfortunately, configure fails to set-up bootstrapping if flexlink is
      # in PATH
      sed -i -e 's/@iflexdir@/-I"$(ROOTDIR)\/flexdll"/' Makefile.config.in

      ./configure --build=i686-pc-cygwin --host=$OCAML_TARGET \
                    --prefix=$OCAMLROOT \
                    --disable-debugger \
                    --disable-ocamldoc \
                    --disable-systhreads \
                    --disable-str-lib \
                    --disable-unix-lib \
                    --disable-bigarray-lib \
                    $GRAPHICS_DISABLE \
                    --disable-debug-runtime
    else
      # "Classic" configuration
      cp config/m-nt.h $HEADER_DIR/m.h
      cp config/s-nt.h $HEADER_DIR/s.h

      sed -e "s|PREFIX=.*|PREFIX=$OCAMLROOT|" \
          -e 's/\(OTHERLIBRARIES\|WITH_DEBUGGER\|WITH_OCAMLDOC\|DEBUGGER\|EXTRALIBS\|WITH_OCAMLBUILD\|CAMLP4\)=.*/\1=/' \
          config/Makefile.$OCAML_PORT > $CONFIG_DIR/Makefile
      #run "Content of config/Makefile" cat $CONFIG_DIR/Makefile
    fi
}

case "$OCAML_PORT" in
  msvc) OCAML_TARGET=i686-pc-windows; OCAML_SYSTEM=win32;;
  msvc64) OCAML_TARGET=x86_64-pc-windows; OCAML_SYSTEM=win64;;
  mingw) OCAML_TARGET=i686-w64-mingw32; OCAML_SYSTEM=mingw;;
  mingw64) OCAML_TARGET=x86_64-w64-mingw32; OCAML_SYSTEM=mingw64;;
  *) echo "Unrecognised OCAML_PORT: $OCAML_PORT"; exit 1;;
esac

echo ** OCAMLROOT=$OCAMLROOT

#echo "APPVEYOR_PULL_REQUEST_NUMBER = $APPVEYOR_PULL_REQUEST_NUMBER"
#echo "APPVEYOR_PULL_REQUEST_TITLE = $APPVEYOR_PULL_REQUEST_TITLE"
#echo "APPVEYOR_PULL_REQUEST_HEAD_REPO_NAME = $APPVEYOR_PULL_REQUEST_HEAD_REPO_NAME"
#echo "APPVEYOR_PULL_REQUEST_HEAD_REPO_BRANCH = $APPVEYOR_PULL_REQUEST_HEAD_REPO_BRANCH"
#echo "APPVEYOR_PULL_REQUEST_HEAD_COMMIT = $APPVEYOR_PULL_REQUEST_HEAD_COMMIT"
#echo "APPVEYOR_REPO_PROVIDER = $APPVEYOR_REPO_PROVIDER"
#echo "APPVEYOR_REPO_NAME = $APPVEYOR_REPO_NAME"
#echo "APPVEYOR_REPO_BRANCH = $APPVEYOR_REPO_BRANCH"

APPVEYOR_BUILD_FOLDER=$(echo "$APPVEYOR_BUILD_FOLDER" | cygpath -f -)
cd $APPVEYOR_BUILD_FOLDER

git tag merge

# Do not perform end-of-line conversion
git config --global core.autocrlf false
git clone https://github.com/ocaml/ocaml.git --branch $OCAMLBRANCH${OCAMLREV:+.}$OCAMLREV --depth 1 --recurse-submodules ocaml

cd ocaml

MAKEOCAML=make
CONFIG_DIR=config
GRAPHICS_DISABLE=
HEADER_DIR=

case $OCAMLBRANCH in
    3.11|3.12|4.00|4.01|4.02|4.03|4.04)
        MAKEOCAML="make -f Makefile.nt"
        HEADER_DIR=config
        ;;
    4.05)
        HEADER_DIR=config
        ;;
    4.06|4.07)
        HEADER_DIR=byterun/caml
        ;;
    4.08)
        GRAPHICS_DISABLE=--disable-graph-lib
esac

if [ $OCAMLBRANCH = "4.03" ] ; then
  sed -i -e "s/:=.*/:=/" config/Makefile.msvc64
fi

if [ ! -f $OCAMLROOT/STAMP ] || [ "$(git rev-parse HEAD)" != "$(cat $OCAMLROOT/STAMP)" ] || [ "$(sed -ne 's/ *SYSTEM *= *//p' "$(ocamlc -where | tr -d '\r')/Makefile.config" | tr -d '\r')" != "$OCAML_SYSTEM" ] ; then
    if [ ! -f $OCAMLROOT/STAMP ] ; then
        echo "Building the compiler"
    else
        echo "Rebuilding the compiler"
    fi

    configure_ocaml

    if [ ${OCAMLBRANCH/./} -lt 403 ] ; then
      rm -rf $OCAMLROOT
      mkdir -p /cygdrive/c/flexdll
      mv "$APPVEYOR_BUILD_FOLDER/flexdll.zip" /cygdrive/c/flexdll
      pushd /cygdrive/c/flexdll
      unzip -q flexdll.zip
      popd
      mkdir -p ../flexdll-src
      pushd ../flexdll-src
      tar -xzf "$APPVEYOR_BUILD_FOLDER/flexdll.tar.gz"
      cd flexdll-*
      make CHAINS=msvc64 MSVC_DETECT=0 support
      mkdir -p $OCAMLROOT/bin
      cp -v *.obj /cygdrive/c/flexdll/
      cp -v *.obj $OCAMLROOT/bin/
      cp /cygdrive/c/flexdll/default_amd64.manifest $OCAMLROOT/bin/
      popd
      # GPR#405
      sed -i -e '/kernel/a#pragma comment(lib , "ucrt.lib")\n#pragma comment(lib , "vcruntime.lib")' stdlib/headernt.c
      if [ -e byterun/caml/misc.h ] ; then
        sed -i -e 's/def _WIN32/ndef _UCRT/' byterun/caml/misc.h
      fi
      # MPR#6319
      sed -i -e '/chmod -w/d' Makefile.nt
      # Unavailable configuration
      sed -i -e '/^opt.opt:/,+1s/ocamlbuild.native\|camlp4opt\|ocamldoc.opt//g' \
             -e '/[^(]cd ocamldoc/d' \
             -e '/partial-install/d' \
             -e '/^opt:/s/other.*//' \
             -e '/^all:/,+1s/ocamldoc.byte\|ocamlbuild.byte\|camlp4out//g' Makefile.nt
      run "make world.opt" $MAKEOCAML world opt opt.opt
      cp /cygdrive/c/flexdll/flexlink.exe $OCAMLROOT/bin/
    else
      run "make world.opt" $MAKEOCAML flexdll world.opt
    fi
    run "make install" $MAKEOCAML install
    git clean -dfx > /dev/null
    if [ -e flexdll/Makefile ] ; then
        cd flexdll
        git clean -dfx > /dev/null
        cd ..
    fi

    git rev-parse HEAD > $OCAMLROOT/STAMP
fi

export CAML_LD_LIBRARY_PATH=$OCAMLROOT/lib/stublibs

pushd $APPVEYOR_BUILD_FOLDER

run "make flexlink.exe" make MSVC_DETECT=0 flexlink.exe

CHAINS="mingw mingw64 cygwin cygwin64 msvc msvc64"

for CHAIN in $CHAINS; do
    run "make build_$CHAIN" make build_$CHAIN
done

for CHAIN in $CHAINS; do
    # Decision on operation of Cygwin64 not yet taken
    if [ "$CHAIN" = "cygwin64" ] ; then continue; fi
    run "make demo_$CHAIN" make demo_$CHAIN
done

popd

if [ "$SKIP_OCAML_TEST" = no ] ; then
    configure_ocaml

    cd flexdll
    git remote add local $(echo "$APPVEYOR_BUILD_FOLDER"| cygpath -f -) -f --tags
    run "git checkout $APPVEYOR_REPO_COMMIT" git checkout merge
    cd ..

    run "make world" $MAKEOCAML flexdll world
fi

if [ "$ARTEFACTS" = 'yes' ] ; then
    pushd "$APPVEYOR_BUILD_FOLDER" &> /dev/null

    make package_bin installer
    SUFFIX="$(git describe)"
    VERSION="$(sed -ne 's/^VERSION *= *//p' Makefile)"
    if [ "$SUFFIX" != "$VERSION" ] ; then
      mv "flexdll-bin-$VERSION.zip" "flexdll-bin-$SUFFIX.zip"
      mv "flexdll-$VERSION-setup.exe" "flexdll-$SUFFIX-setup.exe"
    fi
    appveyor PushArtifact "flexdll-$SUFFIX-setup.exe"
    appveyor PushArtifact "flexdll-bin-$SUFFIX.zip"

    popd &> /dev/null
fi
