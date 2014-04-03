#!/bin/bash

rm -rf gen/

mkdir -p gen/hsc/Bindings/OpenNI2
(
  cd gen/hsc/Bindings/OpenNI2
  ~/tmp/c2hsc/c2hsc/dist/build/c2hsc/c2hsc --prefix=Bindings.OpenNI2 \
    ~/tmp/openni/OpenNI2/Include/OniCAPI.h \
    ~/tmp/openni/OpenNI2/Include/OniCEnums.h \
    ~/tmp/openni/OpenNI2/Include/OniCProperties.h \
    ~/tmp/openni/OpenNI2/Include/OniCTypes.h \
    ~/tmp/openni/OpenNI2/Include/OniPlatform.h \
    ~/tmp/openni/OpenNI2/Include/OniVersion.h
)

# (
#   cd gen/
#   cp -r hsc/ hs/
#   cd hs/
#   find * -name '*.hsc' | xargs hsc2hs --verbose # -I $HOME/tmp/c2hsc/bindings-dsl
# )

# (
#   cd gen/
#   mkdir hs/
#   cd hsc/
#   for f in `find * -name '*.hsc'`; do
#     # echo hsc2hs $f -o "../hs/$f"
#     hsc2hs $f -o "../hs/$f"
#   done
# )

# hsc2hs -I $HOME/tmp/c2hsc/bindings-dsl *.hsc
