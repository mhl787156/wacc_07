#!/bin/bash
EXECUTABLE=$(echo "$BASH_ARGV" | sed 's/\(.*\)\..*/\1/')
echo $EXECUTABLE
EXE=`basename $EXECUTABLE`
END=".s"
echo $EXE$END
SFLAG=0
for var in "$@"
do
  if [ $var == "-s" ]
    then
      SFLAG=1
  fi
done
if [ $SFLAG -eq 0 ]
  then
    echo
    echo "MAKING!..........................."
    echo
    make
    if [ $? -ne 0 ]
      then 
        echo "make failed."
        exit
    fi
    echo
    echo "Running This Wacc File............"
    echo
    cat $BASH_ARGV
    echo
    echo "Compiling using our compiler......"
    echo
    ./compile $BASH_ARGV
    echo
    echo "FINISHED COMPILATION.............."
fi
echo "USING THE EMULATOR................"
echo
arm-linux-gnueabi-gcc -o $EXE -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $EXE$END
if [ $? -ne 0 ]
  then
    echo "assembly failed."
    exit
fi
echo
echo "RUNNING THE ASSEMBLY.............."
echo
qemu-arm -L /usr/arm-linux-gnueabi/ $EXE
rm $EXE
