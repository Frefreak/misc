#!/usr/bin/bash

export QUARTUS_ROOTDIR=`echo $PATH | sed 's/:/\n/g' | grep quartus`/..
NIOS2_ROOTDIR=$QUARTUS_ROOTDIR/../`ls $QUARTUS_ROOTDIR/.. | grep nios`
export PATH=$PATH:$NIOS2_ROOTDIR/bin:$NIOS2_ROOTDIR/bin/gnu/H-x86_64-pc-linux-gnu/bin

case $1 in
        1)      # $2: sof file
                nios2-configure-sof $2
                ;;
        2)      # $2: sof file
                sof_flash=`mktemp`
                sof2flash --input=$2 --output=$sof_flash --epcs --verbose
                nios2-flash-programmer $sof_flash --base=0x0 --epcs --program --verbose -g
                rm $sof2flash
                ;;
        3)      # $2: sof file
                # $3: elf file
                # $4: sysid
                # $5: sysid_base
                # $6: timestamp
                nios2-configure-sof $2
                nios2-download --sidp=$5 --id=$4 --timestamp=$6 $3 -g
                ;;
        4)      # $2: sof file
                # $3: elf file
                # $4: sysid
                # $5: sysid_base
                # $6: timestamp
                sof_flash=`mktemp`
                elf_flash=`mktemp`
                sof2flash --input=$2 --output=$sof_flash --epcs --verbose
                elf2flash --input=$3 --output=$elf_flash --epcs --after=$sof_flash --verbose
                # flash elf without flash sof???
                nios2-configure-sof $2
                nios2-flash-programmer $sof_flash --base=0x0 --epcs --sidp=$5 --id=$4 --timestamp=$6 --program --verbose
                nios2-configure-sof $2
                nios2-flash-programmer $elf_flash --base=0x0 --epcs --sidp=$5 --id=$4 --timestamp=$6 --program --verbose
                ;;
        *)
                # since this script is called by a QT program, we may never
                # get to this case.
                echo "Unknown option"
                ;;
esac
