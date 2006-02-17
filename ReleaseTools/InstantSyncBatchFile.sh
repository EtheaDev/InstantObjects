#!/bin/sh
InstantVersionResFile="InstantVersion.res"
InstantScriptFile="InstantReplicateResource.bat"

echo
echo "This script will (re)create ${InstantScriptFile}."
echo "Press Enter to continue, Ctrl+C to cancel."
read

echo -n -e \
"@echo off\r\n"\
"echo.\r\n"\
"echo This script will replicate ${InstantVersionResFile},\r\n"\
"echo creating or overwriting all packages' resource files.\r\n"\
"echo.\r\n"\
"echo Press Enter to continue, Ctrl+C to cancel.\r\n"\
"echo.\r\n"\
"pause > nul\r\n"\
"echo Copying...\r\n"\
"echo.\r\n" > ${InstantScriptFile}

find .. -name "*.dpk" -type f | sed 's/\//\\\\/g' | while read DpkFile
do
  echo -e "copy \"${InstantVersionResFile}\" \"${DpkFile/.dpk/.res}\" > nul\r"
  echo -e "echo ${DpkFile/.dpk/.res}\r"
done >> ${InstantScriptFile}
echo -n -e \
"echo.\r\n"\
"echo Done.\r\n" >> ${InstantScriptFile}
echo "Done."
