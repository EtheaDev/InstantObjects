#!/bin/sh
[ $# -gt 0 ] &&
  InstantVersionResFile=$1 ||
  InstantVersionResFile="InstantVersion.res"
[ -f ${InstantVersionResFile} ] ||
  { echo "Resource file ${InstantVersionResFile} was not found."; exit 1; }
echo
echo "This script will replicate ${InstantVersionResFile},"
echo "creating or overwriting all projects' resource files."
echo
echo "Press Enter to continue, Ctrl+C to cancel."
read

find . -iname "*.dpk" -type f | while read DpkFile
do
  cp -f "${InstantVersionResFile}" "${DpkFile/.dpk/.res}"
done
echo "Done."
