#!/bin/sh
#
#   InstantObjects
#   Release Tools
#

# ##### BEGIN LICENSE BLOCK #####
# Version: MPL 1.1
#
# The contents of this file are subject to the Mozilla Public License Version
# 1.1 (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
# http://www.mozilla.org/MPL/
#
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# for the specific language governing rights and limitations under the
# License.
#
# The Original Code is: InstantObjects Release Tools
#
# The Initial Developer of the Original Code is: Joao Morais
#
# Portions created by the Initial Developer are Copyright (C) 2006
# the Initial Developer. All Rights Reserved.
#
# Contributor(s):
#
#
# ##### END LICENSE BLOCK #####

[ $# -gt 0 ] &&
  InstantVersionResFile=$1 ||
  InstantVersionResFile="InstantVersion.res"
[ -f ${InstantVersionResFile} ] ||
  { echo "Resource file ${InstantVersionResFile} was not found."; exit 1; }
echo
echo "This script will replicate ${InstantVersionResFile},"
echo "creating or overwriting all packages' resource files."
echo
echo "Press Enter to continue, Ctrl+C to cancel."
read

find .. -iname "*.dpk" -type f | while read DpkFile
do
  cp -f "${InstantVersionResFile}" "${DpkFile/.dpk/.res}"
done
echo "Done."
