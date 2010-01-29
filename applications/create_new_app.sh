#!/bin/sh

if test "$#" != "1"; then
	echo "Usage: sh ./create_new_app.sh MYAPPLICATIONNAME"
	exit 1
fi

mkdir "$1"
mkdir "$1/client"
mkdir "$1/server"
echo "* How to use" >> "$1/README.txt"
echo "- Build application: erl -make" >> "$1/README.txt"
echo "- Start application with start script" >> "$1/README.txt"
echo "* Development" >> "$1/README.txt"

echo "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" >> "$1/$1.xml"
echo "<omsapp>" >> "$1/$1.xml"
echo "<display-name>OMS $1 Application</display-name>" >> "$1/$1.xml"
echo "<short-name>$1</short-name>"  >> "$1/$1.xml"
echo "<apppath>server/ebin</apppath>" >> "$1/$1.xml"
echo "<adapter obj=\"oms_netconnection\" adapter=\"$1_netconnection\" />" >> "$1/$1.xml"
echo "<!-- adapter obj=\"oms_netstream\" adapter=\"$1_mynetstream\" / -->" >> "$1/$1.xml"
echo "</omsapp>" >> "$1/$1.xml"

mkdir "$1/server/ebin"
mkdir "$1/server/src"

echo "{'src/*', [{outdir, \"ebin\"}, {i, \"../../../include/\"},{debug_info}]}." >> "$1/server/Emakefile"


touch "$1/server/src/$1_netconnection.erl"
touch "$1/server/src/$1_netstream.erl"

echo "-module($1_netconnection)." >> $1/server/src/$1_netconnection.erl

echo "-export([])." >> $1/server/src/$1_netconnection.erl
echo "-include(\"oms_gendef.hrl\")." >> $1/server/src/$1_netconnection.erl

echo "-module($1_netstream)." >> $1/server/src/$1_netstream.erl

echo "-export([])." >> $1/server/src/$1_netstream.erl
echo "-include(\"oms_gendef.hrl\")." >> $1/server/src/$1_netstream.erl
