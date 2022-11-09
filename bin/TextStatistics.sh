#!/bin/bash

cd /home/oliveirmic/Projetos-dev/GitHub/Text_statistics/scala/ || exit

sbt "runMain statisticsText.StatisticsText $1 $2"
ret="$?"

cd - || exit

exit $ret
