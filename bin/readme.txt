PROCEDIMENTO PARA EXECUÇÃO

1 - Clone o projeto em um diretorio:
git clone https://github.com/Michael-Junior/Text_statistics.git

2 - Configure o arquivo bin/TextStatistics.sh, exemplo:
#!/bin/bash

cd /home/javaapps/sbt-projects/Text_statistics/scala/ || exit

sbt "runMain statisticsText.StatisticsText $1 $2"
ret="$?"

cd - || exit

exit $ret

3 - Execute os comandos para execução indicando um arquivo de entrada e um caminho de saída em 'Bin', exemplo:
./TextStatistics.sh --in=/PATH/FILE.TXT --out=/PATH/RELATÓRIO.TXT
