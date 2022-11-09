package statisticsText

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path}
import java.time.LocalDate
import java.time.format.{DateTimeFormatter, FormatStyle}
import scala.util.{Try, Using}

case class Statistics(inputFile: String,
                      outputFile: String,
                      dateCreation: String,
                      dateModification: String,
                      numberLines: String,
                      numberWords: String,
                      numberSpaces: String)


class StatisticsText {

  def staticsText(path_in: Path, path_out: Path): Try[Unit] = {
    Try {

      val dirFile = new String(Files.readAllBytes(path_in))

      val teste = if path_out == null then "" else path_out

      val createReport: Boolean = path_out != null
      val date = LocalDate.now.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
      val statics = Statistics(path_in.toString, teste.toString, date, date, "3", "4", "5")

      if (createReport) {
        report(statics)
      } else {
        println("----------------------------------------")
        println(statics.dateCreation)
        println("----------------------------------------")
        println(dirFile)
        println("----------------------------------------")
      }
    }
  }

  def report(statistics: Statistics): Unit = {

    val pathFileLog = File(s"${statistics.outputFile}/logs.txt")
    if (!pathFileLog.exists) pathFileLog.createNewFile

    val fileWriter = FileWriter(pathFileLog, true)
    val bufferedWriter = BufferedWriter(fileWriter)

    bufferedWriter.write(statistics.dateCreation)
    bufferedWriter.newLine()
    bufferedWriter.close()
    fileWriter.close()
  }
}


object StatisticsText {

  def main(args: Array[String]): Unit = {

    val parser = argparse.default.ArgumentParser(description = "Programa para Estatísticas de texto")

    val path_in = parser.requiredParam[java.nio.file.Path]("--in", help = "Diretório de entrada do arquivo .TXT")
    val path_out = parser.param[java.nio.file.Path]("--out", default = null, help = "Se informado um diretório," +
      " será gerado um aquivo .TXT com as Estatísticas de texto")

    parser.parseOrExit(args)

    (new StatisticsText).staticsText(path_in.value, path_out.value)
  }
}