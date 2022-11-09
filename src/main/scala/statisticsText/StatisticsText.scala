package statisticsText

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.io.Source
import scala.util.{Failure, Success, Try}

case class Statistics(inputFile: String,
                      dateCreation: String,
                      dateModification: String,
                      numberLines: String,
                      //                      numberWords: String,
                      //                      numberSpaces: String
                     )

class StatisticsText {

  def mappingStatistics(path_in: Path): Statistics = {

    //val dirFile: String = String(Files.readAllBytes(path_in))
    val dateCreation = LocalDate.now.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
    val dateModification = LocalDate.now.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
    val bufferedSource = Source.fromFile(File(path_in.toString))
    val numberLines = bufferedSource.getLines().size

    Statistics(path_in.toString, dateCreation, dateModification, numberLines.toString)
  }

  def defineOutput(path_in: Path, path_out: Path): Try[Unit] = {
    Try {
      val createReportFile: Boolean = path_out != null
      val statistics: Statistics = mappingStatistics(path_in)

      if createReportFile then reportStatistics(statistics, path_out)
      else standardOutput(statistics)
    }
  }

  def reportStatistics(statistics: Statistics, path_out: Path): Try[Unit] = {
    Try {

      val statisticsFile = File(s"$path_out/statistics.txt")
      if (!statisticsFile.exists) statisticsFile.createNewFile

      val fileWriter = FileWriter(statisticsFile, true)
      val bufferedWriter = BufferedWriter(fileWriter)

      bufferedWriter.write(s"\nARQUIVO: ${statistics.inputFile}\n")
      bufferedWriter.write(s"DATA DE CRIAÇÃO: ${statistics.dateCreation}\n")
      bufferedWriter.write(s"DATA DE MODIFICAÇÃO: ${statistics.dateModification}\n")
      bufferedWriter.write(s"NÚMERO DE LINHAS: ${statistics.numberLines}\n")
      bufferedWriter.close()
      fileWriter.close()
    }
  }

  def standardOutput(statistics: Statistics): Unit = {
    println("----------------------------------------------")
    println(s"ARQUIVO: ${statistics.inputFile}")
    println(s"DATA DE CRIAÇÃO: ${statistics.dateCreation}")
    println(s"DATA DE MODIFICAÇÃO: ${statistics.dateModification}")
    println(s"NÚMERO DE LINHAS: ${statistics.numberLines}")
    println("----------------------------------------------")
  }
}

object StatisticsText {

  def main(args: Array[String]): Unit = {

    val parser = argparse.default.ArgumentParser(description = "Programa para Estatísticas de texto")

    val path_in = parser.requiredParam[Path]("--in", help = "Diretório de entrada do arquivo .TXT")
    val path_out = parser.param[Path]("--out", default = null, help = "Se informado um diretório, será gerado" +
      " um aquivo .TXT com as Estatísticas de texto")

    parser.parseOrExit(args)

    (new StatisticsText).defineOutput(path_in.value, path_out.value) match
      case Success(_) if path_out.value == null =>
        println("\nStatistics generated successfully!\n")
        System.exit(0)
      case Success(_) =>
        println(s"\nStats file successfully generated at: ${path_out.value}/statistics.txt\n")
        System.exit(0)
      case Failure(exception) =>
        println(s"\nFile not generated: ${exception.toString}\n")
        System.exit(1)
  }
}