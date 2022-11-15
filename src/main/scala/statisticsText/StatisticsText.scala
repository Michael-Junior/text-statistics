package statisticsText

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.collection.MapView
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

case class Statistics(inputFile: String,
                      dateCreation: String,
                      dateModification: String,
                      numberLines: String,
                      numberWords: String,
                      numberSpaces: String,
                      lineGreeting: String,
                      wordsGreeting: String)

class StatisticsText {

  def mappingStatistics(path_in: Path): Statistics = {

    val dirFile: String = String(Files.readAllBytes(path_in))
    val dateCreation: String = LocalDate.now.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
    val dateModification: String = LocalDate.now.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
    val numberLines: String = dirFile.lines().count().toString
    val numberWords: String = dirFile.split("\\S+").length.toString
    val numberSpaces: String = dirFile.split("\\b(\\s)\\b").length.toString //validar expressão

    val bufferedSource: BufferedSource = Source.fromFile(File(path_in.toString))
    val linesLengthList: Seq[Int] = bufferedSource.getLines().toSeq.map(x => x.split("\\W").mkString(" ").length)
    val linesGreeting: String = statisticsLines(linesLengthList)

    val bufferedSourceW: BufferedSource = Source.fromFile(File(path_in.toString))
    val wordsLengthList: Seq[String] = bufferedSourceW.getLines().toSeq.map(x => x.split("\\s").mkString(" "))
    val words: Seq[Seq[String]] = wordsLengthList.map(f => f.split(" ").map(_.trim))
    val maisUm: Seq[Seq[Int]] = words.map(f => f.map(f => f.length))

    val wordsGreeting: String = statisticsWords(maisUm)
    val wordsGreetingWords: String = statisticsWords(maisUm)

    bufferedSource.close()
    bufferedSourceW.close()

    Statistics(path_in.toString, dateCreation, dateModification, numberLines, numberWords, numberSpaces, linesGreeting, wordsGreeting)
  }

  def statisticsWords(linesLengthList: Seq[Seq[Int]]): String = {

    var str = ""

    for (i <- linesLengthList) {
      val listLines: MapView[Int, Int] = i.groupBy(identity).mapValues(f = _.size)

      val concatLength: (String, (Int, Int)) => String = (a, b) => a + s"comprimento de linha [${b._1}]:${b._2}\n"
      val linesConcatLength: String = listLines.foldLeft("")(concatLength)

      val concatLengthAverage: (Int, (Int, Int)) => Int = (a, b) => a + b._1 / listLines.size
      val linesConcatLengthAverage: Int = listLines.foldLeft(0)(concatLengthAverage)

      val statisticsLines: String = linesConcatLength.concat(s"comprimento médio das linhas: $linesConcatLengthAverage")

      str = statisticsLines
    }
    str
  }


  def statisticsLines(linesLengthList: Seq[Int]): String = {

    val listLines: MapView[Int, Int] = linesLengthList.groupBy(identity).mapValues(f = _.size)

    val concatLength: (String, (Int, Int)) => String = (a, b) => a + s"comprimento de linha [${b._1}]:${b._2}\n"
    val linesConcatLength: String = listLines.foldLeft("")(concatLength)

    val concatLengthAverage: (Int, (Int, Int)) => Int = (a, b) => a + b._1 / listLines.size
    val linesConcatLengthAverage: Int = listLines.foldLeft(0)(concatLengthAverage)

    val statisticsLines: String = linesConcatLength.concat(s"comprimento médio das linhas: $linesConcatLengthAverage")

    statisticsLines
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

      val statisticsFile = File(path_out.toString)
      if (!statisticsFile.exists) statisticsFile.createNewFile

      val fileWriter = FileWriter(statisticsFile, false)
      val bufferedWriter = BufferedWriter(fileWriter)

      bufferedWriter.write("---------Statistics Text---------\n")
      bufferedWriter.write(s"ARQUIVO: ${statistics.inputFile}\n")
      bufferedWriter.write(s"DATA DE CRIAÇÃO: ${statistics.dateCreation}\n")
      bufferedWriter.write(s"DATA DE MODIFICAÇÃO: ${statistics.dateModification}\n")
      bufferedWriter.write(s"NÚMERO DE LINHAS: ${statistics.numberLines}\n")
      bufferedWriter.write(s"NÚMERO DE PALAVRAS: ${statistics.numberWords}\n")
      bufferedWriter.write(s"NÚMERO DE ESPAÇOS: ${statistics.numberSpaces}\n")
      bufferedWriter.write(s"\n${statistics.lineGreeting}")
      bufferedWriter.write(s"\n${statistics.wordsGreeting}")

      bufferedWriter.close()
      fileWriter.close()
    }
  }

  def standardOutput(statistics: Statistics): Unit = {
    println("---------Statistics Text---------")
    println(s"ARQUIVO: ${statistics.inputFile}")
    println(s"DATA DE CRIAÇÃO: ${statistics.dateCreation}")
    println(s"DATA DE MODIFICAÇÃO: ${statistics.dateModification}")
    println(s"NÚMERO DE LINHAS: ${statistics.numberLines}")
    println(s"NÚMERO DE PALAVRAS: ${statistics.numberWords}")
    println(s"NÚMERO DE ESPAÇOS: ${statistics.numberSpaces}\n")
    println(statistics.lineGreeting)
    println(s"\n${statistics.wordsGreeting}")
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
      case Success(_) =>
        if (path_out.value == null) {
          println("Statistics generated successfully!\n")
          System.exit(0)
        }
        else println(s"\nStats file successfully generated at: ${path_out.value}\n")
        System.exit(0)
      case Failure(exception) =>
        println(s"\nFile not generated: ${exception.toString}\n")
        System.exit(1)
  }
}