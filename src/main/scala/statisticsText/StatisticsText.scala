package statisticsText

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.file.{Files, Path, Paths}
import java.text.SimpleDateFormat
import java.time.{Instant, LocalDate}
import java.time.format.DateTimeFormatter
import java.util.Date
import scala.collection.MapView
import scala.collection.immutable.TreeMap
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.util.{Failure, Sorting, Success, Try}

case class Statistics(inputFile: String,
                      dateCreation: String,
                      dateModification: String,
                      numberLines: String,
                      numberWords: String,
                      numberSpaces: String,
                      lineGreeting: String,
                      wordsGreeting: String,
                      charStatistics: String,
                      wordsRepetition: String)

class StatisticsText {

  private def mappingStatistics(path_in: Path): Statistics = {


    val dirFile: String = String(Files.readAllBytes(path_in))
    val dateCreation: String = getDateModification(path_in, "Creation")
    val dateModification: String = getDateModification(path_in, "Modification")
    val text: Seq[String] = dirFile.split("\n").toSeq

    val numberLines: String = text.size.toString
    val numberWords: String = text.map(f => f.split("\\s+").length).sum.toString
    val numberSpaces: String = text.map(f => f.split("[ \t]+").length).sum.toString

    val linesLengthList: Seq[Int] = text.map(x => x.split("\\s").mkString(" ").length)
    val linesGreeting: String = getStatisticsLines(linesLengthList, "linha")

    val wordsLengthList: Seq[Int] = dirFile.split("\\s+").map(f => f.length).toSeq
    val wordsGreeting: String = getStatisticsLines(wordsLengthList, "palavra")

    val charStatistics: String = charCodeRepetition(dirFile.split(" ").flatten)
    val wordsRepeated: String = wordsRepetition(dirFile.split("\\s+"))

    Statistics(path_in.toString, dateCreation, dateModification, numberLines, numberWords, numberSpaces, linesGreeting,
      wordsGreeting, charStatistics, wordsRepeated)
  }

  private def getDateModification(path: Path, typeDate: String): String = {

    val formatDateModification = new SimpleDateFormat("dd-MM-yyyy")
    typeDate match
      case "Creation" => formatDateModification.format(new Date(Files.readAttributes(path, classOf[BasicFileAttributes]).creationTime().toMillis)).replace("-", "/")
      case "Modification" => formatDateModification.format(new Date(Files.getLastModifiedTime(path).toMillis)).replace("-", "/")
      case _ => ""
  }

  private def defineOutput(path_in: Path, path_out: Path): Try[Unit] = {
    Try {
      val createReportFile: Boolean = path_out != null
      val statistics: Statistics = mappingStatistics(path_in)

      if createReportFile then reportStatistics(statistics, path_out)
      else standardOutput(statistics)
    }
  }

  private def charCodeRepetition(str: Seq[Char]): String = {

    val strList: Map[Char, (Int, Seq[Char])] = str.groupBy(identity).map(f => (f._1, (f._1.intValue, f._2)))
    val contatWordsQtd: (String, (Char, (Int, Seq[Char]))) => String = (a, b) => a + s"[${b._1}][${b._2._1}]: ${b._2._2.length}\n"
    strList.foldLeft("")(contatWordsQtd)
  }

  private def wordsRepetition(str: Seq[String]): String = {

    val strList: Map[String, Int] = str.groupBy(identity).map(f => (f._1, f._2.length))
    val contatWordsQtd: (String, (String, Int)) => String = (a, b) => a + s"[${b._1}]:${b._2}\n"
    strList.foldLeft("")(contatWordsQtd)
  }

  private def getStatisticsLines(linesLengthList: Seq[Int], title: String): String = {

    val meuMap: Map[Int, Int] = linesLengthList.foldLeft(TreeMap[Int, Int]()) {
      case (tm, len) => tm + (len -> (tm.getOrElse(len, 0) + 1))
    }
    //val listLines: Map[Int, Int] = linesLengthList.groupBy(identity).map(f => (f._1, f._2.length))

    val concatLength: (String, (Int, Int)) => String = (a, b) => a + s"comprimento de $title [${b._1}]: ${b._2}\n"
    val linesConcatLength: String = meuMap.foldLeft("")(concatLength)

    val concatLengthAverage: (Int, (Int, Int)) => Int = (a, b) => a + b._1
    val linesConcatLengthAverage: Int = meuMap.foldLeft(0)(concatLengthAverage)

    val statisticsLines: String = linesConcatLength.concat(s"comprimento médio das ${title}s: ${linesConcatLengthAverage/meuMap.size}\n")

    statisticsLines
  }

  private def reportStatistics(statistics: Statistics, path_out: Path): Try[Unit] = {
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
      bufferedWriter.write(s"NÚMERO DE ESPAÇOS: ${statistics.numberSpaces}\n\n")
      bufferedWriter.write(s"${statistics.lineGreeting}\n")
      bufferedWriter.write(s"${statistics.wordsGreeting}\n")
      bufferedWriter.write(s"${statistics.charStatistics}\n")
      bufferedWriter.write(s"${statistics.wordsRepetition}\n")

      bufferedWriter.close()
      fileWriter.close()
    }
  }

  private def standardOutput(statistics: Statistics): Unit = {
    println("----------------Statistics Text----------------")
    println(s"ARQUIVO: ${statistics.inputFile}")
    println(s"DATA DE CRIAÇÃO: ${statistics.dateCreation}")
    println(s"DATA DE MODIFICAÇÃO: ${statistics.dateModification}")
    println(s"NÚMERO DE LINHAS: ${statistics.numberLines}")
    println(s"NÚMERO DE PALAVRAS: ${statistics.numberWords}")
    println(s"NÚMERO DE ESPAÇOS: ${statistics.numberSpaces}\n")
    println(statistics.lineGreeting)
    println(statistics.wordsGreeting)
    println(statistics.charStatistics)
    println(statistics.wordsRepetition)
    println("-----------------------------------------------")
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
        } else println(s"\nStats file successfully generated at: ${path_out.value}\n")
        System.exit(0)
      case Failure(exception) =>
        println(s"\nFile not generated: ${exception.toString}\n")
        System.exit(1)
  }
}