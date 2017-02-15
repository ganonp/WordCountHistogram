package wordcount

import java.io.File
import scala.collection.mutable
import scala.io.Source

/**
  * Created by Ganon on 2/11/2017.
  */
object WordCount {
  var zip = Unzip;
  val stemmer = new Stemmer();
  var wordCounts = new mutable.HashMap[String, Int]();
  var maxWordCounts = new mutable.HashMap[String,Int]()
  var stemToFull = new mutable.HashMap[String,mutable.HashSet[String]]()
  var totalCount = 0;
  var input = "";
  var output = "";


  def main(args: Array[String]) {
    getInputOutputFolders()
    unzipFiles(input)
    countWords(output)
    getNLargestWordCounts()
  }

  def getInputOutputFolders():Unit = {
    input = readLine("What's the input directory? ")
    output = readLine("What's the output directory? ")
  }

  def unzipFiles(dir: String) = {
    val d = new File(dir)
    for(file <- d.listFiles.toList){
      identifyZips(file.toString)
    }
  }

  def identifyZips(file: String):Unit = {
    if(file.matches(".*zip")) {
      zip.INPUT_ZIP_FILE=file
      zip.OUTPUT_FOLDER= output
      zip.unZipIt(file,output)
    }else{
      unzipFiles(file)
    }
  }

  def countWords(txtFile: String):Unit = {
    val d = new File(txtFile)
    if (d.exists && d.isDirectory) {
      for(file <- d.listFiles.toList){
        if(file.isFile){
          parseFile(file.toString)
        }else{
          countWords(file.toString)
        }
      }
    }
  }

  def parseFile(filename: String): Unit = {
    for (line <- Source.fromFile(filename).getLines()) {
      val words = line.replaceAll("[^a-zA-Z ]", "").toLowerCase().split("\\s+");;
      for (wordtemp <- words){
        val word = stemmer.stemIt(wordtemp);
        if(!stemToFull.contains(word)){
          var unstemmedCollection = new mutable.HashSet[String]()
          unstemmedCollection.add(wordtemp)
          stemToFull.put(word,unstemmedCollection)
        }else{
          val currentWords = stemToFull.getOrElse(word, new mutable.HashSet[String]())
          currentWords.add(wordtemp)
          stemToFull.update(word,currentWords)
        }
        if(!wordCounts.contains(word)){
          wordCounts.put(word,1)
        }else{
          val currentCount = wordCounts.getOrElse(word,0)+1
          wordCounts.put(word,currentCount)
        }
      }
    }

  }

  def getNLargestWordCounts() = {
    wordCounts foreach (x => totalCount = totalCount + x._2)
    wordCounts.toSeq.sortBy(_._2).foreach(x =>{
      var bar = ""
      for(ind <- 0 to (1500 * x._2.toDouble/totalCount.toDouble).toInt){
        bar = bar + "|"
      }
      println(bar)
      println(stemToFull.get(x._1).toString().replaceFirst("""Some\(Set""","").replaceFirst("""\)""","") + ", " + x._2.toDouble/totalCount.toDouble)
    })
  }

  def sumWordCounts(word:String,count:Int):Unit = {
    totalCount = totalCount + count;
  }

}
