import java.util.StringTokenizer 
import collection.mutable.HashMap
import java.io.FileWriter
import java.io.BufferedReader
import java.io.FileReader
import java.lang.String

object parseLabels {
    def parseFile(inputUrlFileName:String) :HashMap[String,Int]= {
      var lineIter = scala.io.Source.fromFile(inputUrlFileName).getLines
      var curLine = lineIter.next()

      var labelIter = new StringTokenizer(curLine, ",")
      var curLabel = labelIter.nextToken()
      
      var wordIter = new StringTokenizer(curLabel, " ")
      var curWord = wordIter.nextToken().toLowerCase
      var curCount = 0
      var clothingMap = new HashMap[String,Int] 

      while (curLine != null){
        while (curLabel != null){
          while (curWord != null){
            if (clothingMap.contains(curWord)){
              curCount=0
              curCount = clothingMap.get(curWord).getOrElse(0)
              curCount = curCount + 1
              clothingMap.put(curWord, curCount) 
            }
            else{
              clothingMap.put(curWord, 1)
            }
            if (wordIter.hasMoreTokens()){
              curWord = wordIter.nextToken().toLowerCase
            }
            else{
              curWord = null
            }
          }
          if (labelIter.hasMoreTokens()){
            curLabel = labelIter.nextToken()
            wordIter = new StringTokenizer(curLabel, " ")
            curWord = wordIter.nextToken().toLowerCase
          }
          else if (lineIter.hasNext){ 
              curLine = lineIter.next()
              labelIter = new StringTokenizer(curLine, ",")
          }
          else{
              curLine = null
              curLabel = null
          }
        }
      }
      clothingMap -= "\n"
      clothingMap
    }
    
    def removeDups(arr:Array[Int]):Array[Int] = {
      var curIndex = 0
      var curVal = 0
      var prevVal = -1
      var retHashMap = new HashMap[Int, Int]
      while (curIndex < arr.size-1){
        curVal = arr(curIndex)
        if (retHashMap.contains(curVal) == false){
          retHashMap.put(curVal,1)
        }
        prevVal = curVal 
        curIndex = curIndex + 1
      }
      retHashMap.keys.toArray
    }
    
    def sortLabels(mapping:HashMap[String,Int],fileName:String) = {
      var freqVals = mapping.values.toArray
      var curVal = 0
      freqVals = removeDups(freqVals)
      var keyIterator = mapping.keys.iterator
      var curKey = " "
      
      scala.util.Sorting.quickSort(freqVals)
      
      val fw = new FileWriter(fileName)
      var curIndex = freqVals.size-1      
      while (curIndex >= 0){
        curVal = freqVals(curIndex)
        
        while (keyIterator.hasNext){
          curKey = keyIterator.next()
          val currentValue = mapping.get(curKey).getOrElse(0)
          if (currentValue == curVal){
            fw.write(curKey+"->"+curVal+"\n")
          } 
        }
        curIndex = curIndex-1
        keyIterator = mapping.keys.iterator
        curKey = " "
      }
      fw.close()
    }
    
    def twoWordParseFile(inputUrlFileName:String) :HashMap[String,Int]= {
      var lineIter = scala.io.Source.fromFile(inputUrlFileName).getLines
      var curLine = lineIter.next()

      var labelIter = new StringTokenizer(curLine, ",")
      var curLabel = labelIter.nextToken()
      
      var wordIter = new StringTokenizer(curLabel, " ")
      var word1 = wordIter.nextToken().toLowerCase
      var word2 = " "
      if (wordIter.hasMoreTokens()){
        word2 = wordIter.nextToken().toLowerCase
      }
      else
      {
        word2 = null
      }
      var curPhrase = word1+" "+word2
      var curCount = 0
      var clothingMap = new HashMap[String,Int] 

      while (curLine != null){
        while (curLabel != null){
          while ((word1 != null) || (word2 != null)){
//            if ((word1 == null) && (word2 != null)){
//              curPhrase = word2
//            }
//            else if ((word1 != null) && (word2 == null)){
            if ((word1 != null) && (word2 == null)){
              curPhrase = null
            }
            else if ((word1 == null) && (word2 != null)){
              curPhrase = null
            }
            else{
              curPhrase = word1+" "+word2
            }
            if (clothingMap.contains(curPhrase)){
              curCount=0
              curCount = clothingMap.get(curPhrase).getOrElse(0)
              curCount = curCount + 1
              clothingMap.put(curPhrase, curCount) 
            }
            else{
              clothingMap.put(curPhrase, 1)
            }
            if (wordIter.hasMoreTokens()){
              word1 = word2
              word2 = wordIter.nextToken().toLowerCase
            }
            else{
              word1 = word2
              word2 = null
            }
          }
          if (labelIter.hasMoreTokens()){
            curLabel = labelIter.nextToken()
            wordIter = new StringTokenizer(curLabel, " ")
            word1 = word2
            word2 = wordIter.nextToken().toLowerCase
          }
          else if (lineIter.hasNext){ 
              curLine = lineIter.next()
              labelIter = new StringTokenizer(curLine, ",")
          }
          else{
              curLine = null
              curLabel = null
          }
        }
      }
      clothingMap -= "\n"
      clothingMap -= null
    }
    
    def getMostFrequentlyOccurringTokens(mapping: HashMap[String,Int]) :String={
      val keyIterator = mapping.keysIterator
      var curKey = "null" 
      var curVal = 0
      var greatestVal1 = 0
      var greatestVal2 = 0
      var greatestVal3 = 0
      var mostFrequent = "The most frequently used tokens are: "
      while (keyIterator.hasNext){
        curKey = keyIterator.next()
        curVal = mapping.get(curKey).getOrElse(0)
        if ((curVal > greatestVal1) && (curVal > greatestVal2) && (curVal > greatestVal3)){
          greatestVal3 = greatestVal2
          greatestVal2 = greatestVal1
          greatestVal1 = curVal
        }
        else if ((curVal < greatestVal1) && (curVal > greatestVal2) && (curVal > greatestVal3)){
          greatestVal3 = greatestVal2
          greatestVal2 = curVal
        }
        else if ((curVal < greatestVal1) && (curVal < greatestVal2) && (curVal > greatestVal3)){
          greatestVal3 = curVal
        }        
      }
      val keyIterator2 = mapping.keysIterator
      
      while (keyIterator2.hasNext){
        curKey = keyIterator2.next()
        curVal = mapping.get(curKey).getOrElse(0)
        if ((curVal == greatestVal1)){
          mostFrequent = mostFrequent+ " "+curKey.toString()+"("+greatestVal1.toString()+") "
        }
        else if  ((curVal == greatestVal2)){
          mostFrequent = mostFrequent+ " "+curKey.toString()+"("+greatestVal2.toString()+") "
        } 
        else if ((curVal == greatestVal3)){
          mostFrequent = mostFrequent+ " "+curKey.toString()+"("+greatestVal3.toString()+") "
        }
      }
      mostFrequent
    }
    
    def createCSVFile(mapping: HashMap[String,Int], outputFileName: String)={
      var curKey = ""
      var curVal = 0
      val keyIterator = mapping.keysIterator
      val fw = new FileWriter(outputFileName)
      fw.write("token,occurrences\n")
      while (keyIterator.hasNext){
        curKey= keyIterator.next()
        curVal=mapping.get(curKey).getOrElse(0)
        fw.write(curKey+","+curVal+"\n")
      }
      fw.close()
    }
    
    def main(args: Array[String]) {
         val inputUrlFileName = args(0)
         val outputFileName = args(1)
         val labelMap = parseFile(inputUrlFileName)
         //val labelMap = twoWordParseFile(inputUrlFileName)
         sortLabels(labelMap,outputFileName)
         //val labelMap = parseFile(inputUrlFileName)
         //val fw = new FileWriter(outputFileName)
         //createCSVFile(labelMap, outputFileName)
         //fw.write(getMostFrequentlyOccurringTokens(labelMap))
         //fw.write("\n")
         //fw.write("\n")         
         //fw.write(labelMap.mkString("\n"))
         //fw.close()
        }
 }