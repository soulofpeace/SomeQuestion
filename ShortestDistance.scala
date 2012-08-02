package someQuestion

import scala.math._

object ShortestDistance{
  def main(args:Array[String]){
    /**
     * Given a locality map
     * where 0 denote no train station
     * and 1 denote presence of a train station
     * find the shortest distance from every
     * location to the train station
     **/
    val sampleLocalityMap = 
      List(
        List(0, 0, 1, 0, 1, 0),
        List(0, 0, 0, 1, 0, 1),
        List(1, 0, 1, 0, 0, 1),
        List(0, 1, 1, 1, 0, 1),
        List(0, 0, 0, 0, 0, 0)
      )

    println(shortestDistanceToTrainStation(sampleLocalityMap))

  }

  //O(m^2*y)
  def shortestDistanceToTrainStation(localityMap:List[List[Int]])={
    val trainStationLocationList = getTrainStationLocationList(localityMap)
    localityMap.indices.toList.foldLeft(List[List[Int]]())(
      (distanceMatrix, rowNum) =>{
        distanceMatrix:+localityMap(rowNum).indices.toList.foldLeft(List[Int]())(
          (distanceMatrixRow, columnNum) => {
            distanceMatrixRow:+trainStationLocationList.foldLeft(9999)(
              (min, trainStationLocation)=>{
                val xDistance = abs(trainStationLocation._1 -rowNum)
                val yDistance = abs(trainStationLocation._2 - columnNum)
                val newMin = if(min>(xDistance+yDistance)){
                  xDistance+yDistance
                }
                else{
                  min
                }
                newMin
          })
        })
      }
    )

  }

  //O(n^2)
  def getTrainStationLocationList(localityMap:List[List[Int]])={
    localityMap.zipWithIndex.foldLeft(Set[(Int, Int)]())(
      (trainStationLocationList, row)=>{
        val rowItems = row._1
        val rowNum = row._2
        rowItems.zipWithIndex.filter(
          column  =>{
            val item = column._1
            item == 1
        }).foldLeft(trainStationLocationList)(
          (trainList, column)=>{
            val columnNum = column._2
            trainList+((rowNum, columnNum))
          })
      }
    )
  }
}
