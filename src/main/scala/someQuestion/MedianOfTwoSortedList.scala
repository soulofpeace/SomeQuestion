package someQuestion

//given 2 sorted list.
//find the median of the 2 list
//without merging them into a single list
//

object MedianOfTwoSortedList{

  def median(listA:List[Int], listB:List[Int])={
    def rFindMedian(
      list1:List[Int],
      list2:List[Int],
      currentPosition:Int, 
      medianPostion:Int):Option[Int]={
      if(list1.isEmpty && list2.isEmpty){
        None
      }
      else if(list1.isEmpty || list1.head > list2.head){
        if(currentPosition == medianPostion){
          Some(list2.head)
        }
        else{
          rFindMedian(list1, list2.tail, currentPosition+1, medianPostion)
        }
      }
      else {
        if(currentPosition == medianPostion){
          Some(list1.head)
        }
        else{
          rFindMedian(list1.tail, list2, currentPosition+1, medianPostion)
        }
      }
    }
  }

}
