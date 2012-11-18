package someQuestion

object PermutateList{
  //generate all permutation of a list
  def permutateList[A](list:List[A]):List[List[A]]={
    if(list.isEmpty){
      List[List[A]]()
    }
    else if(list.size == 1){
      List[List[A]](list)
    }
    else{
      list.foldLeft(List[List[A]]())((permutations, item) =>{
        val remainingList = list-(item)
        permutateList(remainingList).foldLeft(permutations)((newPermutations, permutatedList)=>{
          val newPermutation = item::permutatedList
          newPermutation::newPermutations
        })
      })
      }
    }

  def main(args:Array[String])={
    val inputList = 1.to(5).toList
    val permutatedList = permutateList(inputList)
    permutatedList.foreach(println)

  }
}
