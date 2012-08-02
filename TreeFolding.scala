package someQuestion

import scala.collection.immutable.Queue

case class Tree[A](value:A, left:Option[Tree[A]], right:Option[Tree[A]])

/**
 *       a
 *      / \
 *      b  c
 *      /   \
 *     d    e
 *     foldable tree
 *
 *       a
 *      / \
 *      b  c
 *      /\  \
 *     d  f  e
 *     unfoldable tree
 */

object TreeFolding{
  def main(args:Array[String]){
    val sampleFoldableTree = 
      Tree[String](
        "a",
        Some(
          Tree[String](
            "b",
            Some(
              Tree[String](
                "d",
                None,
                None
              )
            ),
            None
          )
        ),
        Some(
          Tree[String](
            "c",
            None,
            Some(
              Tree[String](
                "e",
                None,
                None
              )
            )
          )
        )
    )
    val sampleUnFoldableTree = 
      Tree[String](
        "a",
        Some(
          Tree[String](
            "b",
            Some(
              Tree[String](
                "d",
                None,
                None
              )
            ),
            Some(
              Tree[String](
                "f",
                None,
                None
              )
            )
          )
        ),
        Some(
          Tree[String](
            "c",
            None,
            Some(
              Tree[String](
                "e",
                None,
                None
              )
            )
          )
        )
    )
    println(isFoldable(sampleFoldableTree))
    println(isFoldable(sampleUnFoldableTree))

  }


  O(log n + n)
  def isFoldable[A](tree:Tree[A]):Boolean={
    val nodesToVisit = tree match{
        case Tree(a, None, None) =>{
          true
        }
        case Tree(a, Some(left), None) =>{
          false
        }
        case Tree(a, None, Some(right)) =>{
          false
        }
        case Tree(a, Some(left), Some(right)) =>{
          val leftSequence = bft(Queue((left, "left")), List[String]())
          val rightSequence = bft(Queue((right, "right")), List[String]())
          leftSequence == invertSequenceTree(rightSequence)
        }
      }
    nodesToVisit

  }
   //O(log n)
  def bft[A](nodesToVisit:Queue[(Tree[A], String)], sequenceTree:List[String]):List[String]={
    if(nodesToVisit.isEmpty){
      sequenceTree
    }
    else{
      val (currentItem, remainingNodes) = nodesToVisit.dequeue
      val current = currentItem._1
      val kind = currentItem._2
      val newNodeToVisit = current match{
        case Tree(a, None, None) =>{
          remainingNodes
        }
        case Tree(a, Some(left), None) =>{
         remainingNodes.enqueue((left, "left"))
        }
        case Tree(a, None, Some(right)) =>{
          remainingNodes.enqueue((right, "right"))
        }
        case Tree(a, Some(left), Some(right)) =>{
          remainingNodes.enqueue((left, "left")).enqueue((right, "right"))
        }
      }
      bft[A](newNodeToVisit, sequenceTree:+kind)

    }

  }

  //O(n)
  def invertSequenceTree(sequenceTree:List[String]):List[String]={
    sequenceTree match{
      case Nil =>{
        Nil
      }
      case "left"::xs =>{
        "right"::invertSequenceTree(xs)
      }
      case "right"::xs =>{
        "left"::invertSequenceTree(xs)
      }
    }
  }
}
