package someQuestion

import scala.io.Source

case class Trie[A](value:A,  children:Map[A, Trie[A]], end:Boolean=false)


object LongestEnglishWord{

  //O(height of tree)
  def lookupTrie(word:String, trie:Trie[String]):Option[Trie[String]]={
    word match{
      case "" =>{
        Some(trie)
      }
      case _ => {
        val charArray = word.toCharArray
        val head = charArray.head
        val tail = charArray.tail
        val currentValue = trie.value+head
        if(trie.children.get(currentValue).isDefined){
          lookupTrie(new String(tail), trie.children.get(currentValue).get)
        }
        else{
          None
        }
      }
    }
  }

  //O(n*m) where n is the number of words in dict and m is the number of characters in word
  def buildTrie(wordList:List[String])={
    wordList.foldLeft(Trie[String]("", Map[String, Trie[String]]()))((trie, word) =>{
      constructTrie(trie, word)
    })
  }

  def constructTrie(currentTrie:Trie[String], word:String):Trie[String]={
    word match{
      case ""=> {
        currentTrie.copy(end=true)
      }
      case _=> {
        val charArray = word.toCharArray
        val head = charArray.head
        val tail = charArray.tail
        val newValue = currentTrie.value+head
        val child = currentTrie.children.getOrElse(
          newValue, Trie(newValue, Map[String, Trie[String]]()))
        val newChild = constructTrie(child, new String(tail))
        val newChildren = currentTrie.children + (newValue -> newChild)
        currentTrie.copy(children = newChildren)
      }
    }
  }

  //O(n!)
  def findLongestWord(currentWord:String, symbols:List[String], trie:Trie[String]):String={
    symbols match{
      case Nil => {
        ""
      }
      case _  => {
        symbols.foldLeft("")((longestWord, symbol) =>{
            val found = lookupTrie(currentWord+symbol, trie)
            if(found.isDefined){
              val foundTrie = found.get
              if(foundTrie.children.isEmpty){
                longestWord
              }
              else {
                val tempLongestWord = if(foundTrie.end && foundTrie.value.size > longestWord.size){
                  foundTrie.value
                }
                else{
                  longestWord
                }
                val longestWordFound =  findLongestWord(currentWord+symbol, symbols.filterNot(_==symbol), trie)
                val newLongestWord = if(longestWordFound.size > tempLongestWord.size){
                  longestWordFound
                }
                else{
                  tempLongestWord
                }
                newLongestWord
              }
            }
            else{
              longestWord
            }
        })
      }
    }
  }

  /*
   * Given the symbol in the periodic table
   * what is the longest english word you can 
   * form
   *
   */
  def main(args:Array[String]){
    val periodTableSymbol=
      List(
        "Ac",
        "Ag",
        "Al",
        "Am",
        "Ar",
        "As",
        "At",
        "Au",
        "B",
        "Ba",
        "Be",
        "Bh",
        "Bi",
        "Bk",
        "Br",
        "C",
        "Ca",
        "Cd",
        "Ce",
        "Cf",
        "Cl",
        "Cm",
        "Cn",
        "Co",
        "Cr",
        "Cs",
        "Cu",
        "Db",
        "Ds",
        "Dy",
        "Er",
        "Es",
        "Eu",
        "F",
        "Fe",
        "Fl",
        "Fm",
        "Fr",
        "Ga",
        "Gd",
        "Ge",
        "H",
        "He",
        "Hf",
        "Hg",
        "Ho",
        "Hs",
        "I",
        "In",
        "Ir",
        "K",
        "Kr",
        "La",
        "Li",
        "Lr",
        "Lu",
        "Lv",
        "Md",
        "Mg",
        "Mn",
        "Mo",
        "Mt",
        "N",
        "Na",
        "Nb",
        "Nd",
        "Ne",
        "Ni",
        "No",
        "Np",
        "O",
        "Os",
        "P",
        "Pa",
        "Pb",
        "Pd",
        "Pm",
        "Po",
        "Pr",
        "Pt",
        "Pu",
        "Ra",
        "Rb",
        "Re",
        "Rf",
        "Rg",
        "Rh",
        "Rn",
        "Ru",
        "S",
        "Sb",
        "Sc",
        "Se",
        "Sg",
        "Si",
        "Sm",
        "Sn",
        "Sr",
        "Ta",
        "Tb",
        "Tc",
        "Te",
        "Th",
        "Ti",
        "Tl",
        "Tm",
        "U",
        "Uuo",
        "Uup",
        "Uus",
        "Uut",
        "V",
        "W",
        "Xe",
        "Y",
        "Yb",
        "Zn",
        "Zr"
        ).map(_.toLowerCase)

    val dictionary = Source.fromFile("/usr/share/dict/words").getLines.map(_.toLowerCase).toList
    val wordTrie = buildTrie(dictionary)
    //result => pauciarticulate
    println("result:" + findLongestWord("", periodTableSymbol, wordTrie))
  }


  
}

// vim: set ts=2 sw=2 et:
