// Trait for terms, which can be either constants or variables
sealed trait Term
case class Constant(value: String) extends Term
case class Variable(name: String) extends Term

// Case class for atoms, which consist of a relation name and a tuple of terms
case class Atom(relationName: String, terms: Set[Term])

// Case class for the conjunctive query
case class ConjunctiveQuery(
  queryId: Int,
  headAtom: Atom,
  bodyAtoms: List[Atom]
)

// Define some utility methods
object ConjunctiveQueryUtils {
  def printQuery(query: ConjunctiveQuery): Unit = {
    println(s"Query ID: ${query.queryId}")
    println(s"Head Atom: ${query.headAtom}")
    println("Body Atoms:")
    query.bodyAtoms.foreach(println)
  }
}

class Homomorphism(private val mapping: Map[Term, Term] = Map.empty) {

  // Method to add a mapping
  def addMapping(from: Term, to: Term): Homomorphism = {
    new Homomorphism(mapping + (from -> to))
  }

  // Method to retrieve a mapping
  def apply(term: Term): Option[Term] = {
    mapping.get(term)
  }

  // Method to get all mappings
  def getAllMappings: Map[Term, Term] = {
    mapping
  }

  // Override toString for better readability
  override def toString: String = {
    mapping.toString()
  }
}

// Define a case class for the hypergraph
case class Hypergraph(edges: Set[Set[Term]]) {
  def isEmpty: Boolean = edges.isEmpty
}

//object Hypergraph {
///  def fromQuery(query: ConjunctiveQuery): Hypergraph = {
//    val edges = query.bodyAtoms.map(atom => atom.terms.toSet)
//    Hypergraph(edges)
//  }/
//}

object GYO {
  // maxRecursions to stop recursion in case of repeated false for witness..
  def reduceQuery(body: List[Atom], counter: Int = 0 , maxRecursions: Int = 4): List[Atom] = {
    body match {
      case Nil => body
      case head :: Nil => 
        // single atomic formula, has no cycle, return empty list
        List.empty[Atom]
      
      case head :: restOfAtoms =>
        val headTerms = head.terms
        val uniqueTerms = restOfAtoms.foldLeft(headTerms) {(acc, atom) =>
            val terms = atom.terms
            acc.diff(terms)
          } 
        val nonUniqueTerms = headTerms.diff(uniqueTerms)        
        val witness = restOfAtoms.find(atom => atom.terms.intersect(nonUniqueTerms) == nonUniqueTerms)
        
        println("ut: " + uniqueTerms)
        println("nut: " + nonUniqueTerms)
        println("witness: " + witness)
        println("--------------------------")
      
        witness match {
          case Some(_) =>
            reduceQuery(restOfAtoms, 0)
          case None => 
            if (counter >= maxRecursions) {
              body
            } else {
              reduceQuery(restOfAtoms :+ head, counter + 1)

            }
            
        }
    }  
    
  } 
  
  def isAcyclic(query: ConjunctiveQuery): Boolean = {
    val ba = query.bodyAtoms
    val reducedQuery = reduceQuery(ba) 
    reducedQuery.isEmpty
    
  }

  def log(line: String): Unit = {
    
    val txtOutputPath= os.pwd / "output"

    //make folder 
    //if (!os.exists(txtOutputPath)){
   //   os.makeDir.all(txtOutputPath)
   // }

    val path: os.Path = txtOutputPath / "output.txt"

    os.write(path, line)
  }
}


// Example usage
@main def main(): Unit = {
  // Define some constants and variables
  val x = Variable("x")
  val y = Variable("y")
  val z = Variable("z")
  val w = Variable("w")
  val u = Variable("u")
  val V = Constant("V")

  // Define conjunctive query's

  val answer1 = Atom("answer", Set())
  val bodyA = Atom("A", Set(x, y))
  val bodyB = Atom("B", Set(y, z))
  val bodyC = Atom("C", Set(z, w, u, V))
  val bodyD = Atom("D", Set(w, y))

  val query1 = ConjunctiveQuery(
    queryId = 1,
    headAtom = answer1,
    bodyAtoms = List(bodyA, bodyB, bodyC)
  )

    val query2 = ConjunctiveQuery(
    queryId = 2,
    headAtom = answer1,
    bodyAtoms = List(bodyA, bodyB, bodyC, bodyD)
  )

  // Print the query
  //ConjunctiveQueryUtils.printQuery(query)

  // Create a homomorphism
 // val homomorphism = new Homomorphism()
///    .addMapping(x, c1)
//    .addMapping(y, c2)

  println(GYO.isAcyclic(query1))
  println(GYO.isAcyclic(query2))
  //GYO.isAcyclic(query2)

}
