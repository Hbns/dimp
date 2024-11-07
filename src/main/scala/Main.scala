// all queries are in Queries.scala
import Queries._

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
        log("Remove ear" + head)
        log("Current query is: empty")
        List.empty[Atom]
      case head :: restOfAtoms =>
        val headTerms = head.terms
        val uniqueTerms = restOfAtoms.foldLeft(headTerms) {(acc, atom) =>
            val terms = atom.terms
            acc.diff(terms)
          } 
        val nonUniqueTerms = headTerms.diff(uniqueTerms)        
        val witness = restOfAtoms.find(atom => atom.terms.intersect(nonUniqueTerms) == nonUniqueTerms)
    
        witness match {
          case Some(_) =>
            log("Remove ear: " + head + "with witness" + witness.get)
            log("Current query is: " + restOfAtoms)
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
    val bodyAtoms = query.bodyAtoms
    log("------------------------------------------------------------------------------------------------------------")
    log("GYO for query: " + bodyAtoms)
    val reducedQuery = reduceQuery(body = bodyAtoms ) 
    reducedQuery.isEmpty
    
  }

  def log(line: String): Unit = {
    val txtOutputPath= os.pwd / "output"
    val path: os.Path = txtOutputPath / "output.txt"
    os.write.append(path, line + "\n")
  }
}


// Example usage
@main def main(): Unit = {
    // Print the query
  //ConjunctiveQueryUtils.printQuery(query)

  // Create a homomorphism
 // val homomorphism = new Homomorphism()
///    .addMapping(x, c1)
//    .addMapping(y, c2)

  val queries = List(query1, query2, query3, query4, query5, query6, query7, query8, query9, query10)
  for (query <- queries) {
      println("qid: " + query.queryId + "," + GYO.isAcyclic(query))
  }

}
