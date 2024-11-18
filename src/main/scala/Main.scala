// all queries are in Queries.scala
import Queries._

// Trait for terms, which can be either constants or variables
sealed trait Term
case class Constant(name: String) extends Term
case class Variable(name: String) extends Term

// Case class for atoms, which consist of a relation name and a tuple of terms
case class Atom(relationName: String, terms: List[Term])
// Using Set for GYO but List for containment..
case class AtomSet(relationName: String, terms: Set[Term])

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
  def addMapping(from: Term, to: Term): Homomorphism = {
     if (!mapping.contains(from)) {
      new Homomorphism(mapping + (from -> to))
    } else {
      this 
    }
  }
  
  def containsMapping(term: Term): Boolean = {
    mapping.contains(term)
  }

  // Method to retrieve a mapping
  def getMapping(term: Term): Option[Term] = {
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
  def reduceQuery(body: List[AtomSet], counter: Int = 0 , maxRecursions: Int = 4): List[AtomSet] = {
    body match {
      case Nil => body
      case head :: Nil => 
        // single atomic formula, has no cycle, return empty list
        log("Remove ear" + head)
        log("Current query is: empty")
        List.empty[AtomSet]
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
    // convert terms form List[Terms] to Set[Terms]
    val bodyAtomsAsSet = bodyAtoms.map { atom =>
      AtomSet(atom.relationName, atom.terms.toSet)
    }
    log("------------------------------------------------------------------------------------------------------------")
    log("GYO for query: " + bodyAtoms)
    val reducedQuery = reduceQuery(body = bodyAtomsAsSet) 
    reducedQuery.isEmpty
    
  }

  def log(line: String): Unit = {
    val txtOutputPath= os.pwd / "output"
    val path: os.Path = txtOutputPath / "output-GYO.txt"
    os.write.append(path, line + "\n")
  }
}

// algorithm does not check for relation arity, 
// in the given cq's all similar named relations have same arity.
object Containment {
  // Conjunctive queries can only be contained if similar number of terms in the head.
  def checkHeadSize(cq1: ConjunctiveQuery, cq2: ConjunctiveQuery): Boolean = {
   cq1.headAtom.terms.size == cq2.headAtom.terms.size
  }
  // Try to build Homomorphism proposition
  def makeMapping(fromBody: List[Atom], toBody: List[Atom]): Homomorphism = {
    var homomorphism = new Homomorphism()
    // make a mapping
    fromBody.foreach { atomFrom =>
        // Find all matching atoms in toBody based on relationName
        toBody.filter(_.relationName == atomFrom.relationName).foreach { atomTo =>
          // Zip the terms and add mappings
          atomFrom.terms.zip(atomTo.terms).foreach { case (from, to) =>
            homomorphism = homomorphism.addMapping(from, to)
          }
        }
  }
      // return
      homomorphism
  }
  // verify for each relation if it exists in cq1 after replacing terms from mapping.
  def isValidMapping(mapping: Homomorphism, fromBody: List[Atom], toBody: List[Atom]): Boolean = {
    fromBody.forall{ atomFrom =>
      // replace terms by the ones found in mapping.
      val mappedAtom = Atom(
        relationName = atomFrom.relationName,
        terms = atomFrom.terms.map(term => mapping.getMapping(term).getOrElse(term)))
      // does the atom with terms replaced from mapping exist in toBody?
      toBody.contains(mappedAtom)
     }
  }
  // Function to log the grounded CQ counterexample database D.
  def groundQuery(cq: ConjunctiveQuery): Unit = {
    // Helper function to ground terms ("apply Gs mapping")
    def groundTerms(terms: List[Term]): String =
      terms.map {
        case Variable(name) => s"'${name.toUpperCase}'"
        case Constant(name) => s"'${name.toUpperCase}'"
      }.mkString(",")

    val cqHeadGrounded = groundTerms(cq.headAtom.terms)
    
    cq.bodyAtoms.foreach { atom =>
      val grounded = groundTerms(atom.terms)
      log(s"${atom.relationName}($grounded)")
    }

    log(s"Then q1(D) contains the tuple ($cqHeadGrounded)")
    log(s"However, ($cqHeadGrounded) is not in q2(D) since q2(D) is empty.")
  }


  def isContainedIn(cq1: ConjunctiveQuery, cq2: ConjunctiveQuery): Boolean = {
    // cq1 contained in cq2 -> find mapping from cq2 to
    val fromBody = cq2.bodyAtoms
    val toBody = cq1.bodyAtoms
    log(s"q1 is: $cq1")
    log(s"q2 is: $cq2")
    // only start verification if headsize of cq1 and cq2 is equal.
    if (checkHeadSize(cq1, cq2)){
      val homomorphism = makeMapping(fromBody, toBody)
      // if test to organize logging into output-containment.txt
      if (isValidMapping(homomorphism, fromBody, toBody)){
        log(s"A possible homomorphism h from q2 to q1 contains the following mappings: \n" + homomorphism.toString)
        log("------------------------------------------------------------------------------------------------------------")
        true
      } else {
        // we know the mapping is not valid -> the canonical database is the grounding of cq1 into facts.
        log("A possible counterexample database D contains the following atoms: ")
        groundQuery(cq1)
        log("------------------------------------------------------------------------------------------------------------")
        false
      }
    } else {
      // headsizes do not match.
      false
    }
  }

  def log(line: String): Unit = {
    val txtOutputPath= os.pwd / "output"
    val path: os.Path = txtOutputPath / "output-containment.txt"
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

// al queries..
// val queries = List(query1, query2, query3, query4, query5, query6, query7, query8, query9, query10)
// for (query <- queries) {
//     println("qid: " + query.queryId + "," + GYO.isAcyclic(query))  
// }

  println(Containment.isContainedIn(query8,query9))
  println(Containment.isContainedIn(query5,query6))

  
  

}
