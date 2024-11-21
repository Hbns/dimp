// all queries are in Queries.scala
import Queries._
import Containment.isContainedIn
import os.Path
import GYO.isAcyclic
import Minimality.isMinimal

// Trait for terms, which can be either constants or variables
sealed trait Term
case class Constant(name: String) extends Term
case class Variable(name: String) extends Term

// Case class for atoms, which consist of a relation name and a tuple of terms
case class Atom(relationName: String, terms: List[Term])
// Using Set[Term] for GYO but List for containment..
case class AtomSet(relationName: String, terms: Set[Term])

// F1. Query Data Structure
case class ConjunctiveQuery(
  queryId: Int,
  headAtom: Atom,
  bodyAtoms: List[Atom]
)

// F2. Homomorphism Data Structure
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

  def getMapping(term: Term): Option[Term] = {
    mapping.get(term)
  }

  def getAllMappings: Map[Term, Term] = {
    mapping
  }

  // Override toString for better readability
  override def toString: String = {
    mapping.toString()
  }
}

// logger to write output.txt files
trait Logger {
  def log(line: String): Unit
}

object TestLogger extends Logger {
  private var filePath: Option[Path] = None

  def setFileName(name: String): Unit = {
    val outputDir = os.pwd / "output"
    if (!os.exists(outputDir)) os.makeDir.all(outputDir)
    filePath = Some(outputDir / name)
  }

  def setFilePath(path: String): Unit = {
    filePath = Some(Path(path))
  }

  def getFileName(): String = {
    filePath match {
      case Some(path) => path.toString
      case None => "No file path set"
    }
  }

  def log(line: String): Unit = this.synchronized{
    filePath match {
      case Some(path) => 
        try {
        os.write.append(path, line + "\n")
      } catch {
        case e: Exception =>
          println(s"Failed to write to log file: ${e.getMessage}")
          throw e
      }
      case None => throw new IllegalStateException("File path is not set. Call setFileName first.") 
    }
  }

  def deleteFile(): Unit = {
    filePath.foreach(path => if (os.exists(path)) os.remove(path))
  }
}


// F3. Acyclicity Test
object GYO {
  @scala.annotation.tailrec
  def reduceQuery(body: List[AtomSet], visited: Set[AtomSet]): List[AtomSet] = {
    if (visited.contains(body.head)){
      // visited before
      body
    } else {
    body match {
      case Nil => body
      case head :: Nil => 
        // single atomic formula, has no cycle, return empty list
        TestLogger.log("Remove ear: " + head)
        TestLogger.log("Current query is: empty")
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
            TestLogger.log(s"Remove ear: $head with witness $witness.get")
            TestLogger.log(s"Current query is: $restOfAtoms")
            reduceQuery(restOfAtoms, visited)
          case None => 
            reduceQuery(restOfAtoms :+ head, visited +head)
            }  
        }
      } 
        
  } 
  
  def isAcyclic(cq: ConjunctiveQuery): Int = {
    val bodyAtoms = cq.bodyAtoms
    // convert terms form List[Terms] to Set[Terms]
    val bodyAtomsAsSet = bodyAtoms.map { atom =>
      AtomSet(atom.relationName, atom.terms.toSet)
    }
    // TestLogger administration
    val fileName = s"test-acyclicity-${cq.queryId}.txt"
    TestLogger.setFileName(fileName)
    TestLogger.log(s"GYO for query: $bodyAtoms.")
    
    val reducedQuery = reduceQuery(body = bodyAtomsAsSet, Set.empty) 
    if (reducedQuery.isEmpty) 1 else 0
  } 
  
}

// algorithm does not check for relation arity, 
// in the given cq's to check all similar named relations have same arity.
// F4. Containment Test
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
      TestLogger.log(s"mapped: $mappedAtom")
      toBody.contains(mappedAtom)
     }
  }
  // Function to make the grounded CQ counterexample database D.
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
      TestLogger.log(s"${atom.relationName}($grounded)")
    }

    TestLogger.log(s"Then q1(D) contains the tuple ($cqHeadGrounded)")
    TestLogger.log(s"However, ($cqHeadGrounded) is not in q2(D) since q2(D) is empty.")
  }

  def isContainedIn(cq1: ConjunctiveQuery, cq2: ConjunctiveQuery): Boolean = {
    // cq1 contained in cq2 -> find mapping from cq2 to
    val fromBody = cq2.bodyAtoms
    val toBody = cq1.bodyAtoms
    // TestLogger administration
    val fileName = s"test-containment-${cq1.queryId}-${cq2.queryId}.txt"
    TestLogger.setFileName(fileName)
    TestLogger.log(s"q1 is: $cq1.")
    TestLogger.log(s"q2 is: $cq2.")
    // only start verification if headsize of cq1 and cq2 is equal.
    if (checkHeadSize(cq1, cq2)){
      val homomorphism = makeMapping(fromBody, toBody)
      // if test to organize logging into output-containment.txt
      if (isValidMapping(homomorphism, fromBody, toBody)){
        TestLogger.log(s"A possible homomorphism h from q2 to q1 contains the following mappings: \n" + homomorphism.toString)
        true
      } else {
        // we know the mapping is not valid -> the canonical database is the grounding of cq1 into facts.
        TestLogger.log("A possible counterexample database D contains the following atoms: ")
        groundQuery(cq1)
        false
      }
    } else {
      // headsizes do not match.
      false
    }
  }

}

// F5. Minimality Test
object Minimality{
  
  def isSelfJoinFree(cqBody: List[Atom]): Boolean = {
    val relationNames = cqBody.map(_.relationName)
    // if all relationNames are unique there count equals the number of relations.
    relationNames.distinct.size == relationNames.size
  }
 
// select an Atom, make sure the terms of head are still a subset of the remainingBody.
  def findAtom(body: List[Atom], head: Atom): Option[Atom] = {
    body.find { atom =>
      val remainingBody = body.filterNot(_ == atom)
      val remainingBodyTerms = remainingBody.flatMap(_.terms)
      head.terms.forall(term => remainingBodyTerms.contains(term))
    }
  }

  // minimize the Body for as long we findAtom.
  @scala.annotation.tailrec
  def minimizeBody(body: List[Atom], head: Atom): List[Atom] = {
    val filePath = TestLogger.getFileName()
    findAtom(body, head) match {
      case Some(atom) =>
        TestLogger.log(s"Remove atom: $atom")
        val minimizedBody = body.filterNot(_ == atom)
        // need cq Atom, not AtomSet :-(
        val cq1 = ConjunctiveQuery(1, head, body)
        val cq2 = ConjunctiveQuery(2, head, minimizedBody)
        if ((isContainedIn(cq1, cq2)) && minimizedBody.length > 1){
        TestLogger.setFilePath(filePath)
        TestLogger.log(s"Current query is: $cq2.")
        minimizeBody(minimizedBody, head) 
        } else { 
          minimizedBody
        }
        
      case None => 
        body
    }
  }

  def computeCore(cq: ConjunctiveQuery): ConjunctiveQuery = {
    // extract values
    val cqBody = cq.bodyAtoms
    val cqHead = cq.headAtom
    // if not selfjoinfree minimizeBody.
    isSelfJoinFree(cqBody) match {
      case true => ConjunctiveQuery(cq.queryId, cqHead, cqBody)
      case false => ConjunctiveQuery(cq.queryId, cqHead, minimizeBody(cqBody, cqHead))
    }
  }
  
  def isMinimal(cq: ConjunctiveQuery) = {
    // TestLogger administration
    val fileName = s"test-minimality-${cq.queryId}.txt"
    TestLogger.setFileName(fileName)
    TestLogger.log(s"Minimization for query: $cq.")
    val cqCore = computeCore(cq)
    // if cqCore equals cq no minimization took place => cq is minimal.
    if (cqCore == cq){
      // delete report since: if running isMinimal on the query returns 1 no report is generated.
      TestLogger.deleteFile()
      1
    } else {
      TestLogger.setFileName(fileName)
      TestLogger.log(s"Current query is: $cqCore")
      TestLogger.log(s"No more atoms can be removed.")
      0
    }    
  }
}

// Example usage
@main def main(): Unit = {

val csvMainFileName = os.pwd / "output" / "main-output.csv"
val csvContainmentFileName = os.pwd / "output" / "containment-output.csv"
 
// all queries..
 val queries = List(query1, query2, query3, query4, query5, query6, query7, query8, query9, query10)
 //for (query <- queries) {
//     println("qid: " + query.queryId + "," + GYO.isAcyclic(query))  
// }
 val mainOutputHeader = List("queryId", "isAcyclic", "isMinimal")
 val mainOutput = queries.map {query => (query.queryId, isAcyclic(query), isMinimal(query))}
 val containmentHeader = List("queryId1", "queryId2", "isContainedIn")
 //val containmentOutput = queries.map {query => (query.queryId, isAcyclic(query), isMinimal(query))}

  println(mainOutput)
  //println(Containment.isContainedIn(query5,query6))

  //println(Minimality.isMinimal(query4))
  //Minimality.isMinimal(query8)
  //Minimality.isMinimal(query1)

  
  

}
