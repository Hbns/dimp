// Trait for terms, which can be either constants or variables
sealed trait Term
case class Constant(value: String) extends Term
case class Variable(name: String) extends Term

// Case class for atoms, which consist of a relation name and a tuple of terms
case class Atom(relationName: String, terms: Seq[Term])

// Case class for the conjunctive query
case class ConjunctiveQuery(
  queryId: Int,
  headAtom: Atom,
  bodyAtoms: Set[Atom]
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

object Hypergraph {
  def fromQuery(query: ConjunctiveQuery): Hypergraph = {
    val edges = query.bodyAtoms.map(atom => atom.terms.toSet)
    Hypergraph(edges)
  }
}

object GYO {
  def reduce(hypergraph: Hypergraph): Hypergraph = {
    var currentHypergraph = hypergraph
    var changed = true

    while (changed) {
      changed = false
      val newEdges = currentHypergraph.edges.flatMap { edge =>
        if (isEar(edge, currentHypergraph.edges)) {
          changed = true
          None
        } else {
          Some(edge)
        }
      }

      val newHypergraph = Hypergraph(newEdges)
      if (newHypergraph.edges != currentHypergraph.edges) {
        changed = true
        currentHypergraph = newHypergraph
      }
    }

    currentHypergraph
  }

  def isEar(edge: Set[Term], edges: Set[Set[Term]]): Boolean = {
    edge.exists { term =>
      edges.count(_.contains(term)) == 1
    } && edge.exists { term =>
      edges.exists(otherEdge => otherEdge != edge && otherEdge.contains(term))
    }
  }

  def isAcyclic(query: ConjunctiveQuery): Boolean = {
    val hypergraph = Hypergraph.fromQuery(query)
    val reducedHypergraph = reduce(hypergraph)
    reducedHypergraph.isEmpty
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
  val c1 = Constant("c1")
  val c2 = Constant("c2")
  val V = Constant("V")

  // Define some atoms
  val headAtom = Atom("headRelation", Seq(x, y))
  val bodyAtom1 = Atom("bodyRelation1", Seq(x, c1))
  val bodyAtom2 = Atom("bodyRelation2", Seq(y, c2))

  // Define a conjunctive query
  val query = ConjunctiveQuery(
    queryId = 1,
    headAtom = headAtom,
    bodyAtoms = Set(bodyAtom1, bodyAtom2)
  )

  val answer1 = Atom("answer", Seq())
  val bodyA = Atom("A", Seq(x, y))
  val bodyB = Atom("B", Seq(y, z))
  val bodyC= Atom("C", Seq(z, w, u, V))

  val query1 = ConjunctiveQuery(
    queryId = 2,
    headAtom = answer1,
    bodyAtoms = Set(bodyA, bodyB, bodyC)
  )

  // Print the query
  //ConjunctiveQueryUtils.printQuery(query)

  // Create a homomorphism
  val homomorphism = new Homomorphism()
    .addMapping(x, c1)
    .addMapping(y, c2)

  // Print the homomorphism
  //println(homomorphism)

    // Create a hypergraph from the query
  val hypergraph = Hypergraph.fromQuery(query1)

  // Check if the hypergraph is empty
  println(hypergraph)
  GYO.log(hypergraph.toString())
}
