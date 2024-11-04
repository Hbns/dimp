// Define a trait for terms, which can be either constants or variables
sealed trait Term
case class Constant(value: String) extends Term
case class Variable(name: String) extends Term

// Define a case class for atoms, which consist of a relation name and a tuple of terms
case class Atom(relationName: String, terms: Seq[Term])

// Define a case class for the conjunctive query
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

// Example usage
@main def main(): Unit = {
  // Define some constants and variables
  val x = Variable("X")
  val y = Variable("Y")
  val c1 = Constant("c1")
  val c2 = Constant("c2")

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

  // Print the query
  ConjunctiveQueryUtils.printQuery(query)
}
