object Queries {

// Define constants and variables
  val x = Variable("x")
  val y = Variable("y")
  val z = Variable("z")
  val w = Variable("w")
  val u = Variable("u")
  val V = Constant("V")

// Define the test cq from the project description.
  val query1 = ConjunctiveQuery(
    queryId = 1,
    headAtom = Atom("answer", List()),
    bodyAtoms = List(Atom("A", List(x, y)), Atom("B", List(y, z)), Atom("C", List(z, w, u, V)))
  )

  val query2 = ConjunctiveQuery(
    queryId = 2,
    headAtom = Atom("answer", List()),
    bodyAtoms = List(Atom("A", List(x, y)), Atom("B", List(y, z)), Atom("C", List(z, w, u, V)), Atom("D", List(w, y)))
  )

  val query3 = ConjunctiveQuery(
    queryId = 3,
    headAtom = Atom("answer", List()),
    bodyAtoms = List(Atom("A", List(x, x)), Atom("D", List(x, x)))
  )

  val query4 = ConjunctiveQuery(
    queryId = 4,
    headAtom = Atom ("answer", List(x)),
    bodyAtoms = List(Atom("E", List(x, x)))
  )

  val query5 = ConjunctiveQuery(
    queryId = 5,
    headAtom = Atom ("answer", List(x)),
    bodyAtoms = List(Atom("E", List(x, y)), Atom("E", List(y, z)))
  )

  val query6 = ConjunctiveQuery(
    queryId = 6,
    headAtom = Atom ("answer", List(w)),
    bodyAtoms = List(Atom("E", List(w, w)), Atom("E", List(w, u)))
  )

  val query7 = ConjunctiveQuery(
    queryId = 7,
    headAtom = Atom ("answer", List(x)),
    bodyAtoms = List(Atom("E", List(x, x)), Atom("E", List(x, y)), Atom("E", List(y, z)))
  )

  val query8 = ConjunctiveQuery(
    queryId = 8,
    headAtom = Atom ("answer", List(y, z)),
    bodyAtoms = List(Atom("E", List(y, z)), Atom("E", List(z, z)), Atom("E", List(z, w)))
  )

    val query9 = ConjunctiveQuery(
    queryId = 9,
    headAtom = Atom ("answer", List(x, y)),
    bodyAtoms = List(Atom("E", List(x, y)), Atom("E", List(y, z)), Atom("E", List(z, y)), Atom("E", List(y, w)))
  )

     val query10 = ConjunctiveQuery(
    queryId = 10,
    headAtom = Atom ("answer", List(z, z)),
    bodyAtoms = List(Atom("E", List(z, y)), Atom("E", List(y, z)), Atom("E", List(y, w)))
  )
}