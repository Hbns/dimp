object Queries {

// Define constants and variables
  val x = Variable("x")
  val y = Variable("y")
  val z = Variable("z")
  val w = Variable("w")
  val u = Variable("u")
  val V = Constant("V")

// Define conjunctive query's
  val query1 = ConjunctiveQuery(
    queryId = 1,
    headAtom = Atom("answer", Set()),
    bodyAtoms = List(Atom("A", Set(x, y)), Atom("B", Set(y, z)), Atom("C", Set(z, w, u, V)))
  )

  val query2 = ConjunctiveQuery(
    queryId = 2,
    headAtom = Atom("answer", Set()),
    bodyAtoms = List(Atom("A", Set(x, y)), Atom("B", Set(y, z)), Atom("C", Set(z, w, u, V)), Atom("D", Set(w, y)))
  )

  val query3 = ConjunctiveQuery(
    queryId = 3,
    headAtom = Atom("answer", Set()),
    bodyAtoms = List(Atom("A", Set(x, x)), Atom("D", Set(x, x)))
  )

  val query4 = ConjunctiveQuery(
    queryId = 4,
    headAtom = Atom ("answer", Set(x)),
    bodyAtoms = List(Atom("E", Set(x, x)))
  )

  val query5 = ConjunctiveQuery(
    queryId = 5,
    headAtom = Atom ("answer", Set(x)),
    bodyAtoms = List(Atom("E", Set(x, y)), Atom("E", Set(y, z)))
  )

  val query6 = ConjunctiveQuery(
    queryId = 6,
    headAtom = Atom ("answer", Set(w)),
    bodyAtoms = List(Atom("E", Set(w, w)), Atom("E", Set(w, u)))
  )

  val query7 = ConjunctiveQuery(
    queryId = 7,
    headAtom = Atom ("answer", Set(x)),
    bodyAtoms = List(Atom("E", Set(x, x)), Atom("E", Set(x, y)), Atom("E", Set(y, z)))
  )

  val query8 = ConjunctiveQuery(
    queryId = 8,
    headAtom = Atom ("answer", Set(y, z)),
    bodyAtoms = List(Atom("E", Set(y, z)), Atom("E", Set(z, z)), Atom("E", Set(z, w)))
  )

    val query9 = ConjunctiveQuery(
    queryId = 9,
    headAtom = Atom ("answer", Set(x, y)),
    bodyAtoms = List(Atom("E", Set(x, y)), Atom("E", Set(y, z)), Atom("E", Set(z, y)), Atom("E", Set(y, w)))
  )

     val query10 = ConjunctiveQuery(
    queryId = 10,
    headAtom = Atom ("answer", Set(z, z)),
    bodyAtoms = List(Atom("E", Set(z, y)), Atom("E", Set(y, z)), Atom("E", Set(y, w)))
  )
}