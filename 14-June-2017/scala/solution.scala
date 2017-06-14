import scala.annotation.tailrec

// object Main {

  sealed trait TreeNode[+A]
  case class ExistingTreeNode[A](value: A, left: TreeNode[A], right: TreeNode[A]) extends TreeNode[A]
  case object EmptyTreeNode extends TreeNode[Nothing]


  def addToStack[A](stack: List[A], item: A) = item :: stack

  def popFromStack[A](stack: List[A]): (List[A], Option[A]) = stack match {
    case Nil => (stack, None)
    case x :: xs => (xs, Some(x))
  }


  def enqueue[A](queue: List[A], item: A) = queue :+ item

  def dequeue[A](queue: List[A]) = popFromStack(queue)

  def addToTree[A : Ordering](tree: TreeNode[A], value: A): TreeNode[A] = tree match {
    case EmptyTreeNode => ExistingTreeNode(value, EmptyTreeNode, EmptyTreeNode)
    case ExistingTreeNode(v, l, r) if implicitly[Ordering[A]].lt(value, v) => ExistingTreeNode(v, addToTree(l, value), r)
    case ExistingTreeNode(v, l, r) if implicitly[Ordering[A]].gteq(value, v) => ExistingTreeNode(v, l, addToTree(r, value))
  }


 // def main(args: Array[String]) = {
    /*
        println(addToStack(Nil, 1) == List(1))
        println(addToStack(List(2, 3), 1) == List(1, 2, 3))
        println(popFromStack(List(2, 1)) == (List(1), Some(2)))
        println(popFromStack(Nil) == (List.empty, None))

        println(enqueue(Nil, 1) == List(1))
        println(enqueue(List(2, 3), 1) == List(2, 3, 1))
        println(dequeue(List(2, 1)) == (List(1), Some(2)))
        println(dequeue(Nil) == (List.empty, None))
    */

    addToTree(EmptyTreeNode, 1) == ExistingTreeNode(1, EmptyTreeNode, EmptyTreeNode)
 //   println(
      addToTree(
        ExistingTreeNode(5, EmptyTreeNode, EmptyTreeNode),
        3
      ) ==
        ExistingTreeNode(5, ExistingTreeNode(3, EmptyTreeNode, EmptyTreeNode), EmptyTreeNode)

addToTree(
  ExistingTreeNode(5, EmptyTreeNode, EmptyTreeNode),
  7
) ==
  ExistingTreeNode(5, EmptyTreeNode, ExistingTreeNode(7, EmptyTreeNode, EmptyTreeNode))


addToTree(
  ExistingTreeNode(5, EmptyTreeNode, EmptyTreeNode),
  5
) ==
  ExistingTreeNode(5, EmptyTreeNode, ExistingTreeNode(5, EmptyTreeNode, EmptyTreeNode))

    val tree = List(5, 3, 7, 1, 4).foldLeft[TreeNode[Int]](EmptyTreeNode)({case (t, v) => addToTree(t, v)})

def foldLeft[A, B](tree: TreeNode[A])(init: B)(f: (B, A) => B): B = tree match {
  case EmptyTreeNode => init
  case ExistingTreeNode(v, l, r) => val lres = foldLeft(l)(init)(f)
    val mres = f(lres, v)
    foldLeft(r)(mres)(f)
}

def foldRight[A, B](tree: TreeNode[A])(init: B)(f: (B, A) => B): B = tree match {
  case EmptyTreeNode => init
  case ExistingTreeNode(v, l, r) => val lres = foldRight(r)(init)(f)
    val mres = f(lres, v)
    foldRight(l)(mres)(f)
}

def foldPostOrder[A, B](tree: TreeNode[A])(init: B)(f: (B, A) => B): B = tree match {
  case EmptyTreeNode => init
  case ExistingTreeNode(v, l, r) => val lres = foldPostOrder(l)(init)(f)
    val mres = foldPostOrder(r)(lres)(f)
    f(mres, v)
}

@tailrec
def foldWQ_[A, B](q: List[TreeNode[A]])(init: B)(f: (B, A) => B): B = q match {
  case Nil => init
  case EmptyTreeNode :: qs => foldWQ_(qs)(init)(f)
  case ExistingTreeNode(v, l, r) :: qs =>
    val res = f(init, v)
    foldWQ_(qs ++ List(l, r))(res)(f)
}


def foldWQ[A, B](tree: TreeNode[A])(init: B)(f: (B, A) => B): B = foldWQ_(List(tree))(init)(f)


def foldLeftH[A, B](tree: TreeNode[A], initH: Int = 0)(init: B)(f: (B, Option[A], Int) => B): B = tree match {
  case EmptyTreeNode => f(init, None, initH)
  case ExistingTreeNode(v, l, r) => val lres = foldLeftH(l, initH + 1)(init)(f)
    val mres = f(lres, Some(v), initH)
    foldLeftH(r, initH + 1)(mres)(f)
}



    foldLeft(tree)(List.empty[Int])({case (pv, ne) => pv :+ ne})
    foldRight(tree)(List.empty[Int])({case (pv, ne) => pv :+ ne})
    foldPostOrder(tree)(List.empty[Int])({case (pv, ne) => pv :+ ne})
    foldWQ(tree)(List.empty[Int])({case (pv, ne) => pv :+ ne})



foldLeftH(tree)(Map.empty[Int, List[Int]])({
  case (res, None, _) => res
  case (res, Some(a), currentHeight) => res.updated(currentHeight, res.get(currentHeight).map(_ :+ a).getOrElse(List(a)))
})
foldLeftH(tree)((Int.MaxValue, 0))({
  case ((min, max), None, currentHeight) => (min.min(currentHeight), max.max(currentHeight))
  case ((min, max), _, currentHeight) => (min, max)
})


    //)

    //    println(enqueue(List(2, 3), 1) == List(2, 3, 1))
    //    println(dequeue(List(2, 1)) == (List(1), Some(2)))
    //    println(dequeue(Nil) == (List.empty, None))
  //}

// }
