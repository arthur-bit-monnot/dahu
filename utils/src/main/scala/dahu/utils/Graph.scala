package dahu.utils

object Graph {

  def topologicalOrderLeavesToRoot[@sp(Int) A: ClassTag, F[_]](
      root: A,
      node: A => F[A],
      children: F[A] => Iterable[A]): Iterable[A] = {
    val queue = debox.Buffer[A]()
    val processed = debox.Set[A]()
    val order = debox.Buffer[A]()

    @inline def push(i: A): Unit = {
      if(!processed(i))
        queue.append(i)
    }
    @inline def pop(): A = {
      queue.remove(queue.length - 1)
    }
    push(root)

    while(queue.nonEmpty) {
      val a = pop()
      if(!processed(a)) {
        val fa = node(a)
        val faChildren = children(fa)
        if(faChildren.forall(i => processed(i))) {
          processed += a
          order.append(a)
        } else {
          push(a)
          faChildren.foreach(push)
        }
      }
    }
    order.toIterable()
  }

  def tarjan[V: ClassTag](graph: Map[V, Set[V]]): Array[debox.Set[V]] = {
    tarjan(
      debox.Map.fromIterable(graph.mapValues(s => debox.Set.fromIterable(s)))
    )
  }

  /** Tarjan's algorithm for extracting strongly connected components.
    *
    * Given a directed graph, the algorithm outputs a sequence of strongly connected
    * components sorted in topological order.
    * */
  def tarjan[@specialized(Int) V: ClassTag](
      graph: debox.Map[V, debox.Set[V]]): Array[debox.Set[V]] = {
    class Data(var index: Int, var lowLink: Int, var onStack: Boolean)
    var index = 0
    val stack = debox.Buffer[V]()
    val data = debox.Map[V, Data]()

    @inline def last(stack: debox.Buffer[V]): V = {
      stack(stack.length - 1)
    }
    @inline def pop(stack: debox.Buffer[V]): V = {
      stack.remove(stack.length - 1)
    }

    val components: debox.Buffer[debox.Set[V]] = debox.Buffer()

    for(v <- graph.keysSet) {
      if(!data.contains(v))
        strongConnect(v)
    }

    def strongConnect(v: V): Unit = {
      assert(!data.contains(v))
      data(v) = new Data(index, index, true)
      index += 1
      stack += v
      for(w <- graph.getOrElse(v, debox.Set())) { // todo: creating an object that is not needed
        if(!data.contains(w)) {
          strongConnect(w)
          data(v).lowLink = data(v).lowLink.min(data(w).lowLink)
        } else if(data(w).onStack) {
          data(v).lowLink = data(v).lowLink.min(data(w).index)
        }
      }

      if(data(v).lowLink == data(v).index) {
        val scc: debox.Set[V] = debox.Set()
        var w = last(stack)
        do {
          w = pop(stack)
          data(w).onStack = false
          scc += w
        } while(w != v)
        components += scc
      }
    }

    components.reverse.toArray()
  }

  def topologicalOrder[V: ClassTag](graph: Map[V, Set[V]]): Option[Seq[V]] = {
    val sccs = tarjan(graph)

    val order = debox.Buffer[V]()

    var i = 0
    while(i < sccs.length) {
      if(sccs(i).size == 1)
        sccs(i).foreach(e => order += e)
      else
        return None // the graph has a (non-singleton) strongly connected component
      i += 1
    }
    Some(order.toArray())
  }

}
