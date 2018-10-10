package dahu.model.problem

import dahu.graphs._
import dahu.model.ir._
import dahu.model.types._

object IntBoolUtils {

  def filterIntBools[K, ID <: IDTop](
      tree: LazyTree[K, Total, cats.Id, ID]): LazyTree[K, Total, Option, ID] = {
    filterIntBoolsGraph(tree.tree).rootedAt(tree.root)

  }
  def supportedType(t: Tag[_]): Boolean = t.isBoolean || t.isInt

  def isExprOfIntBool[ID <: IDTop](openASG: OpenASG[_, Total, cats.Id, ID]): ID => Boolean = {
    val recursion: (SomeID => Boolean) => Total[SomeID] => Boolean =
      (hist: (SomeID => Boolean)) =>
        (t: Total[SomeID]) =>
          if(!t.typ.isInt && !t.typ.isBoolean) {
//             println("unsupported: " + t)
            false
          } else if(TreeNode[Total].forallChildren(t)(b => hist(b))) {
            true
          } else {
//            println("indirectly unsupported: " + t)
            false
      }

    val graph = openASG
    val presence = graph.cataExplicit(recursion)
    (id: ID) =>
      presence.getInternal(id)
  }

  def filterIntBoolsGraph[K, ID <: IDTop](
      openASG: OpenASG[K, Total, cats.Id, ID]): OpenASG[K, Total, Option, ID] = {

    openASG.filterInternal(isExprOfIntBool(openASG))
  }

}
