package mill.javalib.internal

import mill.api.daemon.internal.internal

import scala.collection.mutable.{LinkedHashSet, HashSet}

@internal
object ModuleUtils {

  /**
   * Find all dependencies.
   * The result contains `start` and all its transitive dependencies provided by `deps`,
   * but does not contain duplicates.
   * If it detects a cycle, it throws an exception with a meaningful message containing the cycle trace.
   * @param name The nane is used in the exception message only
   * @param start the start element
   * @param deps A function provided the direct dependencies
   * @throws mill.api.MillException if there were cycles in the dependencies
   */
  // FIXME: Remove or consolidate with copy in JvmWorkerImpl
  def recursive[T](name: String, start: T, deps: T => Seq[T]): Seq[T] = {
    val seen = LinkedHashSet.empty[T]
    val traceSet = HashSet.empty[T]
    
    def visit(trace: List[T], node: T): Unit = {
      if (traceSet.contains(node)) {
        val rendered =
          (node :: (node :: trace.takeWhile(_ != node)).reverse).mkString(" -> ")
        throw new RuntimeException(s"${name}: cycle detected: ${rendered}")
      }
      if (!seen.contains(node)) {
        seen += node
        traceSet += node
        deps(node).foreach(d => visit(node :: trace, d))
        traceSet -= node
      }
    }

    visit(Nil, start)
    seen.toSeq
  }
}
