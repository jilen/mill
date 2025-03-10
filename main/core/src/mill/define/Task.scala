package mill.define

import mill.api.{CompileProblemReporter, Logger, PathRef, Result, TestReporter}
import mill.define.Applicative.Applyable
import mill.define.EnclosingClass
import sourcecode.Compat.Context
import upickle.default.{ReadWriter => RW, Reader => R, Writer => W}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
 * Models a single node in the Mill build graph, with a list of inputs and a
 * single output of type [[T]].
 *
 * Generally not instantiated manually, but instead constructed via the
 * [[Target.apply]] & similar macros.
 */
abstract class Task[+T] extends Task.Ops[T] with Applyable[Task, T] with GraphNode[Task[_]] {

  /**
   * What other Targets does this Target depend on?
   */
  val inputs: Seq[Task[_]]

  /**
   * Evaluate this target
   */
  def evaluate(args: mill.api.Ctx): Result[T]

  /**
   * Even if this target's inputs did not change, does it need to re-evaluate
   * anyway?
   */
  def sideHash: Int = 0

  def flushDest: Boolean = true

  def asTarget: Option[Target[T]] = None
  def asCommand: Option[Command[T]] = None
  def asWorker: Option[Worker[T]] = None
  def self: Task[T] = this
}

trait NamedTask[+T] extends Task[T] {
  def ctx: mill.define.Ctx
  def label: String = ctx.segment match { case Segment.Label(v) => v }
  override def toString = ctx.segments.render
}
trait Target[+T] extends NamedTask[T] {
  // TODO: change from Some[Target[T]] to Option[Target[T]] in 0.11
  override def asTarget: Some[Target[T]] = Some(this)
  def readWrite: RW[_]
}

object Target extends Applicative.Applyer[Task, Task, Result, mill.api.Ctx] {
  // convenience
  def dest(implicit ctx: mill.api.Ctx.Dest): os.Path = ctx.dest
  def log(implicit ctx: mill.api.Ctx.Log): Logger = ctx.log
  def home(implicit ctx: mill.api.Ctx.Home): os.Path = ctx.home
  def env(implicit ctx: mill.api.Ctx.Env): Map[String, String] = ctx.env
  def args(implicit ctx: mill.api.Ctx.Args): IndexedSeq[_] = ctx.args
  def testReporter(implicit ctx: mill.api.Ctx): TestReporter = ctx.testReporter
  def reporter(implicit ctx: mill.api.Ctx): Int => Option[CompileProblemReporter] = ctx.reporter
  def workspace(implicit ctx: mill.api.Ctx): os.Path = ctx.workspace

  implicit def apply[T](t: T)(implicit rw: RW[T], ctx: mill.define.Ctx): Target[T] =
    macro targetImpl[T]

  def targetImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[T])(
      rw: c.Expr[RW[T]],
      ctx: c.Expr[mill.define.Ctx]
  ): c.Expr[Target[T]] = {
    import c.universe._
    val lhs = Applicative.impl0[Task, T, mill.api.Ctx](c)(reify(Result.Success(t.splice)).tree)

    mill.moduledefs.Cacher.impl0[TargetImpl[T]](c)(
      reify(
        new TargetImpl[T](lhs.splice, ctx.splice, rw.splice)
      )
    )
  }

  implicit def apply[T](t: Result[T])(implicit rw: RW[T], ctx: mill.define.Ctx): Target[T] =
    macro targetResultImpl[T]

  def targetResultImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[Result[T]])(
      rw: c.Expr[RW[T]],
      ctx: c.Expr[mill.define.Ctx]
  ): c.Expr[Target[T]] = {
    import c.universe._
    mill.moduledefs.Cacher.impl0[Target[T]](c)(
      reify(
        new TargetImpl[T](
          Applicative.impl0[Task, T, mill.api.Ctx](c)(t.tree).splice,
          ctx.splice,
          rw.splice
        )
      )
    )
  }

  def apply[T](t: Task[T])(implicit rw: RW[T], ctx: mill.define.Ctx): Target[T] =
    macro targetTaskImpl[T]

  def targetTaskImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[Task[T]])(
      rw: c.Expr[RW[T]],
      ctx: c.Expr[mill.define.Ctx]
  ): c.Expr[Target[T]] = {
    import c.universe._
    mill.moduledefs.Cacher.impl0[Target[T]](c)(
      reify(
        new TargetImpl[T](t.splice, ctx.splice, rw.splice)
      )
    )
  }

  def sources(values: Result[os.Path]*)(implicit ctx: mill.define.Ctx): Sources = macro sourcesImpl1

  def sourcesImpl1(c: Context)(values: c.Expr[Result[os.Path]]*)(ctx: c.Expr[mill.define.Ctx])
      : c.Expr[Sources] = {
    import c.universe._
    val wrapped =
      for (value <- values.toList)
        yield Applicative.impl0[Task, PathRef, mill.api.Ctx](c)(
          reify(value.splice.map(PathRef(_))).tree
        ).tree

    mill.moduledefs.Cacher.impl0[Sources](c)(
      reify(
        new Sources(
          Target.sequence(c.Expr[List[Task[PathRef]]](q"_root_.scala.List(..$wrapped)").splice),
          ctx.splice
        )
      )
    )
  }

  def sources(values: Result[Seq[PathRef]])(implicit ctx: mill.define.Ctx): Sources =
    macro sourcesImpl2

  def sourcesImpl2(c: Context)(values: c.Expr[Result[Seq[PathRef]]])(ctx: c.Expr[mill.define.Ctx])
      : c.Expr[Sources] = {
    import c.universe._

    mill.moduledefs.Cacher.impl0[Sources](c)(
      reify(
        new Sources(
          Applicative.impl0[Task, Seq[PathRef], mill.api.Ctx](c)(values.tree).splice,
          ctx.splice
        )
      )
    )
  }
  def source(value: Result[os.Path])(implicit ctx: mill.define.Ctx): Source = macro sourceImpl1

  def sourceImpl1(c: Context)(value: c.Expr[Result[os.Path]])(ctx: c.Expr[mill.define.Ctx])
      : c.Expr[Source] = {
    import c.universe._

    val wrapped =
      Applicative.impl0[Task, PathRef, mill.api.Ctx](c)(
        reify(value.splice.map(PathRef(_))).tree
      )

    mill.moduledefs.Cacher.impl0[Source](c)(
      reify(
        new Source(
          wrapped.splice,
          ctx.splice
        )
      )
    )
  }

  def source(value: Result[PathRef])(implicit ctx: mill.define.Ctx): Source = macro sourceImpl2

  def sourceImpl2(c: Context)(value: c.Expr[Result[PathRef]])(ctx: c.Expr[mill.define.Ctx])
      : c.Expr[Source] = {
    import c.universe._
    mill.moduledefs.Cacher.impl0[Source](c)(
      reify(
        new Source(
          Applicative.impl0[Task, PathRef, mill.api.Ctx](c)(value.tree).splice,
          ctx.splice
        )
      )
    )
  }
  def input[T](value: Result[T])(implicit
      w: upickle.default.Writer[T],
      ctx: mill.define.Ctx
  ): Input[T] =
    macro inputImpl[T]

  def inputImpl[T: c.WeakTypeTag](c: Context)(value: c.Expr[T])(
      w: c.Expr[upickle.default.Writer[T]],
      ctx: c.Expr[mill.define.Ctx]
  ): c.Expr[Input[T]] = {
    import c.universe._

    mill.moduledefs.Cacher.impl0[Input[T]](c)(
      reify(
        new Input[T](
          Applicative.impl[Task, T, mill.api.Ctx](c)(value).splice,
          ctx.splice,
          w.splice
        )
      )
    )
  }

  def command[T](t: Task[T])(implicit
      ctx: mill.define.Ctx,
      w: W[T],
      cls: EnclosingClass
  ): Command[T] = {
    new Command(t, ctx, w, cls.value)
  }

  def command[T](t: Result[T])(implicit
      w: W[T],
      ctx: mill.define.Ctx,
      cls: EnclosingClass
  ): Command[T] = macro commandImpl[T]

  def commandImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[T])(
      w: c.Expr[W[T]],
      ctx: c.Expr[mill.define.Ctx],
      cls: c.Expr[EnclosingClass]
  ): c.Expr[Command[T]] = {
    import c.universe._
    reify(
      new Command[T](
        Applicative.impl[Task, T, mill.api.Ctx](c)(t).splice,
        ctx.splice,
        w.splice,
        cls.splice.value
      )
    )
  }

  def worker[T](t: Task[T])(implicit ctx: mill.define.Ctx): Worker[T] = macro workerImpl1[T]

  def workerImpl1[T: c.WeakTypeTag](c: Context)(t: c.Expr[Task[T]])(ctx: c.Expr[mill.define.Ctx])
      : c.Expr[Worker[T]] = {
    import c.universe._
    mill.moduledefs.Cacher.impl0[Worker[T]](c)(
      reify(
        new Worker[T](t.splice, ctx.splice)
      )
    )
  }
  def worker[T](t: Result[T])(implicit ctx: mill.define.Ctx): Worker[T] = macro workerImpl2[T]

  def workerImpl2[T: c.WeakTypeTag](c: Context)(t: c.Expr[T])(ctx: c.Expr[mill.define.Ctx])
      : c.Expr[Worker[T]] = {
    import c.universe._
    mill.moduledefs.Cacher.impl0[Worker[T]](c)(
      reify(
        new Worker[T](Applicative.impl[Task, T, mill.api.Ctx](c)(t).splice, ctx.splice)
      )
    )
  }

  def task[T](t: Result[T]): Task[T] = macro Applicative.impl[Task, T, mill.api.Ctx]

  def persistent[T](t: Result[T])(implicit rw: RW[T], ctx: mill.define.Ctx): Persistent[T] =
    macro persistentImpl[T]

  def persistentImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[T])(
      rw: c.Expr[RW[T]],
      ctx: c.Expr[mill.define.Ctx]
  ): c.Expr[Persistent[T]] = {
    import c.universe._

    mill.moduledefs.Cacher.impl0[Persistent[T]](c)(
      reify(
        new Persistent[T](
          Applicative.impl[Task, T, mill.api.Ctx](c)(t).splice,
          ctx.splice,
          rw.splice
        )
      )
    )
  }

  def traverse[T, V](source: Seq[T])(f: T => Task[V]) = {
    new Task.Sequence[V](source.map(f))
  }
  def sequence[T](source: Seq[Task[T]]) = new Task.Sequence[T](source)
  def traverseCtx[I, R](xs: Seq[Task[I]])(f: (IndexedSeq[I], mill.api.Ctx) => Result[R])
      : Task[R] = {
    new Task.TraverseCtx[I, R](xs, f)
  }
}

abstract class NamedTaskImpl[+T](ctx0: mill.define.Ctx, t: Task[T]) extends NamedTask[T] {
  def evaluate(args: mill.api.Ctx) = args[T](0)
  val ctx = ctx0.copy(segments = ctx0.segments ++ Seq(ctx0.segment))
  val inputs = Seq(t)
}

class TargetImpl[+T](t: Task[T], ctx0: mill.define.Ctx, val readWrite: RW[_])
    extends NamedTaskImpl[T](ctx0, t)
    with Target[T] {}

class Command[+T](
    t: Task[T],
    ctx0: mill.define.Ctx,
    val writer: W[_],
    val cls: Class[_]
) extends NamedTaskImpl[T](ctx0, t) {
  override def asCommand = Some(this)
}

class Worker[+T](t: Task[T], ctx0: mill.define.Ctx) extends NamedTaskImpl[T](ctx0, t) {
  override def flushDest = false
  override def asWorker = Some(this)
}

class Persistent[+T](t: Task[T], ctx0: mill.define.Ctx, readWrite: RW[_])
    extends TargetImpl[T](t, ctx0, readWrite) {

  override def flushDest = false
}

class Input[T](t: Task[T], ctx0: mill.define.Ctx, val writer: upickle.default.Writer[_])
    extends NamedTaskImpl[T](ctx0, t) {
  override def sideHash = util.Random.nextInt()
}

class Sources(t: Task[Seq[PathRef]], ctx0: mill.define.Ctx)
    extends Input[Seq[PathRef]](
      t,
      ctx0,
      upickle.default.SeqLikeWriter[Seq, PathRef]
    )

class Source(t: Task[PathRef], ctx0: mill.define.Ctx)
    extends Input[PathRef](
      t,
      ctx0,
      PathRef.jsonFormatter
    )

object Task {

  class Task0[T](t: T) extends Task[T] {
    lazy val t0 = t
    val inputs = Nil
    def evaluate(args: mill.api.Ctx) = t0
  }

  abstract class Ops[+T] { this: Task[T] =>
    def map[V](f: T => V) = new Task.Mapped(this, f)
    def mapDest[V](f: (T, mill.api.Ctx) => Result[V]) = new Task.MappedDest(this, f)

    def filter(f: T => Boolean) = this
    def withFilter(f: T => Boolean) = this
    def zip[V](other: Task[V]) = new Task.Zipped(this, other)

  }

  class Sequence[+T](inputs0: Seq[Task[T]]) extends Task[Seq[T]] {
    val inputs = inputs0
    def evaluate(args: mill.api.Ctx) = {
      for (i <- 0 until args.length)
        yield args(i).asInstanceOf[T]
    }
  }
  class TraverseCtx[+T, V](
      inputs0: Seq[Task[T]],
      f: (IndexedSeq[T], mill.api.Ctx) => Result[V]
  ) extends Task[V] {
    val inputs = inputs0
    def evaluate(args: mill.api.Ctx) = {
      f(
        for (i <- 0 until args.length)
          yield args(i).asInstanceOf[T],
        args
      )
    }
  }
  class Mapped[+T, +V](source: Task[T], f: T => V) extends Task[V] {
    def evaluate(args: mill.api.Ctx) = f(args(0))
    val inputs = List(source)
  }
  class MappedDest[+T, +V](source: Task[T], f: (T, mill.api.Ctx) => Result[V]) extends Task[V] {
    def evaluate(args: mill.api.Ctx) = f(args(0), args)
    val inputs = List(source)
  }
  class Zipped[+T, +V](source1: Task[T], source2: Task[V]) extends Task[(T, V)] {
    def evaluate(args: mill.api.Ctx) = (args(0), args(1))
    val inputs = List(source1, source2)
  }
}
