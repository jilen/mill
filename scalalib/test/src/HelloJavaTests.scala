package mill
package scalalib

import mill.api.Result
import mill.util.{TestEvaluator, TestUtil}
import utest._
import utest.framework.TestPath

object HelloJavaTests extends TestSuite {

  object HelloJava extends TestUtil.BaseModule {
    def millSourcePath = TestUtil.getSrcPathBase() / millOuterCtx.enclosing.split('.')

    object core extends JavaModule {
      override def docJarUseArgsFile = false
      object test extends Tests with TestModule.Junit4
    }
    object app extends JavaModule {
      override def docJarUseArgsFile = true
      override def moduleDeps = Seq(core)
      object test extends Tests with TestModule.Junit4
      object testJunit5 extends Tests with TestModule.Junit5 {
        override def ivyDeps: T[Agg[Dep]] = T {
          super.ivyDeps() ++ Agg(ivy"org.junit.jupiter:junit-jupiter-params:5.7.0")
        }
      }
    }
  }
  val resourcePath = os.pwd / "scalalib" / "test" / "resources" / "hello-java"

  def init()(implicit tp: TestPath) = {
    val eval = new TestEvaluator(HelloJava)
    os.remove.all(HelloJava.millSourcePath)
    os.remove.all(eval.outPath)
    os.makeDir.all(HelloJava.millSourcePath / os.up)
    os.copy(resourcePath, HelloJava.millSourcePath)
    eval
  }
  def tests: Tests = Tests {
    "compile" - {
      val eval = init()

      val Right((res1, n1)) = eval.apply(HelloJava.core.compile)
      val Right((res2, 0)) = eval.apply(HelloJava.core.compile)
      val Right((res3, n2)) = eval.apply(HelloJava.app.compile)

      assert(
        res1 == res2,
        n1 != 0,
        n2 != 0,
        os.walk(res1.classes.path).exists(_.last == "Core.class"),
        !os.walk(res1.classes.path).exists(_.last == "Main.class"),
        os.walk(res3.classes.path).exists(_.last == "Main.class"),
        !os.walk(res3.classes.path).exists(_.last == "Core.class")
      )
    }
    "docJar" - {
      "withoutArgsFile" - {
        val eval = init()
        val Right((ref1, _)) = eval.apply(HelloJava.core.docJar)
        assert(os.proc("jar", "tf", ref1.path).call().out.lines.contains("hello/Core.html"))
      }
      "withArgsFile" - {
        val eval = init()
        val Right((ref2, _)) = eval.apply(HelloJava.app.docJar)
        assert(os.proc("jar", "tf", ref2.path).call().out.lines.contains("hello/Main.html"))
      }
    }
    "test" - {
      val eval = init()

      val Left(Result.Failure(ref1, Some(v1))) = eval.apply(HelloJava.core.test.test())

      assert(
        v1._2(0).fullyQualifiedName == "hello.MyCoreTests.lengthTest",
        v1._2(0).status == "Success",
        v1._2(1).fullyQualifiedName == "hello.MyCoreTests.msgTest",
        v1._2(1).status == "Failure"
      )

      val Right((v2, _)) = eval.apply(HelloJava.app.test.test())

      assert(
        v2._2(0).fullyQualifiedName == "hello.MyAppTests.appTest",
        v2._2(0).status == "Success",
        v2._2(1).fullyQualifiedName == "hello.MyAppTests.coreTest",
        v2._2(1).status == "Success"
      )

      val Right((v3, _)) = eval.apply(HelloJava.app.testJunit5.test())

      assert(
        v3._2(0).fullyQualifiedName == "hello.Junit5TestsA",
        v3._2(0).selector == "coreTest()",
        v3._2(0).status == "Success",
        v3._2(1).fullyQualifiedName == "hello.Junit5TestsA",
        v3._2(1).selector == "skippedTest()",
        v3._2(1).status == "Skipped",
        v3._2(2).fullyQualifiedName == "hello.Junit5TestsA",
        v3._2(2).selector == "palindromes(String):1",
        v3._2(2).status == "Success",
        v3._2(3).fullyQualifiedName == "hello.Junit5TestsA",
        v3._2(3).selector == "palindromes(String):2",
        v3._2(3).status == "Success", 
        v3._2(4).fullyQualifiedName == "hello.Junit5TestsB",
        v3._2(4).selector == "packagePrivateTest()",
        v3._2(4).status == "Success"
      )
    }
    "failures" - {
      val eval = init()

      val mainJava = HelloJava.millSourcePath / "app" / "src" / "Main.java"
      val coreJava = HelloJava.millSourcePath / "core" / "src" / "Core.java"

      val Right(_) = eval.apply(HelloJava.core.compile)
      val Right(_) = eval.apply(HelloJava.app.compile)

      os.write.over(mainJava, os.read(mainJava) + "}")

      val Right(_) = eval.apply(HelloJava.core.compile)
      val Left(_) = eval.apply(HelloJava.app.compile)

      os.write.over(coreJava, os.read(coreJava) + "}")

      val Left(_) = eval.apply(HelloJava.core.compile)
      val Left(_) = eval.apply(HelloJava.app.compile)

      os.write.over(mainJava, os.read(mainJava).dropRight(1))
      os.write.over(coreJava, os.read(coreJava).dropRight(1))

      val Right(_) = eval.apply(HelloJava.core.compile)
      val Right(_) = eval.apply(HelloJava.app.compile)
    }
  }
}
