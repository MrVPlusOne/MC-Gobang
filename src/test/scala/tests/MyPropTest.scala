package tests

import org.scalacheck.{Prop, Shrink}
import org.scalacheck.Test.Parameters
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.Checkers._

trait MyPropTest extends WordSpec{
  def checkProp(prop: Prop, maxSize: Int = 80): Unit = {
    check(prop, Parameters.default.withMaxSize(maxSize).withMinSize(1).withMinSuccessfulTests(20))
  }
}
