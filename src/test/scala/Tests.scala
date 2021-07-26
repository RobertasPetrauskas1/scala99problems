import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Tests extends AnyFlatSpec {

  "isEmpty" should "return true" in {
    Chain().isEmpty shouldBe true
  }
  it should "return false" in {
    Chain(1, 1, 2, 3, 5, 8).isEmpty shouldBe false
  }

  "takeWhile" should "return a chain of values while predicate is true" in {
    Chain(1, 1, 2, 3, 5, 8).takeWhile(_ < 5) shouldBe Chain(1, 1, 2, 3)
  }

  "dropWhile" should "return a subset of values without the one that were dropped while predicate was true" in {
    Chain(1, 1, 2, 3, 5, 8).dropWhile(_ <= 3) shouldBe Chain(5, 8)
  }

  "::" should "append element to start of chain" in {
    1 :: Chain(2) shouldBe Chain(1, 2)
  }

  ":::" should "append Chain of elements to start of chain and return a flat chain" in {
    Chain(1, 2) ::: Chain(3, 4) shouldBe Chain(1, 2, 3, 4)
  }

  "P1 - last" should "return last item" in {
    Chain(1, 1, 2, 3, 5, 8).last shouldBe 8
  }

  "P2 - prelast" should "return second last item" in {
    Chain(1, 1, 2, 3, 5, 8).prelast shouldBe 5
  }

  "P3 - get" should "return the nth item" in {
    Chain(1, 1, 2, 3, 5, 8).get(2) shouldBe 2
  }

  "P4 - size" should "return size of chain" in {
    Chain(1, 1, 2, 3, 5, 8).size shouldBe 6
  }

  "P5 - reverse" should "return chain in reversed order" in {
    Chain(1, 1, 2, 3, 5, 8).reverse shouldBe Chain(8, 5, 3, 2, 1, 1)
  }

  "P7 - flatten" should "return flattened chain" in {
    Chain(Chain(1, 1), 2, Chain(3, Chain(5, 8))).flatten shouldBe Chain(1, 1, 2, 3, 5, 8)
  }

  "P8 - compress" should "remove duplicate preceding entries" in {
    Chain('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e').compress shouldBe Chain('a', 'b', 'c', 'a', 'd', 'e')
  }

  "P9 - pack" should "pack duplicate preceding entries in seperate lists" in {
    Chain('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e').pack shouldBe
      Chain(Chain('a', 'a', 'a', 'a'), Chain('b'), Chain('c', 'c'), Chain('a', 'a'), Chain('d'), Chain('e', 'e', 'e', 'e'))
  }

  "P10 - encode" should "encode packed data to a tuple in a format of (size, element)" in {
    Chain('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e').encode shouldBe
      Chain((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
  }

  "P12 - decode" should "decode the encoded chain" in {
    Chain.decode(Chain((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))) shouldBe
      Chain('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  }
}
