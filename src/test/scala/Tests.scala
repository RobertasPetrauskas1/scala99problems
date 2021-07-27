import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.util.NoSuchElementException

class Tests extends AnyFlatSpec {

  "isEmpty" should "return true when chain is empty" in {
    Chain().isEmpty shouldBe true
  }
  it should "return false when chain has values" in {
    Chain(1, 1, 2, 3, 5, 8).isEmpty shouldBe false
  }

  "takeWhile" should "return a chain of values while predicate is true" in {
    Chain(1, 1, 2, 3, 5, 8).takeWhile(_ < 5) shouldBe Chain(1, 1, 2, 3)
  }

  it should "return an empty chain if first value fails predicate check" in {
    Chain(1, 1, 2, 3, 5, 8).takeWhile(_ > 5) shouldBe Chain()
  }

  "dropWhile" should "return a subset of values without the one that were dropped while predicate was true" in {
    Chain(1, 1, 2, 3, 5, 8).dropWhile(_ <= 3) shouldBe Chain(5, 8)
  }

  it should "return the original chain if first element fails predicate check" in {
    Chain(1, 1, 2, 3, 5, 8).dropWhile(_ > 3) shouldBe Chain(1, 1, 2, 3, 5, 8)
  }

  "+:" should "append element to start of chain" in {
    1 +: Chain(2) shouldBe Chain(1, 2)
  }

  "++" should "append Chain of elements to start of chain and return a flat chain" in {
    Chain(1, 2) ++ Chain(3, 4) shouldBe Chain(1, 2, 3, 4)
  }

  "P1 - last" should "return last item" in {
    Chain(1, 1, 2, 3, 5, 8).last shouldBe 8
  }

  it should "return NoSuchElementException if the chain is empty" in {
    intercept[NoSuchElementException]{
      Chain().last
    }
  }

  "P2 - prelast" should "return second last item" in {
    Chain(1, 1, 2, 3, 5, 8).prelast shouldBe 5
  }

  it should "return NoSuchElementException if the chain is empty or only has one element" in {
    intercept[NoSuchElementException]{
      Chain().prelast
    }
    intercept[NoSuchElementException]{
      Chain(1).prelast
    }
  }


  "P3 - get" should "return the nth item" in {
    Chain(1, 1, 2, 3, 5, 8).get(2) shouldBe 2
  }

  it should "return IndexOutOfBoundsException if the index is larger than the size of the chain" in {
    intercept[IndexOutOfBoundsException]{
      Chain(1, 2).get(5)
    }
  }

  it should "return IllegalArgumentException if the index is smaller than 0" in {
    intercept[IllegalArgumentException]{
      Chain(1, 2).get(-1)
    }
  }


  "P4 - size" should "return size of chain" in {
    Chain(1, 1, 2, 3, 5, 8).size shouldBe 6
    Chain().size shouldBe 0
  }

  "P5 - reverse" should "return chain in reversed order" in {
    Chain(1, 1, 2, 3, 5, 8).reverse shouldBe Chain(8, 5, 3, 2, 1, 1)
    Chain().reverse shouldBe Chain()
    Chain(1).reverse shouldBe Chain(1)
  }

  "P7 - flatten" should "return flattened chain" in {
    Chain(Chain(1, 1), 2, Chain(3, Chain(5, 8))).flatten shouldBe Chain(1, 1, 2, 3, 5, 8)
    Chain(1).flatten shouldBe Chain(1)
    Chain().flatten shouldBe Chain()
  }

  "P8 - compress" should "remove duplicate preceding entries" in {
    Chain('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e').compress shouldBe Chain('a', 'b', 'c', 'a', 'd', 'e')
    Chain('a').compress shouldBe Chain('a')
    Chain().compress shouldBe Chain()
  }

  "P9 - pack" should "pack duplicate preceding entries in seperate lists" in {
    Chain('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e').pack shouldBe
      Chain(Chain('a', 'a', 'a', 'a'), Chain('b'), Chain('c', 'c'), Chain('a', 'a'), Chain('d'), Chain('e', 'e', 'e', 'e'))

    Chain().pack shouldBe Chain()
  }

  "P10 - encode" should "encode packed data to a tuple in a format of (size, element)" in {
    Chain('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e').encode shouldBe
      Chain((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))

    Chain().encode shouldBe Chain()
  }

  "P12 - decode" should "decode the encoded chain" in {
    Chain((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')).decode shouldBe
      Chain('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')

    Chain().decode shouldBe Chain()
  }
}
