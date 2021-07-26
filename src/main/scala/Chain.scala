import Chain.ChainImplicits

import scala.annotation.tailrec
import scala.language.postfixOps

sealed trait Chain[+A]

case object Hole extends Chain[Nothing]

case class Link[A](head: A, tail: Chain[A]) extends Chain[A]

object Chain {

  def apply[A](items: A*): Chain[A] = {
    if (items.isEmpty) Hole
    else Link(items.head, apply(items.tail: _*))
  }

  final def repeat[A](n: Int, e: A): Chain[A] = {
    @tailrec
    def loop(counter: Int, out: Chain[A] = Hole): Chain[A] = {
      if (counter == 0) out
      else loop(counter - 1, Link(e, out))
    }

    loop(n)
  }

  final def decode[A](ls: Chain[(Int, A)]): Chain[A] = {
    def loop(in: Chain[(Int, A)]): Chain[A] = {
      in match {
        case Link((n, e), Hole) => repeat(n, e)
        case Link((n, e), tail) => repeat(n, e) ::: loop(tail)
      }
    }

    loop(ls)
  }

  implicit class ChainImplicits[A](chain: Chain[A]) {
    final def isEmpty: Boolean = {
      chain == Hole
    }

    final def size: Int = {
      @tailrec
      def loop(in: Chain[A], sum: Int = 0): Int = {
        in match {
          case Hole => sum
          case Link(_, tail) => loop(tail, sum + 1)
        }
      }

      loop(chain)
    }

    final def head: A = {
      chain match {
        case Hole => throw new NoSuchElementException
        case Link(a, _) => a
      }
    }

    final def tail: Chain[A] = {
      chain match {
        case Hole => throw new NoSuchElementException
        case Link(_, tail) => tail
      }
    }

    final def last: A = {
      @tailrec
      def loop(in: Chain[A]): A = {
        in match {
          case Link(h, Hole) => h
          case Link(_, tail) => loop(tail)
          case _ => throw new NoSuchElementException
        }
      }

      loop(chain)
    }

    final def prelast: A = {
      @tailrec
      def loop(in: Chain[A]): A = {
        in match {
          case Link(head, Link(_, Hole)) => head
          case Link(_, tail) => loop(tail)
        }
      }

      loop(chain)
    }

    final def get(i: Int): A = {
      @tailrec
      def loop(in: Chain[A], count: Int = i): A = {
        in match {
          case Link(a, _) if count == 0 => a
          case Link(_, tail) if count != 0 => loop(tail, count - 1)
        }
      }

      if (i < 0) throw new IllegalArgumentException
      else if (i >= chain.size) throw new IndexOutOfBoundsException

      loop(chain)
    }

    final def reverse: Chain[A] = {
      @tailrec
      def loop(in: Chain[A], out: Chain[A] = Hole): Chain[A] = {
        in match {
          case Hole => out
          case Link(head, tail) => loop(tail, Link(head, out))
        }
      }

      loop(chain)
    }

    final def flatten: Chain[Any] = {
      def loop(in: Chain[Any], out: Chain[Any] = Hole): Chain[Any] = {
        in match {
          case Hole => out
          case Link(head: Chain[Any], Hole) => loop(head, out)
          case Link(head, Hole) => Link(head, out)
          case Link(head: Chain[Any], tail) => loop(tail, loop(head, out))
          case Link(head, tail) => loop(tail, Link(head, out))
        }
      }

      loop(chain).reverse
    }

    final def compress: Chain[A] = {
      @tailrec
      def loop(in: Chain[A], out: Chain[A] = Hole): Chain[A] = {
        in match {
          case Link(a, Link(b, Hole)) if a == b => Link(b, out)
          case Link(a, Link(b, Hole)) if a != b => Link(a, Link(b, out))
          case Link(a, Link(b, tail)) if a == b => loop(Link(b, tail), out)
          case Link(a, Link(b, tail)) if a != b => loop(Link(b, tail), Link(a, out))
        }
      }

      if (chain.isEmpty || chain.size == 1) chain
      else loop(chain).reverse
    }

    final def takeWhile(p: A => Boolean): Chain[A] = {
      @tailrec
      def loop(in: Chain[A], out: Chain[A] = Hole): Chain[A] = {
        in match {
          case Link(e, Hole) if p(e) => Link(e, out)
          case Link(e, tail) if p(e) => loop(tail, Link(e, out))
          case _ => out
        }
      }

      loop(chain).reverse
    }

    final def dropWhile(p: A => Boolean): Chain[A] = {
      @tailrec
      def loop(in: Chain[A]): Chain[A] = {
        in match {
          case Link(e, _) if !p(e) => in
          case Link(e, Hole) if p(e) => Hole
          case Link(e, tail) if p(e) => loop(tail)
        }
      }

      loop(chain)
    }

    final def pack: Chain[Chain[A]] = {
      @tailrec
      def loop(in: Chain[A], out: Chain[Chain[A]] = Hole): Chain[Chain[A]] = {
        in match {
          case Hole => out
          case Link(a, _) => loop(in.dropWhile(_ == a), Link(in.takeWhile(_ == a), out))
        }
      }

      loop(chain).reverse
    }

    final def encode: Chain[(Int, A)] = {
      @tailrec
      def loop(in: Chain[Chain[A]], out: Chain[(Int, A)] = Hole): Chain[(Int, A)] = {
        in match {
          case Link(a, Hole) => Link((a.size, a.head), out)
          case Link(a, tail) => loop(tail, Link((a.size, a.head), out))
        }
      }

      loop(chain.pack).reverse
    }

    final def ::(e: A): Chain[A] = {
      Link(e, chain)
    }

    final def :::(ls: Chain[A]): Chain[A] = {
      @tailrec
      def loop(in: Chain[A], out: Chain[A]): Chain[A] = {
        in match {
          case Link(a, Hole) => a :: out
          case Link(a, tail) => loop(tail, a :: out)
        }
      }
      loop(ls.reverse, chain)
    }

  }
}

