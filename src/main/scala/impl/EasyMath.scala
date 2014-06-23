package impl

object EasyMath{
    implicit class EasyIterable[A](elements: Iterable[A]){
        def minArgValue[B <% Ordered[B]](f: A => B) = {
            val it = elements.toIterator
            var minX = it.next
            var minV = f(minX)
            for(x <- it){
                if(f(x) < minV) minV = f(x); minX = x
            }
            (minX, minV)
        }

        def minArg[B <% Ordered[B]](f: A => B) = minArgValue(f)._1
        def minValue[B <% Ordered[B]](f: A => B) = minArgValue(f)._2

        def maxArgValue[B <% Ordered[B]](f: A => B) = {
            val it = elements.toIterator
            var maxX = it.next
            var maxV = f(maxX)
            for(x <- it){
                if(f(x) > maxV) maxV = f(x); maxX = x
            }
            (maxX, maxV)
        }

        def maxArg[B <% Ordered[B]](f: A => B) = maxArgValue(f)._1
        def maxValue[B <% Ordered[B]](f: A => B) = maxArgValue(f)._2

        def forAll(condition: A => Boolean) : Boolean = {
            for(x <- elements)
                if(!condition(x)) return false
            true
        }

        def forOne(condition: A => Boolean) : Boolean = {
            for(x <- elements)
                if(condition(x)) return true
            false
        }

        def find(condition: A => Boolean) : Option[A] = {
            for(x <- elements)
                if(condition(x)) return Some(x)
            None
        }
    }

    def range2D(xRange: Range, yRange: Range) = {
        def row(x: Int) = for(y <- yRange) yield (x, y)
        val rows = for(x <- xRange) yield row(x)
        rows.foldLeft(Iterator[(Int, Int)]())(_ ++ _)
    }
}