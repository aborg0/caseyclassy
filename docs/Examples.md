# Examples

# Same content with FastParse (`FastParseParse`)

```scala mdoc:silent:reset
```

## Intro
There might be multiple implementations of `ParseCaseClass`, currently only `RegexParseCaseClass` and `FastParseParseCaseClass` are supported, but only one should be used in a scope (in this case `FastParseParseCaseClass`):
```scala mdoc:silent
import com.github.aborg0.caseyclassy.FastParseParse
import com.github.aborg0.caseyclassy.FastParseParse.{_, given}
import java.time.{LocalDate, LocalTime}
```
It is possible to parse simple values, like `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `LocalTime`, `LocalDate` and `String`s without comma (`,`) or closing parenthesis (`)`), but this library was designed to parse toString of algebraic data types (products -case classes, tuples- and coproducts) of them. Also some `Seq`s (`List`, `Vector`, `WrappedArray` (for varargs)) are supported.

### Simple primitives

`LocalDate`:

```scala mdoc
val date: LocalDate = FastParseParse.to[LocalDate]("2018-04-01")
```

or it is also possible to create a parser and reuse it:

```scala mdoc
val dateParser = FastParseParse[LocalDate]
dateParser.parse("2018-04-01")
dateParser.parse("2018-04-22")
```

Tuple2 of `String` and `Int`:

```scala mdoc
FastParseParse.to[(String, Int)]("(   hello,4)")
```

Or in the other order:

```scala mdoc
val (i, s) = FastParseParse.to[(Int, String)]("(4,   hello)")
```

The error messages are not very good:


```scala mdoc
val dateTuple1 = util.Try(FastParseParse.to[Tuple1[LocalDate]]("2018-04-01"))
```

## Algebraic data types

With help of shapeless the following constructs are supported:
- case classes
- case objects
- sealed hierarchies
- tuples
- a few `Seq` types

### Case classes

```scala mdoc
case class Example(a: Int, s: String)
FastParseParse.to[Example]("Example(-3, Hello)")
```

```scala mdoc
case object Dot

FastParseParse.to[Dot.type]("Dot")
```

### Sealed hierarchies

#### Either

```scala mdoc
FastParseParse.to[Either[Short, Boolean]]("Left(-1111)")
FastParseParse.to[Either[Short, Boolean]]("Right(false)")
```

#### Option

```scala mdoc
FastParseParse.to[Option[Option[Int]]]("Some(None)")
FastParseParse.to[Option[Option[Int]]]("None")
FastParseParse.to[Option[Either[String, Seq[Boolean]]]]("Some(Right(List()))")
FastParseParse.to[Option[Either[String, Seq[Boolean]]]]("Some(Right(List(false, true)))")
```
