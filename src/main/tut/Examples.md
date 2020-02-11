# Examples

# Same content with FastParse (`FastParseMagnoliaParseCaseClass`)

```tut:silent:reset
```

## Intro
There might be multiple implementations of `ParseCaseClass`, currently only `RegexParseCaseClass` and `FastParseParseCaseClass` are supported, but only one should be used in a scope (in this case `FastParseParseCaseClass`):
```tut:silent
import com.github.aborg0.caseyclassy.FastParseMagnoliaParseCaseClass
import com.github.aborg0.caseyclassy.FastParseMagnoliaParseCaseClass._
import java.time.{LocalDate, LocalTime}
```
It is possible to parse simple values, like `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `LocalTime`, `LocalDate` and `String`s without comma (`,`) or closing parenthesis (`)`), but this library was designed to parse toString of algebraic data types (products -case classes, tuples- and coproducts) of them. Also some `Seq`s (`List`, `Vector`, `WrappedArray` (for varargs)) are supported.

### Simple primitives

`LocalDate`:

```tut
val date: LocalDate = FastParseMagnoliaParseCaseClass.to[LocalDate]("2018-04-01")
```

or it is also possible to create a parser and reuse it:

```tut
val dateParser = FastParseMagnoliaParseCaseClass[LocalDate]
dateParser.parse("2018-04-01")
dateParser.parse("2018-04-22")
```

Tuple2 of `String` and `Int`:

```tut
FastParseMagnoliaParseCaseClass.to[(String, Int)]("(   hello,4)")
```

Or in the other order:

```tut
val (i, s) = FastParseMagnoliaParseCaseClass.to[(Int, String)]("(4,   hello)")
```

The error messages are not very good:

```tut:fail
val dateTuple1 = FastParseMagnoliaParseCaseClass.to[Tuple1[LocalDate]]("2018-04-01")
```

## Algebraic data types

With help of shapeless the following constructs are supported:
- case classes
- case objects
- sealed hierarchies
- tuples
- a few `Seq` types

### Case classes

```tut
case class Example(a: Int, s: String)
FastParseMagnoliaParseCaseClass.to[Example]("Example(-3, Hello)")
```

```tut
case object Dot

FastParseMagnoliaParseCaseClass.to[Dot.type]("Dot")
```

### Sealed hierarchies

#### Either

```tut
FastParseMagnoliaParseCaseClass.to[Either[Short, Boolean]]("Left(-1111)")
FastParseMagnoliaParseCaseClass.to[Either[Short, Boolean]]("Right(false)")
```

#### Option

```tut
FastParseMagnoliaParseCaseClass.to[Option[Option[Int]]]("Some(None)")
FastParseMagnoliaParseCaseClass.to[Option[Option[Int]]]("None")
FastParseMagnoliaParseCaseClass.to[Option[Either[String, Seq[Boolean]]]]("Some(Right(List()))")
FastParseMagnoliaParseCaseClass.to[Option[Either[String, Seq[Boolean]]]]("Some(Right(List(false, true)))")
```
