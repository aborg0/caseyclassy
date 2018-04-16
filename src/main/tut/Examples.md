# Examples

## Intro
There might be multiple implementations of ParseCaseClass, currently only RegexParseCaseClass is supported:
```tut:silent
import com.github.aborg0.caseyclassy.RegexParseCaseClass
import com.github.aborg0.caseyclassy.RegexParseCaseClass._
import java.time.{LocalDate, LocalTime}
import shapeless._
```
It might possible to parse simple values, like `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `LocalTime`, `LocalDate` and `String`s without comma (`,`) or closing parenthesis (`)`), but this library was designed to parse toString of algebraic data types (products -case classes, tuples- and coproducts) of them. Also some `Seq`s (`List`, `Vector`, `WrappedArray` (for varargs)) are supported.

### Simple primitives

`LocalDate`:

```tut
val date: LocalDate = RegexParseCaseClass.to[LocalDate]("2018-04-01")
```

Tuple2 of `String` and `Int`:

```tut
RegexParseCaseClass.to[(String, Int)]("(   hello,4)")
```

Or in the other order:

```tut
val (i, s) = RegexParseCaseClass.to[(Int, String)]("(4,   hello)")
```

The error messages are not very good:

```tut:fail
val dateTuple1 = RegexParseCaseClass.to[Tuple1[LocalDate]]("2018-04-01")
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
RegexParseCaseClass.to[Example]("Example(-3, Hello)")
```

```tut
case object Dot

RegexParseCaseClass.to[Dot.type]("Dot")
```

### Sealed hierarchies

#### Either

```tut
RegexParseCaseClass.to[Either[Short, Boolean]]("Left(-1111)")
RegexParseCaseClass.to[Either[Short, Boolean]]("Right(false)")
```

#### Option

```tut
RegexParseCaseClass.to[Option[Option[Int]]]("Some(None)")
RegexParseCaseClass.to[Option[Option[Int]]]("None")
RegexParseCaseClass.to[Option[Either[String, Seq[Boolean]]]]("Some(Right(List()))")
RegexParseCaseClass.to[Option[Either[String, Seq[Boolean]]]]("Some(Right(List(false, true)))")
```

# Limitations

`String` handling is not ideal:

```tut
RegexParseCaseClass.to[Option[Either[String, Seq[Boolean]]]]("Some(Left(List(false, true)))")
```

Please note that the `String` part contains only till the first `,` (`List(false`) within and no error is reported currently.