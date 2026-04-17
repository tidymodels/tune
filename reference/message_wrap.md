# Write a message that respects the line width

Write a message that respects the line width

## Usage

``` r
message_wrap(
  x,
  width = options()$width - 2,
  prefix = "",
  color_text = NULL,
  color_prefix = color_text
)
```

## Arguments

- x:

  A character string of the message text.

- width:

  An integer for the width.

- prefix:

  An optional string to go on the first line of the message.

- color_text, color_prefix:

  A function (or `NULL`) that is used to color the text and/or prefix.

## Value

The processed text is returned (invisibly) but a message is written.

## Examples

``` r
library(cli)
Gaiman <-
  paste(
    '"Good point." Bod was pleased with himself, and glad he had thought of',
    "asking the poet for advice. Really, he thought, if you couldn't trust a",
    "poet to offer sensible advice, who could you trust?",
    collapse = ""
  )
message_wrap(Gaiman)
#> "Good point." Bod was pleased with himself, and glad he had thought
#> of asking the poet for advice. Really, he thought, if you couldn't
#> trust a poet to offer sensible advice, who could you trust?
message_wrap(Gaiman, width = 20, prefix = "-")
#> - "Good point." Bod
#>   was pleased with
#>   himself, and glad
#>   he had thought of
#>   asking the poet
#>   for advice.
#>   Really, he
#>   thought, if you
#>   couldn't trust a
#>   poet to offer
#>   sensible advice,
#>   who could you
#>   trust?
message_wrap(Gaiman,
  width = 30, prefix = "-",
  color_text = cli::col_silver
)
#> - "Good point." Bod was
#>   pleased with himself, and
#>   glad he had thought of
#>   asking the poet for advice.
#>   Really, he thought, if you
#>   couldn't trust a poet to
#>   offer sensible advice, who
#>   could you trust?
message_wrap(Gaiman,
  width = 30, prefix = "-",
  color_text = cli::style_underline,
  color_prefix = cli::col_green
)
#> - "Good point." Bod was
#>   pleased with himself, and
#>   glad he had thought of
#>   asking the poet for advice.
#>   Really, he thought, if you
#>   couldn't trust a poet to
#>   offer sensible advice, who
#>   could you trust?
```
