Composition Views:
Combine multiple views together to create a new view.
This new view can accept any type of input as long as it can be treated like a number.
- sum
- product
- power

Fixed-Size Views:
- 0..9+
- bool -> 2
- byte -> (power bit 8) -> 256

Unbounded Views:
- uint -> (power bit)
- str -> (power byte)

Process Views:
Changes the way that the data is ordered in a view
- reverse
  - (reverse sum) -> reverse the order of labels
    - eg. (reverse (sum false true))
      true -> false
      false -> true
  - (reverse product) -> reverse the order of fields
    - eg. (reverse (product x byte y byte))
      y -> x
      x -> y
  - (reverse power) -> reverse the order of elements in a product
    - eg. (reverse (power byte 4))
      converts from little endian to big endian

Examples:
v64 = (z64 (power (reverse (power byte 2))))
n64 = (z64 (power (reverse (power byte 4))))