import gleam/list

pub fn literal_bits(source: BitArray, values: List(Int)) -> List(Int) {
  case source {
    <<>> -> list.reverse(values)
    <<bit:int-size(1), rest:bits>> -> literal_bits(rest, [bit, ..values])
    _ -> panic as "where'd that bit go"
  }
}
