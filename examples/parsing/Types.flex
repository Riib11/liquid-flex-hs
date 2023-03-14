// NOTE: maltyped

module Types where

// int"integer"
type T = int1
type T = int2
type T = int8
type T = int16
type T = int32
type T = int64
type T = int128

// uint"integer"
type T = uint1
type T = uint2
type T = uint8
type T = uint16
type T = uint32
type T = uint64
type T = uint128

// float"32|64"
type T = float32
type T = float64

// bit
type T = bit

// char 
type T = char 

// Array<"type">
type T = Array<int32>
type T = Array<Array<int32>>
type T = Array<Array<Array<int32>>>

// Tuple<"type",*>
type T = Tuple<int32>
type T = Tuple<int32, int32>
type T = Tuple<int32, int32, int32>

// Optional<"type">
type T = Optional<int32>
type T = Optional<Optional<int32>>
type T = Optional<Optional<Optional<int32>>>

// "TypeNamed"
type T = T1
type T = T2
type T = T3
