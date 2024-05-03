[
  true,
  false,
  null,
  'foo',
  0.0,
  [],
  [1,2,3],
  [1,2,[3,[4,5,6][0],7][1]][2],
  2*(1+(8>>1)-(1<<(1|1&1^0))),
  !false && false || true,
  [
    1 < 2,  "a" < "b",  [] < ["a"],  [] < [],  ["a"] < [],  ["a", "b"] < ["a", "c"],
    1 > 2,  "a" > "b",  [] > ["a"],  [] > [],  ["a"] > [],  ["a", "b"] > ["a", "c"],
    1 <= 2, "a" <= "b", [] <= ["a"], [] <= [], ["a"] <= [], ["a", "b"] <= ["a", "c"],
    1 >= 2, "a" >= "b", [] >= ["a"], [] >= [], ["a"] >= [], ["a", "b"] >= ["a", "c"],
  ],
  [[1]+[2],"a"+"b"],
  [!false, ~1, -1, +1],
  if 1 < 2 then 10 else 20,
  if 1 > 2 then 10,
  (function() 10)(),
  true || error "unreachable",
  false && error "unreachable",
  [0, error "unreachable"][0],
  local x = 3; 10,
  local x = y * 2, y = z + 3, z = 5; x,
  (function(a) a)(10),
  (function(a) a)(a=10),
  (function(a=10) a)(),
  (function(a, b) a-b)(b=1,a=2),
  (function(a, b=1) a-b)(10,20),
  (function(y) (function(x) function(y) x+y)(y))(2)(1),
  {},
  {a: {b: 1}, [null]: 42, c:: 43},
  {a: {b: 1}, [null]: 42, c:: 43}.a["b"],
  {[x]:0 for x in ["a","b","c"]},
  {a: self.b, b: self.c, c: 10}.a,
  {a: 1} + {b: 2},
  {
    y: {z: 10} + {a:: 1, d:: 3} + {a::: self.d, b: super.a, c: super.z, d: 10}
  },
  std.primitiveEquals(true, true),
  std.primitiveEquals(false, true),
  std.primitiveEquals(false, false),
  std.primitiveEquals(true, false),
  std.primitiveEquals(null, null),
  std.primitiveEquals(1, null),
  std.primitiveEquals(1, 1),
  std.primitiveEquals(2, 1),
  std.primitiveEquals("a", "a"),
  std.primitiveEquals("b", "a"),
  std.length([1,2,3]),
  std.length("abc"),
  std.length({a: 0, b: 1}),
  std.makeArray(3, function(x) x+1),
  std.type(null),
  std.type(true),
  std.type(false),
  std.type(0),
  std.type(""),
  std.type({}),
  std.type(function(x) x),
  std.type([]),
  std.filter(function(x) x, [true, false]),
  std.objectHasEx({a: 1}, "a", false),
  std.objectHasEx({a:: 1}, "a", false),
  std.objectHasEx({a:: 1}, "a", true),
  std.objectHasEx({a: 1}, "b", false),
  std.objectHasEx({a:: 1}, "b", false),
  std.objectHasEx({a:: 1}, "b", true),
  std.objectFieldsEx({a: 1, b:: 2}, false),
  std.objectFieldsEx({a: 1, b:: 2}, true),
  assert true; 0,
  assert true : "foo"; 0,
  (import "success00_import.jsonnet")[0],
  (importbin "success00_import.jsonnet"),
  (importstr "success00_import.jsonnet"),
  {a: x, local x = 3, assert true},
  [x for x in [1, 2, 3]],
  { a: 1, b: 3, c: { d: 4, e: 5 }, f: { g: 6 } } { a+: 2, c+: { f: 6 }, f: { h: 7 } },
  ////  { z: "s2" } {
  ////    x: "s1",
  ////    a: "s7",
  ////    y: { a: "1" } {
  ////      [ self.x + ({ a: "a", b: { a: "s4" } { [ self.a ]+: "s5" } }).b.a ]+: "2",
  ////      a: "s6",
  ////      [ super.z + ({ a: "a" } { b: { a: "s4" } { [ super.a ]+: "s5" } }).b.a ]: "s3",
  ////    },
  ////  },
  {
    local f(x) = { bar: 1 } + x,
    foo: {
      bar: 0,
    } + f({
      x: super.bar,
    }),
  },
  { x: { a: 10 } } + { y: { x: { a: 2 } } + super.x },
  {
    local x = { baz: super.bar },
    foo1: { bar: 0 } + x,
    foo2: { bar: 1 } + x,
  },
]
