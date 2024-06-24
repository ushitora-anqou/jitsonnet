[
  std.manifestYamlDoc({ x: { y: { z: x } } }, quote_keys=false)
  for x in std.range(0, 20000)
]
