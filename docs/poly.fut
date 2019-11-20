let polynomial (x: f32) (poly: []f32) (degree: i32): f32 =
  let (out, _) =
    loop (out, pow) = (0.0f32, 1.0f32)
    for i < (degree+1) do (out + pow * poly[i], pow * x)
  in out

let expansion (arr: []f32) (poly: []f32) (degree: i32): []f32 =
  map (\x -> (polynomial x poly degree)) arr

let main (n: i32) (degree: i32): f32 =
  let poly = replicate (degree+1) 1.0f32
  let arr = replicate n 1.0f32
  let out = expansion arr poly degree
  in out[0]
