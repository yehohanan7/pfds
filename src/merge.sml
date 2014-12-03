fun merge(nil, M) = M
  | merge (L, nil) = L
  | merge (x::xt, y::yt) = if (x < y) then x::merge(xt, y::yt)
                           else y::merge(x::xt, yt);


merge([1,2,5], [4, 6]);
