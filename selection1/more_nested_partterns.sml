
fun nondecreasing xs =
    case xs of
	[] => true
      | _::[]  => true
      | head::(neck::rest) => head <= neck andalso nondecreasing(neck :: rest)
