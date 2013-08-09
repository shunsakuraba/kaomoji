let fetch_train max_depth operator =
  let train_string = Train.fetch_train max_depth operator in
  Train.parse_train_string train_string

let guess id challenge =
  Remoteguess.guess id challenge

