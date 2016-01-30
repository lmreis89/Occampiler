module EnvMap = Map.Make(String)

let assoc s v env = EnvMap.add s v env
let find s env = EnvMap.find s env
let new_env () = EnvMap.empty
let size_env env = EnvMap.cardinal env
let fold f env a = EnvMap.fold f env a