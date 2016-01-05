open System

let w = [(8,4);(10,5);(25,6);(40,7);(74,8)]
let a = [(0,0);(13,1);(31,2);(53,3);(75,4);(102,5)]
let rw = [(0,0);(25,1);(50,2);(100,3)] 
let ra = [(0,0);(20,1);(40,2);(80,3)] 

[for i in w -> [for j in a -> [for k in rw -> [for l in ra -> 
                                                 let ap = i |> fst
                                                 let bp = j |> fst
                                                 let cp = k |> fst
                                                 let dp = l |> fst
                                                 let aw = i |> snd
                                                 let bw = j |> snd
                                                 let cw = k |> snd
                                                 let dw = l |> snd
                                                 (ap + bp + cp + dp),(aw+bw+cw+dw),(aw+cw),[aw;bw;cw;dw]]]]]
|> List.concat
|> List.concat
|> List.concat
|> List.filter (fun (_,v,_,_) -> v = 11)
|> List.filter (fun (_,_,v,_) -> 
                            let tmp = v-2
                            let div = 100/tmp
                            (100%tmp=0) || (tmp<103-(div*tmp)))
|> List.sortBy (fun (v,_,_,_) -> v)