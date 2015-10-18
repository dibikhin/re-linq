module ReLinq =
	let where source predicate =
		seq {
			for element in source do
				if predicate element then
					yield element
		}
	
	let select source projection =
		seq {
			for element in source do
				yield projection element
		}
		
	let range start count =
		seq {
			for n = start to count - 1 do
				yield start + n
		}
		
//	let empty<'a> = Seq.toArray<'a> [] //?
	
	let empty<'b> =
		seq {
			for n = 0 to -1 do
				yield Unchecked.defaultof<'b>
		}
		
open ReLinq

where [1;2;3] (fun x -> x = 2) |> Dump
//where [|1;2;3|] (fun x -> x = 2) |> Dump
//where (seq { 1..3 }) (fun x -> x = 2) |> Dump

select [1;2;3] (fun x -> x * 2) |> Dump

range 0 5 |> Dump

empty |> Dump
