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
		
	let repeat element count =
		seq {
			for n = 0 to count - 1 do
				yield element
		}
		
	let count source predicate = 
		let mutable n = 0
		for element in source do
			if predicate element then
				n <- n + 1
		n
	
	let concat first second =
		seq {
			for element in first do
				yield element
				
			for element in second do
				yield element
		}
		
	let selectMany source selector =
		seq {
			for element in source do
				for elm in selector source do
					yield elm
		}
		
	let any (source:IEnumerable<'a>) =
		let enumerator = source.GetEnumerator()
		enumerator.MoveNext()
		
	// any w/ predicate
	
	let all (source:IEnumerable<'a>) predicate =
		let enumerator = source.GetEnumerator()
		let mutable all1 = true
		while enumerator.MoveNext() do // iterates all anyway, but doesn't
			if not (predicate enumerator.Current) then
				all1 <- false
		all1
		
open ReLinq

where [1;2;3] (fun x -> x = 2) |> Dump
//where [|1;2;3|] (fun x -> x = 2) |> Dump
//where (seq { 1..3 }) (fun x -> x = 2) |> Dump

select [1;2;3] (fun x -> x * 2) |> Dump

range 0 5 |> Dump

empty |> Dump

repeat "a" 3 |> Dump

count [1;2] (fun x -> true) |> Dump

concat [4;5] [6] |> Dump

selectMany [1;2] (fun x -> x) |> Dump

any [] |> Dump

any [5;6] |> Dump

all [2;7] (fun x -> x > 0) |> Dump // all [2;7] <| fun x -> x > 0 |> Dump
all [2;7] (fun x -> x < 0) |> Dump
