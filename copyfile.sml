fun copyfile(copythis, tothis) =
let
  (****************************************************************************
                            Functions
  ****************************************************************************)
  fun getcontents filename =
  let
    val ins  = TextIO.openIn filename

    fun helper(elem: char option) = 
      case elem of
           NONE       => (TextIO.closeIn ins; [])
         | SOME(elem) => elem::helper(TextIO.input1 ins)
  in
    helper(TextIO.input1 ins) 
  end

  fun putcontents(contents, filename) = 
  let
    val outs = TextIO.openOut filename

    fun helper([])    = TextIO.closeOut outs
      | helper(x::xs) = (TextIO.output1(outs, x);
                           helper(xs))
  in
    helper contents
  end

  val insertOpen  = explode "insert into advisers_tmp (rawdata) values ('"
  val insertClose = explode "');"

  fun parse rawlist = 
  let
    fun helper([], newRec, skip)    = []
     |  helper(x::xs, newRec, skip) =
	      case x of
		    #"<" => helper(xs, false, true)
	      | #";" => insertClose @ #"\n"::helper(xs, true, false)
		  | #"\n" => helper(xs, newRec, skip)
		  | #"\r" => helper(xs, newRec, skip)
		  | _    => 
		    if skip = true then helper(xs, false, true)
		    else 
			  if newRec = true then insertOpen @ x::helper(xs, false, false)
			  else x::helper(xs, false, false)
  in
    helper(rawlist, true, false)
  end


  (****************************************************************************
                            Values
  ****************************************************************************)
  val advisers = getcontents copythis
  val cleanedAdvisers = parse advisers
in
  putcontents(cleanedAdvisers, tothis)
end;

copyfile("Advisers.txt", "AdviserNames.txt");
